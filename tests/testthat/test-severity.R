test_that("logit_shift is bounded, monotone and baseline-consistent", {
  # Never reaches 1, unlike the v4.0 convex pull
  expect_lt(logit_shift(0.35, 10), 1)
  expect_gt(logit_shift(0.35, 10), 0.99)

  # Zero shift is the identity
  expect_equal(logit_shift(0.42, 0), 0.42, tolerance = 1e-6)

  # Monotone increasing in delta
  d <- logit_shift(0.35, c(0, 0.5, 1, 2))
  expect_true(all(diff(d) > 0))

  # Constant odds ratio regardless of baseline
  or <- function(p, delta) {
    a <- logit_shift(p, delta)
    (a / (1 - a)) / (p / (1 - p))
  }
  expect_equal(or(0.2, 0.7), or(0.8, 0.7), tolerance = 1e-6)
})

test_that("severity intensity removes the v4.0 'entering the list' cliff", {
  df <- data.frame(
    conditions_impact = c(NA, 1, 3, 5),
    `Society and safety` = c(NA, 1, 3, 5),
    operating_environment = c(NA, 1, 3, 5),
    check.names = FALSE
  )
  i <- severity_intensities(df)

  # Absent crisis and severity 1 both give zero intensity: no discontinuity
  expect_equal(i$i_hazard[1], 0)
  expect_equal(i$i_hazard[2], 0)
  expect_equal(i$i_hazard[3], 0.5)
  expect_equal(i$i_hazard[4], 1)
})

test_that("channel weights saturate and respect the floor", {
  df <- data.frame(
    severity_index = c(NA, 3, 3, 3),
    `Driver Population movement` = c(0, 1, 1, 1),
    `Driver Conflict` = c(0, 0, 1, 1),
    `Driver Drought` = c(0, 0, 0, 1),
    check.names = FALSE
  )
  cm <- data.frame(
    Driver = c("Population movement", "Conflict", "Drought"),
    Vulnerability = c(TRUE, TRUE, TRUE),
    Capacity = c(FALSE, TRUE, FALSE),
    `Communicable diseases` = c(TRUE, FALSE, TRUE),
    check.names = FALSE
  )

  w <- severity_channel_weights(df, cm, kappa = 2, w_floor = 0.25)

  # No crisis record -> no weight at all
  expect_equal(w$w_vulnerability[1], 0)
  expect_equal(w$w_capacity[1], 0)

  # Saturating: 1 driver -> 1/3, 2 -> 1/2, 3 -> 3/5
  expect_equal(w$w_vulnerability[2], 1 / 3, tolerance = 1e-8)
  expect_equal(w$w_vulnerability[3], 1 / 2, tolerance = 1e-8)
  expect_equal(w$w_vulnerability[4], 3 / 5, tolerance = 1e-8)
  expect_true(all(w$w_vulnerability < 1))

  # Floor applies where a crisis exists but no driver hits that channel
  expect_equal(w$w_capacity[2], 0.25)
})

test_that("severity no longer dominates the structural model", {
  base <- 0.35
  betas <- severity_betas()

  cal <- severity_calibration_table(
    baseline = base, severity = 5, n_drivers = 6, betas = betas
  )

  # v4.0 pinned every country to exactly 1.0 at severity 5
  expect_equal(cal$risk_v4_0, 1, tolerance = 1e-8)

  # v4.1 is bounded well below 1 and moves roughly one band
  expect_lt(cal$risk_v4_1, 0.60)
  expect_gt(cal$risk_v4_1, 0.45)

  # A minor single-driver crisis must not move a country a whole band
  minor <- severity_calibration_table(
    baseline = base, severity = 2, n_drivers = 1, betas = betas
  )
  expect_lt(minor$uplift_v4_1, 0.05)
  expect_gt(minor$uplift_v4_0, 0.15)   # the behaviour being fixed
})

test_that("severity adjustment is monotone in severity and drivers", {
  cal <- severity_calibration_table(baseline = 0.35, n_drivers = 2)
  cal <- cal[order(cal$severity), ]
  expect_true(all(diff(cal$risk_v4_1) >= 0))

  cal2 <- severity_calibration_table(baseline = 0.35, severity = 4)
  cal2 <- cal2[order(cal2$n_drivers), ]
  expect_true(all(diff(cal2$risk_v4_1) >= 0))
})

test_that("update_score honours `times` exactly", {
  # v4.0 decremented `times`, so 1 and 2 gave identical results
  expect_lt(update_score(0.5, 3, times = 1), update_score(0.5, 3, times = 2))
  expect_lt(update_score(0.5, 3, times = 2), update_score(0.5, 3, times = 3))

  # Capped at max_times
  expect_equal(update_score(0.5, 3, times = 9),
               update_score(0.5, 3, times = 3))

  # NA multiplier leaves the score untouched; NA base propagates
  expect_equal(update_score(0.5, NA, times = 2), 0.5)
  expect_true(is.na(update_score(NA, 3, times = 2)))

  # times = 0 is the identity
  expect_equal(update_score(0.5, 5, times = 0), 0.5)
})

test_that("driver names match across spellings and comma-space lists", {
  expect_equal(driver_key(c("Land slide", "Landslide", "Food Security",
                            "food-security")),
               c("landslide", "landslide", "foodsecurity", "foodsecurity"))

  sev <- data.frame(COUNTRY = c("meta", "A", "B"), ISO3 = c("M", "AAA", "BBB"),
                    CRISIS = "x", DRIVERS = c("x", "Conflict, Floods", "Landslide"),
                    `INFORM Severity Index` = c(NA, 4, 3),
                    `Operating environment` = 3, `Complexity of the crisis` = 3,
                    `Conditions of people affected` = 3,
                    `Impact of the crisis` = 3, check.names = FALSE)
  p <- preprocess_severity(sev)
  expect_setequal(grep("^Driver", names(p), value = TRUE),
                  c("Driver Conflict", "Driver Floods", "Driver Landslide"))

  cm <- data.frame(Driver = c("Conflict", "Floods", "Land slide"),
                   `Traumatic/Violent injuries` = c(TRUE, TRUE, TRUE),
                   Vulnerability = c(TRUE, FALSE, FALSE),
                   Capacity = c("x", "", "x"), check.names = FALSE)
  w <- severity_channel_weights(p, cm)
  expect_equal(w$n_drivers_hazard[p$ISO3 == "AAA"], 2)
  expect_equal(w$n_drivers_hazard[p$ISO3 == "BBB"], 1)
  expect_equal(w$n_drivers_vulnerability[p$ISO3 == "AAA"], 1)
  expect_equal(w$n_drivers_capacity[p$ISO3 == "AAA"], 1)
})

test_that("label columns are flagged and applied to every driver", {
  expect_equal(crisis_modifier_flags(c("TRUE", "false", NA, "x", "1")),
               c(TRUE, FALSE, FALSE, TRUE, TRUE))
  expect_equal(crisis_modifier_flags(c(0, 2, NA)), c(FALSE, TRUE, FALSE))
  expect_warning(f <- crisis_modifier_flags(c("CotC-OE", "CotC-OE"), "Capacity"),
                 "holds labels")
  expect_equal(f, c(TRUE, TRUE))
})

test_that("audit_crisis_modifier_matrix() reports gaps", {
  cm <- data.frame(Driver = c("Conflict", "Drought"),
                   `Communicable diseases` = c(FALSE, TRUE),
                   Vulnerability = "CotC-SaS", Capacity = c(TRUE, FALSE),
                   check.names = FALSE)
  d <- data.frame(ISO3 = c("A", "B", "C"), severity_index = c(3, 4, NA),
                  `Driver Conflict` = c(1, 0, 0),
                  `Driver Economic` = c(0, 1, 0),
                  driver_matched = c(TRUE, FALSE, FALSE), check.names = FALSE)
  a <- audit_crisis_modifier_matrix(cm, d)
  st <- stats::setNames(a$channels$status, a$channels$channel)
  expect_equal(unname(st["Vulnerability"]), "labels")
  expect_equal(unname(st["Capacity"]), "flags")
  expect_equal(unname(st["WASH"]), "missing")
  expect_equal(a$unmatched$driver, "Economic")
  expect_equal(a$drivers$countries_now, c(1, 0))
  expect_equal(a$summary$countries_on_floor_weight, 1)
  expect_equal(a$summary$matrix_drivers_unused_now, 1)
})
