test_that("gbd_cause_columns excludes non-cause columns", {
  df <- data.frame(
    location_name = c("Italy", "France"),
    ISO_A3 = c("ITA", "FRA"),
    `All causes` = c(20000, 18000),
    `Enteric infections` = c(300, 120),
    `Cardiovascular diseases` = c(4000, 3500),
    check.names = FALSE
  )
  cols <- gbd_cause_columns(df)

  expect_setequal(cols, c("Enteric infections", "Cardiovascular diseases"))
  expect_false("All causes" %in% cols)
  expect_false("ISO_A3" %in% cols)
})

test_that("missing 'All causes' is a hard error, not a silent NA", {
  db <- data.frame(location_id = 1, location_name = "Italy",
                   cause_id = 957, cause_name = "Enteric infections", val = 300)
  keys <- data.frame(`Location ID` = 1, ISO_A3 = "ITA", check.names = FALSE)

  expect_error(preprocess_gbd_rates_by_cause(db, keys), "All causes")

  # ...but can be opted out of
  out <- preprocess_gbd_rates_by_cause(db, keys, require_all_causes = FALSE)
  expect_true("Enteric infections" %in% names(out))
})

test_that("GBD pivot keeps whatever causes the extract contains", {
  db <- data.frame(
    location_id = rep(1:2, each = 3),
    location_name = rep(c("Italy", "France"), each = 3),
    cause_id = rep(c(294, 957, 491), 2),
    cause_name = rep(c("All causes", "Enteric infections",
                       "Cardiovascular diseases"), 2),
    val = c(20000, 300, 4000, 18000, 120, 3500)
  )
  keys <- data.frame(`Location ID` = 1:2, ISO_A3 = c("ITA", "FRA"),
                     check.names = FALSE)

  out <- preprocess_gbd_rates_by_cause(db, keys)

  expect_equal(nrow(out), 2)
  expect_true(all(c("All causes", "Enteric infections",
                    "Cardiovascular diseases") %in% names(out)))
  expect_equal(out$`All causes`[out$ISO_A3 == "ITA"], 20000)
})

test_that("radar cause groups tolerate absent causes", {
  df <- data.frame(
    country = c("Italy", "France"),
    `Enteric infections` = c(300, 120),
    `Transport injuries` = c(200, 150),
    improved_water = c(2, 1),
    improved_sanitation = c(3, 1),
    capacity_score = c(0.3, 0.2),
    vulnerability_score = c(0.4, 0.3),
    check.names = FALSE
  )

  # Most causes are missing: warn, don't fail
  expect_warning(out <- extract_radar_data(df), "not found")
  expect_equal(nrow(out), 2)
  expect_true(all(c("Country", "Communicable diseases",
                    "Traumatic/Violent injuries", "WASH",
                    "Capacity", "Vulnerability") %in% names(out)))
  # Normalised to a 0-1 maximum
  expect_equal(max(out$Capacity, na.rm = TRUE), 1)
})

test_that("gbd_year is a single source of truth", {
  expect_type(gbd_year(), "character")
  expect_match(gbd_year(), "^[0-9]{4}$")
})

test_that("pick_column disambiguates raw vs normalised by range", {
  # The real INFORM Lack of Coping Capacity layout: road density appears twice,
  # once raw (1.46-844) and once as the INFORM 0-10 score.
  lcc <- data.frame(
    `Road density...15` = c(1.455, 843.8),
    `Road density...16` = c(0, 10),
    check.names = FALSE
  )

  expect_equal(
    suppressMessages(pick_column(lcc, "Road density", range = c(0, 10))),
    c(0, 10)
  )

  # Selection must not depend on column order
  swapped <- data.frame(
    `Road density...15` = c(0, 10),
    `Road density...16` = c(1.455, 843.8),
    check.names = FALSE
  )
  expect_equal(
    suppressMessages(pick_column(swapped, "Road density", range = c(0, 10))),
    c(0, 10)
  )
})

test_that("pick_column fails loudly rather than guessing", {
  only_raw <- data.frame(`Road density...15` = c(1.455, 843.8),
                         check.names = FALSE)
  expect_error(pick_column(only_raw, "Road density", range = c(0, 10)),
               "843.8")

  ambiguous <- data.frame(`X...1` = c(1, 2), `X...2` = c(3, 4),
                          check.names = FALSE)
  expect_error(pick_column(ambiguous, "X", range = c(0, 10)), "ambiguous")

  # An exact name beats a suffixed one when both satisfy the range
  both <- data.frame(X = c(1, 2), `X...2` = c(3, 4), check.names = FALSE)
  expect_equal(suppressMessages(pick_column(both, "X", range = c(0, 10))),
               c(1, 2))

  expect_error(pick_column(both, "Nope"), "no column matching")
  expect_true(all(is.na(pick_column(both, "Nope", required = FALSE))))
})
