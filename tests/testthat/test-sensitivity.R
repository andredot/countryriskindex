skewed_scores <- function(n = 120, seed = 3, floor = scale_floor()) {
  set.seed(seed)
  z <- stats::rnorm(n)
  d <- data.frame(ISO_A3 = sprintf("C%03d", seq_len(n)),
                  country = sprintf("Country %03d", seq_len(n)),
                  hazard_raw = z + stats::rnorm(n, 0, 0.5),
                  vulnerability_raw = z + stats::rnorm(n, 0, 0.5),
                  capacity_raw = z + stats::rnorm(n, 0, 0.5))
  d$hazard_score <- apply_scale_floor(normalise_quantiles(d$hazard_raw), floor)
  d$capacity_score <- apply_scale_floor(normalise_quantiles(d$capacity_raw),
                                        floor)
  # Vulnerability: same ordering information, but bounds wider than the data,
  # so no country sits at or near the lower bound.
  r <- d$vulnerability_raw
  d$vulnerability_score <- apply_scale_floor(
    normalise_quantiles(r, q01 = min(r) - 1, q99 = max(r)), floor)
  d$overall_risk <- row_geometric_mean(
    d, paste0(risk_components(), "_score"), min_n = 3)
  d
}

test_that("diagnose_component_scales() finds countries at the lower bound", {
  d <- skewed_scores()
  diag <- diagnose_component_scales(d)
  expect_equal(diag$component, risk_components())
  expect_equal(diag$at_lower_bound[diag$component == "vulnerability"], 0)
  expect_gt(diag$at_lower_bound[diag$component == "hazard"], 0)
  expect_equal(sum(diag$variance_share), 1)
  expect_true(all(diag$bounds == "this run"))
  # Without the floor, the near-zero countries dominate the log variance.
  unfloored <- diagnose_component_scales(skewed_scores(floor = 0), floor = 0)
  expect_gt(unfloored$log_var_from_near_lower[1],
            diag$log_var_from_near_lower[1])
  expect_lt(unfloored$variance_share[2], diag$variance_share[2])
})

test_that("compare_scoring_variants() contrasts the floor with v4.10", {
  cmp <- compare_scoring_variants(skewed_scores())
  share <- function(v, k) {
    cmp$weights$variance_share[cmp$weights$variant == v &
                                 cmp$weights$component == k]
  }
  expect_equal(unique(cmp$weights$variant),
               c("published", "no floor", "percentile", "arithmetic"))
  for (v in unique(cmp$weights$variant)) {
    expect_equal(sum(cmp$weights$variance_share[cmp$weights$variant == v]), 1)
  }
  # The published (floored) score reproduces the overall risk exactly...
  expect_equal(cmp$scores$published, skewed_scores()$overall_risk)
  expect_equal(cmp$summary$max_rank_change[1], 0)
  # ...and removing the floor shrinks the component with no near-zero values.
  expect_lt(share("no floor", "vulnerability"), share("published", "vulnerability"))
  expect_gt(share("percentile", "vulnerability"),
            share("no floor", "vulnerability"))
  expect_equal(nrow(cmp$scores), 120)
  expect_s3_class(create_variant_weights_plot(cmp), "ggplot")

  fs <- floor_sensitivity(skewed_scores(), floors = c(0, 0.1, 0.2))
  expect_equal(fs$floor, c(0, 0.1, 0.2))
  expect_equal(fs$published, c(FALSE, TRUE, FALSE))
  expect_equal(rowSums(fs[, risk_components()]), c(1, 1, 1))
  expect_equal(unname(unlist(fs[2, risk_components()])),
               share("published", risk_components()))
})

test_that("diagnose_pillar_internals() reports zeros and the INFORM benchmark", {
  body <- readLines(test_path("test-explain.R"))
  eval(parse(text = body[seq_len(grep("^test_that", body)[1] - 1)]))
  df <- make_scored(40)
  df$vc_education[1:3] <- 0
  df$inform_vulnerability <- 10 * df$vulnerability_score + stats::rnorm(40, 0, 0.1)
  out <- diagnose_pillar_internals(df)
  expect_equal(out$at_zero[out$subcomponent == "vc_education"], 3)
  bench <- out[out$subcomponent == "INFORM benchmark", ]
  expect_equal(bench$pillar, "vulnerability")
  expect_gt(bench$cor_with_pillar, 0.9)
})

test_that("createmap() loads no third-party tiles unless asked", {
  skip_if_not_installed("leaflet")
  skip_if_not_installed("rnaturalearth")
  skip_if_not_installed("rnaturalearthdata")
  d <- data.frame(ISO_A3 = c("AGO", "ITA"), country = c("Angola", "Italy"),
                  severity_adjusted_risk = c(0.6, 0.2))
  methods <- function(m) vapply(m$x$calls, `[[`, "", "method")

  m <- createmap_adjusted_risk_score(d, zoom_to_country = "Angola")
  expect_false(any(methods(m) %in% c("addTiles", "addProviderTiles")))
  expect_true("addPolygons" %in% methods(m))
  expect_false(is.null(m$x$fitBounds))

  m2 <- createmap(d, data_key = "ISO_A3",
                  value_column = "severity_adjusted_risk",
                  basemap = "CartoDB.PositronNoLabels")
  expect_true("addProviderTiles" %in% methods(m2))
})

test_that("preprocess_inform() keeps INFORM headline dimensions as benchmarks", {
  risk <- data.frame(
    COUNTRY = c("A", "B"), ISO3 = c("AAA", "BBB"),
    `Socio-Economic Vulnerability` = c("2.1", "6.3"),
    `Physical infrastructure` = c(1, 7), `Uprooted people` = c(0, 9),
    `Food Security` = c(1, 5), Governance = c(2, 8),
    VULNERABILITY = c("2.5", "7.1"), `LACK OF COPING CAPACITY` = c(3, 8),
    check.names = FALSE)
  lcc_cols <- c("Adult literacy rate", "Access to electricity",
                "Internet users", "Mobile cellular subscriptions",
                "Road density", "Drinking water", "Sanitation",
                "per capita public and private expenditure on health care")
  lcc <- data.frame(`...2` = c("meta", "meta", "AAA", "BBB"),
                    check.names = FALSE)
  for (cl in lcc_cols) lcc[[cl]] <- c(NA, NA, 1, 9)

  out <- preprocess_inform(risk, lcc)
  expect_equal(out$inform_vulnerability, c(2.5, 7.1))
  expect_equal(out$inform_lack_coping, c(3, 8))
  expect_equal(out$adult_literacy, c(1, 9))

  risk$VULNERABILITY <- NULL
  expect_true(all(is.na(preprocess_inform(risk, lcc)$inform_vulnerability)))
})
