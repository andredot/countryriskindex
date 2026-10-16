test_that("normalised components and doctor density are floored", {
  set.seed(1)
  df <- data.frame(`All causes` = stats::runif(50, 1e4, 9e4),
                   check.names = FALSE)
  h <- add_hazard_score(df)$hazard_score
  expect_equal(min(h), scale_floor())
  expect_equal(max(h), 1)
  expect_equal(add_hazard_score(df, floor = 0)$hazard_score,
               (h - scale_floor()) / (1 - scale_floor()))

  comps <- data.frame(a = stats::runif(50, 1, 9), b = stats::runif(50, 1, 9),
                      c = stats::runif(50, 1, 9), d = stats::runif(50, 1, 9))
  v <- aggregate_vulnerability(comps)
  expect_gte(min(v), scale_floor())
  expect_equal(min(v), scale_floor())
  expect_equal(apply_scale_floor(c(0, 0.5, 1), 0.2), c(0.2, 0.6, 1))
})

crisis_fixture <- function() {
  body <- readLines(test_path("test-explain.R"))
  eval(parse(text = body[seq_len(grep("^test_that", body)[1] - 1)]))
  df <- make_scored(30)
  df$delta_hazard[1:10] <- 0
  df$hazard_score_adj[1:10] <- df$hazard_score[1:10]
  df$severity_uplift <- df$severity_adjusted_risk - df$overall_risk
  df
}

test_that("summarise_crisis_share() splits the uplift by channel", {
  df <- crisis_fixture()
  cs <- summarise_crisis_share(df)
  expect_true(all(cs$severity_uplift > 0))
  expect_equal(cs$severity_uplift, sort(cs$severity_uplift, decreasing = TRUE))
  expect_equal(cs$crisis_share, cs$severity_uplift / cs$severity_adjusted_risk)
  parts <- cs[, paste0("uplift_via_", risk_components())]
  expect_equal(unname(rowSums(parts)), cs$severity_uplift)
  first <- cs$ISO_A3 %in% df$ISO_A3[1:10]
  expect_true(all(cs$uplift_via_hazard[first] == 0))
  expect_s3_class(createimg_severity_uplift(df, top_n = 5), "ggplot")
})

test_that("ranked plots keep the top 10", {
  df <- crisis_fixture()
  p <- createimg_vul_bars(df)
  expect_equal(nrow(p$data), 10)
  expect_setequal(p$data$country,
                  df$country[order(-df$vulnerability_score)][1:10])
  expect_equal(nrow(createimg_risk_bars(df, top_n = 3)$data), 3)
  expect_equal(nrow(createimg_cap_bars(df, top_n = NULL)$data), 30)
  shift <- create_lollipop_plot_shift(df)
  expect_setequal(as.character(shift$data$country),
                  df$country[order(-df$severity_adjusted_risk)][1:10])
})

test_that("create_component_maps() draws three panels", {
  skip_if_not_installed("rnaturalearth")
  skip_if_not_installed("rnaturalearthdata")
  df <- data.frame(ISO_A3 = c("AGO", "ITA"), hazard_score = c(0.8, 0.2),
                   vulnerability_score = c(0.7, 0.1),
                   capacity_score = c(0.9, 0.15))
  p <- create_component_maps(df)
  expect_s3_class(p, "ggplot")
  expect_equal(levels(p$data$component),
               c("Hazard", "Vulnerability", "Lack of coping capacity"))
  ago <- p$data[p$data$adm0_a3 == "AGO", ]
  expect_equal(ago$value, c(0.8, 0.7, 0.9))
})
