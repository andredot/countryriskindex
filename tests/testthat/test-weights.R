test_that("global weights match OLS standardised betas and sum to one", {
  body <- readLines(test_path("test-explain.R"))
  eval(parse(text = body[seq_len(grep("^test_that", body)[1] - 1)]))
  df <- make_scored(60)
  w <- summarise_global_weights(build_decomposition(df))

  expect_equal(sum(w$component$variance_share), 1)
  expect_equal(sum(w$indicator$variance_share), 1)
  expect_equal(w$component$nominal_weight[1:3], rep(1 / 3, 3))
  expect_equal(w$component$term, c(risk_components(), "crisis"))

  # The published log score is an exact linear function of the log pillars
  # and the crisis term, so OLS recovers the same standardised betas.
  L <- function(x) log(pmax(x, 1e-4))
  X <- data.frame(h = L(df$hazard_score), v = L(df$vulnerability_score),
                  c = L(df$capacity_score))
  X$crisis <- (L(df$hazard_score_adj) + L(df$vulnerability_score_adj) +
                 L(df$capacity_score_adj) - rowSums(X)) / 3
  y <- rowSums(X[, 1:3]) / 3 + X$crisis
  fit <- stats::lm(y ~ h + v + c + crisis, data = X)
  expect_equal(unname(coef(fit)[-1]), c(1, 1, 1, 3) / 3, tolerance = 1e-8)
  ols_beta <- unname(coef(fit)[-1] * vapply(X, stats::sd, 0) / stats::sd(y))
  expect_equal(w$component$std_beta, ols_beta, tolerance = 1e-8)
})

test_that("structural-only weights drop the crisis modifier", {
  body <- readLines(test_path("test-explain.R"))
  eval(parse(text = body[seq_len(grep("^test_that", body)[1] - 1)]))
  w <- summarise_global_weights(build_decomposition(make_scored(40)),
                                include_severity = FALSE)
  expect_equal(w$target, "overall_risk")
  expect_false(any(w$indicator$component == "crisis modifier"))
  expect_equal(sum(w$component$variance_share), 1)
  expect_s3_class(create_global_weights_plot(w, "indicator"), "ggplot")
})
