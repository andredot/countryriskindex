make_scored <- function(n = 30, seed = 1) {
  set.seed(seed)
  df <- data.frame(
    ISO_A3 = sprintf("C%02d", seq_len(n)),
    country = sprintf("Country %02d", seq_len(n)),
    vc_infrastructure    = runif(n, 1, 9),
    vc_education         = runif(n, 1, 9),
    vc_vulnerable_groups = runif(n, 1, 9),
    vc_socioeconomic     = runif(n, 1, 9),
    governance = runif(n, 10, 95), financing = runif(n, 10, 95),
    resources  = runif(n, 10, 95), services  = runif(n, 10, 95),
    hazard_score = runif(n, 0.05, 0.95),
    n_missing_hazard = 0L,
    n_missing_vulnerability = sample(0:3, n, TRUE),
    n_missing_capacity = 0L,
    low_confidence = FALSE,
    stringsAsFactors = FALSE
  )
  df$vulnerability_score <- aggregate_vulnerability(
    df[, vulnerability_component_cols()], min_n = 1)
  df$capacity_score <- aggregate_capacity(
    df[, capacity_component_cols()], min_n = 1)
  df$overall_risk <- row_geometric_mean(
    df, paste0(risk_components(), "_score"), min_n = 3)
  for (k in risk_components()) {
    d <- runif(n, 0, 0.8)
    df[[paste0("delta_", k)]] <- d
    df[[paste0("w_", k)]] <- 0.5
    df[[paste0("i_", k)]] <- 0.5
    df[[paste0(k, "_score_adj")]] <- logit_shift(df[[paste0(k, "_score")]], d)
  }
  df$severity_adjusted_risk <- row_geometric_mean(
    df, paste0(risk_components(), "_score_adj"), min_n = 3)
  df
}

test_that("component contributions sum exactly to the log gap", {
  df <- make_scored()
  d <- build_decomposition(df)

  ref_risk <- geometric_mean(d$reference[paste0(risk_components(), "_score")])
  got <- tapply(d$components$contribution, d$components$ISO_A3, sum)
  want <- log(df$overall_risk) - log(ref_risk)
  names(want) <- df$ISO_A3

  expect_equal(as.numeric(got[df$ISO_A3]), as.numeric(want), tolerance = 1e-10)
})

test_that("the two decomposition levels reconcile", {
  df <- make_scored()
  d <- build_decomposition(df)

  for (p in risk_components()) {
    ind <- d$indicators[d$indicators$component == p, ]
    cmp <- d$components[d$components$component == p, ]
    a <- tapply(ind$contribution, ind$ISO_A3, sum)
    b <- stats::setNames(cmp$contribution, cmp$ISO_A3)
    expect_equal(as.numeric(a[df$ISO_A3]), as.numeric(b[df$ISO_A3]),
                 tolerance = 1e-9,
                 info = paste("pillar:", p))
  }
})

test_that("the waterfall reproduces the published score", {
  df <- make_scored()
  d <- build_decomposition(df)
  iso <- df$ISO_A3[5]

  steps <- c(
    d$components$contribution[d$components$ISO_A3 == iso],
    d$severity$contribution[d$severity$ISO_A3 == iso]
  )
  reconstructed <- d$summary$reference_risk[1] * prod(exp(steps))

  expect_equal(reconstructed, df$severity_adjusted_risk[df$ISO_A3 == iso],
               tolerance = 1e-10)
})

test_that("severity contributions sum to the uplift and never flip sign", {
  df <- make_scored()
  d <- build_decomposition(df)

  got <- tapply(d$severity$contribution, d$severity$ISO_A3, sum)
  want <- log(df$severity_adjusted_risk) - log(df$overall_risk)
  names(want) <- df$ISO_A3

  expect_equal(as.numeric(got[df$ISO_A3]), as.numeric(want), tolerance = 1e-10)
  # Every delta here is positive, so no contribution may be negative
  expect_true(all(d$severity$contribution >= -1e-12))
})

test_that("shapley_contributions satisfies its axioms", {
  # Efficiency, on a deliberately nonlinear function
  f <- function(x) log(prod(x)^0.3 + x[["a"]]^2)
  xc <- c(a = 5, b = 3, c = 7); xr <- c(a = 2, b = 2, c = 2)
  expect_equal(sum(shapley_contributions(f, xc, xr)), f(xc) - f(xr),
               tolerance = 1e-10)

  # Symmetry
  g <- function(x) log(sum(x))
  phi <- shapley_contributions(g, c(a = 5, b = 5), c(a = 2, b = 2))
  expect_equal(phi[["a"]], phi[["b"]])

  # Null player
  h <- function(x) log(x[["a"]])
  phi2 <- shapley_contributions(h, c(a = 5, b = 5), c(a = 2, b = 9))
  expect_equal(phi2[["b"]], 0)

  # Refuses to blow up combinatorially
  big <- stats::setNames(as.numeric(1:13), letters[1:13])
  expect_error(shapley_contributions(sum, big, big * 0), "hierarchically")
})

test_that("logit_shift never moves a score against the sign of delta", {
  # A component clipped at 1 used to come back as 0.9995 under a positive shift
  expect_gte(logit_shift(1, 0.5), 1)
  expect_lte(logit_shift(0, -0.5), 0)
  expect_equal(logit_shift(1, 0), 1)
  expect_equal(logit_shift(0, 0), 0)
  expect_equal(logit_shift(0.4, 0), 0.4)

  p <- c(0, 0.001, 0.5, 0.999, 1)
  expect_true(all(logit_shift(p, 0.7) >= p))
  expect_true(all(logit_shift(p, -0.7) <= p))
})

test_that("the reference profile is internally coherent", {
  df <- make_scored()
  bounds <- resolve_pillar_bounds(df)
  ref <- build_reference_profile(df, bounds = bounds)

  # The pillar reference must be reachable from the sub-component reference,
  # otherwise the two decomposition levels cannot reconcile.
  expect_equal(
    unname(ref[["vulnerability_score"]]),
    unname(aggregate_vulnerability(
      as.data.frame(as.list(ref[vulnerability_component_cols()])),
      q01 = bounds$vulnerability$q01, q99 = bounds$vulnerability$q99,
      min_n = 1)),
    tolerance = 1e-12
  )
})

test_that("summary and confidence annotations are populated", {
  df <- make_scored()
  d <- build_decomposition(df)

  expect_equal(nrow(d$summary), nrow(df))
  expect_true(all(d$summary$dominant_driver %in% risk_components()))
  expect_true(all(d$summary$dominant_driver != d$summary$mitigating_factor))
  expect_true(all(is.finite(
    d$indicators$component_completeness[
      d$indicators$component == "vulnerability"])))
})

test_that("plots degrade gracefully for an unknown country", {
  df <- make_scored()
  d <- build_decomposition(df)

  expect_s3_class(create_decomposition_waterfall(d, "Nowhere"), "ggplot")
  expect_s3_class(create_contribution_bars(d, "Nowhere"), "ggplot")
  expect_s3_class(create_decomposition_waterfall(d, df$country[1]), "ggplot")
  expect_s3_class(
    create_decomposition_waterfall(d, df$country[1], level = "indicator"),
    "ggplot")
})
