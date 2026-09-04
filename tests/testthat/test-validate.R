test_that("PCA validation reports dimensionality", {
  set.seed(42)
  n <- 120
  latent <- rnorm(n)
  df <- data.frame(
    country = paste0("C", seq_len(n)),
    hazard_score       = latent + rnorm(n, sd = 0.3),
    infrastructure     = latent + rnorm(n, sd = 0.3),
    adult_literacy     = latent + rnorm(n, sd = 0.3),
    vulnerable_groups  = rnorm(n),
    soc_econ_vulnerability = latent + rnorm(n, sd = 0.3),
    governance = latent + rnorm(n, sd = 0.3),
    financing  = latent + rnorm(n, sd = 0.3),
    resources  = rnorm(n),
    services   = latent + rnorm(n, sd = 0.3)
  )

  res <- run_pca_validation(df)

  expect_equal(res$n_complete, n)
  expect_equal(nrow(res$variance), length(res$vars))
  expect_equal(sum(res$variance$prop_variance), 1, tolerance = 1e-8)
  expect_true(all(diff(res$variance$eigenvalue) <= 1e-8))

  dims <- summarise_dimensionality(res)
  expect_true(all(c("criterion", "n_components") %in% names(dims)))
  # One dominant latent factor should be detected
  expect_gt(dims$pc1_share[1], 0.4)
})

test_that("PCA validation rejects degenerate input", {
  df <- data.frame(a = 1:5)
  expect_error(run_pca_validation(df, vars = c("a")), "at least two")
})
