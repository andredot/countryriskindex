test_that("normalise_quantiles honours frozen bounds", {
  x <- c(0, 5, 10)

  # Run-relative: bounds come from x itself
  expect_equal(normalise_quantiles(x)[2], 0.5, tolerance = 1e-6)

  # Frozen: identical inputs give identical outputs regardless of the rest
  a <- normalise_quantiles(c(5), q01 = 0, q99 = 20)
  b <- normalise_quantiles(c(5, 100, -100), q01 = 0, q99 = 20)[1]
  expect_equal(a, b)

  # Clipping outside the reference range
  expect_equal(normalise_quantiles(c(-5, 25), q01 = 0, q99 = 20), c(0, 1))
})

test_that("normalise_quantiles rejects degenerate bounds", {
  expect_error(normalise_quantiles(rep(3, 10)), "invalid reference bounds")
  expect_error(normalise_quantiles(1:10, q01 = 5, q99 = 5), "invalid")
})

test_that("reference bounds round-trip", {
  df <- data.frame(a = 1:100, b = seq(0, 10, length.out = 100))
  ref <- compute_reference_quantiles(df, c("a", "b"))

  expect_equal(nrow(ref), 2)
  expect_true(all(c("variable", "q01", "q99", "n_obs") %in% names(ref)))

  bounds <- get_reference_bounds(ref, "a")
  expect_type(bounds, "list")
  expect_true(bounds$q99 > bounds$q01)

  expect_null(get_reference_bounds(ref, "nonexistent"))
  expect_null(get_reference_bounds(NULL, "a"))
})

test_that("geometric_mean guards zeros and thin evidence", {
  # A single zero previously collapsed the aggregate to zero
  expect_gt(geometric_mean(c(0, 0.5, 0.5)), 0)

  # min_n forces NA rather than silently scoring on a partial variable set
  expect_true(is.na(geometric_mean(c(1, NA, NA), min_n = 3)))
  expect_equal(geometric_mean(c(1, 4, 16), min_n = 3), 4, tolerance = 1e-8)

  expect_true(is.na(geometric_mean(c(NA, NA))))
})

test_that("row_geometric_mean matches geometric_mean row-wise", {
  df <- data.frame(a = c(1, 4), b = c(4, 4), c = c(16, NA))
  res <- row_geometric_mean(df, c("a", "b", "c"), min_n = 1)
  expect_equal(res[1], 4, tolerance = 1e-8)
  expect_equal(res[2], 4, tolerance = 1e-8)

  res3 <- row_geometric_mean(df, c("a", "b", "c"), min_n = 3)
  expect_true(is.na(res3[2]))
})

test_that("data completeness flags thin evidence bases", {
  sets <- list(hazard = c("h1"), capacity = c("c1", "c2", "c3", "c4"))
  df <- data.frame(
    h1 = c(1, 1, NA),
    c1 = c(1, NA, NA),
    c2 = c(1, NA, NA),
    c3 = c(1, NA, NA),
    c4 = c(1, 1, NA)
  )

  out <- add_data_completeness(df, indicator_sets = sets, max_missing = 1)

  expect_equal(out$data_completeness[1], 1)
  expect_false(out$low_confidence[1])

  # 3 of 4 capacity indicators missing breaches the per-component rule
  expect_true(out$low_confidence[2])
  expect_true(out$low_confidence[3])
  expect_equal(out$n_missing_capacity[2], 3)
})
