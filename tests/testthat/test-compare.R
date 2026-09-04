test_that("compare_runs handles a missing baseline gracefully", {
  current <- data.frame(ISO_A3 = "ITA", country = "Italy",
                        severity_adjusted_risk = 0.3)
  expect_null(compare_runs(current, NULL))

  # The report must still build on the very first run
  p <- create_run_comparison_plot(NULL)
  expect_s3_class(p, "ggplot")
})

test_that("compare_runs labels new, dropped and changed countries", {
  current <- data.frame(
    ISO_A3 = c("ITA", "FRA"), country = c("Italy", "France"),
    severity_adjusted_risk = c(0.30, 0.40), stringsAsFactors = FALSE
  )
  previous <- data.frame(
    ISO_A3 = c("ITA", "ESP"),
    severity_adjusted_risk = c(0.25, 0.50), stringsAsFactors = FALSE
  )

  cmp <- compare_runs(current, previous)

  expect_equal(cmp$status[cmp$ISO_A3 == "ITA"], "changed")
  expect_equal(cmp$difference[cmp$ISO_A3 == "ITA"], 0.05, tolerance = 1e-8)
  expect_equal(cmp$status[cmp$ISO_A3 == "FRA"], "new")
  expect_equal(cmp$status[cmp$ISO_A3 == "ESP"], "dropped")
})

test_that("snapshot round-trips through disk", {
  dir <- withr::local_tempdir()
  df <- data.frame(
    ISO_A3 = c("ITA", "FRA"), country = c("Italy", "France"),
    overall_risk = c(0.3, 0.4), severity_adjusted_risk = c(0.31, 0.44),
    stringsAsFactors = FALSE
  )

  expect_null(load_previous_snapshot(dir))

  path <- save_run_snapshot(df, dir = dir)
  expect_true(file.exists(path))

  back <- load_previous_snapshot(dir)
  expect_equal(nrow(back), 2)
  expect_equal(back$severity_adjusted_risk, c(0.31, 0.44))

  # The file just written is excluded, so a fresh run finds no baseline
  expect_null(load_previous_snapshot(dir, exclude = path))
})
