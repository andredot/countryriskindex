test_that("self_contained() keeps global helpers across serialisation", {
  # Mimics tar_source(): the helper lives in the global environment, which R
  # serialises by reference, so a plain closure loses it on tar_read().
  sc_helper <- function(x) x + 1
  environment(sc_helper) <- globalenv()
  assign("sc_helper", sc_helper, envir = globalenv())
  on.exit(if (exists("sc_helper", envir = globalenv(), inherits = FALSE))
    rm("sc_helper", envir = globalenv()), add = TRUE)
  make <- function(d) function() sc_helper(d)
  environment(make) <- globalenv()

  plain <- make(1)
  bundled <- self_contained(make(1))
  plain_bytes <- serialize(plain, NULL)
  bundled_bytes <- serialize(bundled, NULL)
  rm("sc_helper", envir = globalenv())  # a fresh session reading the target

  expect_error(unserialize(plain_bytes)(), "could not find function")
  expect_equal(unserialize(bundled_bytes)(), 2)
})

test_that("self_contained() leaves namespaced closures alone", {
  fun <- stats::median
  expect_identical(self_contained(fun), fun)
})
