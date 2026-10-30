test_that("build_scoring_bundle() writes a self-contained app folder", {
  withr::local_dir(test_path("..", ".."))  # project root, as in dev/06
  tmp <- withr::local_tempdir()
  files <- risk_input_files()
  defaults <- stats::setNames(rep(NA_character_, nrow(files)), files$id)
  for (id in files$id[files$update != "upload" & files$location == "shared"]) {
    defaults[[id]] <- file.path(tmp, paste0(id, ".csv"))
    writeLines("a,b", defaults[[id]])
  }
  baseline <- data.frame(ISO_A3 = "AAA", country = "A", overall_risk = 0.3,
                         severity_adjusted_risk = 0.4, extra = 1)
  dir <- file.path(tmp, "bundle")
  build_scoring_bundle(dir, defaults = defaults, baseline = baseline)

  expect_true(file.exists(file.path(dir, "app.R")))
  expect_setequal(list.files(file.path(dir, "R")), scoring_app_files())
  expect_true(file.exists(
    file.path(dir, "inst", "extdata", "crisis_modifier_matrix.xlsx")))

  # Every automatic input resolves inside the bundle; uploads are left empty.
  bundled <- readRDS(file.path(dir, "data", "defaults.rds"))
  upload <- files$id[files$update == "upload"]
  expect_true(all(is.na(bundled[upload])))
  auto <- bundled[setdiff(names(bundled), upload)]
  expect_false(anyNA(auto))
  expect_true(all(file.exists(file.path(dir, auto))))

  expect_named(readRDS(file.path(dir, "data", "baseline.rds")),
               c("ISO_A3", "country", "overall_risk",
                 "severity_adjusted_risk"))
  expect_silent(parse(file.path(dir, "app.R")))

  # The bundled files alone define the app and the scoring chain.
  e <- new.env()
  for (f in list.files(file.path(dir, "R"), full.names = TRUE)) {
    sys.source(f, envir = e)
  }
  for (fn in c("run_scoring_app", "compute_risk_score", "score_risk",
               "build_decomposition", "summarise_global_weights",
               "self_contained", "%||%")) {
    expect_true(exists(fn, envir = e, inherits = FALSE), info = fn)
  }

  expect_warning(build_scoring_bundle(file.path(tmp, "b2"), defaults = NULL),
                 "Not bundled")
})
