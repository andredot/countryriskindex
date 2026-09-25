fake_paths <- function() {
  ids <- risk_input_files()$id
  stats::setNames(paste0("/data/", ids), ids)
}

small_scores <- function() {
  data.frame(
    ISO_A3 = c("AAA", "BBB", "CCC"),
    country = c("Aland", "Bland", "Cland"),
    extra_diagnostic = c(1, 2, 3),
    overall_risk = c(0.2, 0.5, 0.3),
    severity_adjusted_risk = c(0.25, 0.6, 0.9),
    stringsAsFactors = FALSE
  )
}

test_that("risk_input_files() is a consistent registry", {
  files <- risk_input_files()
  expect_false(anyDuplicated(files$id) > 0)
  expect_true(all(nzchar(files$file)))
  expect_setequal(files$group, c("GBD", "WHO", "INFORM", "Utilities"))
  expect_error(risk_input_path("nope"), "Unknown risk input")
})

test_that("compute_risk_score() wires each file to the pipeline step", {
  tag <- function(kind) function(path, ...) list(kind = kind, path = path, ...)
  seen <- new.env()
  local_mocked_bindings(
    import_data = tag("csv"),
    import_excel = tag("xlsx"),
    import_inform_excel = function(path, sheet) list(path = path, sheet = sheet),
    import_severity_excel = tag("severity"),
    import_cm_matrix = tag("cm"),
    preprocess_gbd_rates_by_cause = function(db, keys) c(db$path, keys$path),
    preprocess_haq_index = function(db, keys) c(db$path, keys$path),
    preprocess_who_data = function(a, b, keys) c(a$path, b$path, keys$path),
    preprocess_inform = function(risk, lcc) c(risk$sheet, lcc$sheet),
    preprocess_severity = function(x) x$path,
    merge_health_datasets = function(...) {
      seen$merge <- list(...)
      "merged"
    },
    score_risk = function(merged, cm, ref) {
      seen$score <- list(merged, cm$path, ref)
      small_scores()
    },
    .env = environment(compute_risk_score)
  )

  steps <- character(0)
  res <- compute_risk_score(fake_paths(), inform_risk_sheet = 3,
                            inform_lcc_sheet = 7, reference_quantiles = "ref",
                            progress = function(step, i, n) {
                              steps <<- c(steps, step)
                            })

  expect_equal(seen$merge, list(
    c("/data/gbd_daly", "/data/gbd_location_keys"),
    c("/data/haqi", "/data/gbd_location_keys"),
    c(3, 7),
    c("/data/who_uhc", "/data/who_doctors", "/data/who_location_keys"),
    "/data/inform_severity"
  ))
  expect_equal(seen$score, list("merged", "/data/crisis_modifier", "ref"))
  expect_equal(res$risk_score, small_scores())
  expect_equal(nrow(res$log), length(steps))
  expect_equal(res$log$step, steps)
  expect_equal(res$log$rows[nrow(res$log)], 3)
})

test_that("compute_risk_score() names the step that failed", {
  local_mocked_bindings(
    import_data = function(path) {
      if (grepl("haqi", path)) stop("column 'val' not found")
      data.frame()
    },
    .env = environment(compute_risk_score)
  )
  expect_error(compute_risk_score(fake_paths(), reference_quantiles = NULL),
               "Step 'Import HAQI' failed: column 'val' not found")
})

test_that("compute_risk_score() lists missing inputs", {
  p <- fake_paths()
  p[["who_uhc"]] <- NA
  expect_error(compute_risk_score(p[-1], reference_quantiles = NULL),
               "Missing input file\\(s\\): gbd_daly, who_uhc")
})

test_that("write_risk_score_xlsx() writes headline columns first, sorted", {
  skip_if_not_installed("writexl")
  skip_if_not_installed("readxl")
  f <- withr::local_tempfile(fileext = ".xlsx")
  ref <- data.frame(variable = "x", q01 = 0, q99 = 1, n_obs = 10L)
  write_risk_score_xlsx(small_scores(), f, reference_quantiles = ref,
                        log = data.frame(step = "Score risk", seconds = 0.1))

  expect_equal(readxl::excel_sheets(f),
               c("risk_score", "reference_quantiles", "log"))
  out <- readxl::read_excel(f, sheet = "risk_score")
  expect_equal(names(out)[1:4], c("ISO_A3", "country", "overall_risk",
                                   "severity_adjusted_risk"))
  expect_equal(names(out)[5], "extra_diagnostic")
  expect_equal(out$ISO_A3, c("CCC", "BBB", "AAA"))
})

test_that("the calculator app computes and downloads a workbook", {
  skip_if_not_installed("writexl")
  skip_if_not_installed("readxl")
  dir <- withr::local_tempdir()
  ids <- risk_input_files()$id
  defaults <- stats::setNames(file.path(dir, paste0(ids, ".csv")), ids)
  for (p in defaults) writeLines("x", p)
  defaults[["who_uhc"]] <- NA  # this one must be uploaded

  used <- NULL
  fake_compute <- function(paths, ...) {
    used <<- paths
    list(risk_score = small_scores(),
         log = data.frame(step = "Score risk", seconds = 0, rows = 3L))
  }
  app <- run_calculator_app(defaults = defaults, compute = fake_compute)
  upload <- file.path(dir, "uploaded.csv")
  writeLines("y", upload)

  shiny::testServer(app$serverFuncSource(), {
    session$setInputs(inform_risk_sheet = 2, inform_lcc_sheet = 5)
    session$setInputs(calculate = 1)
    expect_match(error(), "WHO 9A706FD")
    expect_null(result())

    session$setInputs(file_who_uhc = data.frame(name = "uhc.csv",
                                                datapath = upload))
    session$setInputs(calculate = 2)
    expect_null(error())
    expect_equal(unname(used[["who_uhc"]]), upload)
    expect_equal(unname(used[["gbd_daly"]]), unname(defaults[["gbd_daly"]]))
    expect_equal(result()$sources$file[risk_input_files()$id == "who_uhc"],
                 "uhc.csv")

    xlsx <- output$download
    expect_true(file.exists(xlsx))
    expect_equal(readxl::read_excel(xlsx, "risk_score")$ISO_A3,
                 c("CCC", "BBB", "AAA"))
    expect_true("sources" %in% readxl::excel_sheets(xlsx))
  })
})
