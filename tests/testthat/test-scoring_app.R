scored_fixture <- function(seed = 1) {
  body <- readLines(test_path("test-explain.R"))
  eval(parse(text = body[seq_len(grep("^test_that", body)[1] - 1)]))
  make_scored(30, seed = seed)
}

upload <- function(dir, id) {
  p <- file.path(dir, paste0(id, ".csv"))
  writeLines("x", p)
  data.frame(name = basename(p), datapath = p)
}

test_that("the scoring app recomputes on the fly once every file is in", {
  skip_if_not_installed("writexl")
  skip_if_not_installed("readxl")
  dir <- withr::local_tempdir()
  ids <- risk_input_files()$id
  scored <- scored_fixture()
  calls <- 0
  fake_compute <- function(paths, ...) {
    calls <<- calls + 1
    if (grepl("bad", paths[["haqi"]])) stop("Step 'Import HAQI' failed: oops")
    list(risk_score = scored,
         log = data.frame(step = "Score risk", seconds = 0, rows = 30L))
  }
  app <- run_scoring_app(baseline = scored_fixture(seed = 2),
                         compute = fake_compute, debounce_ms = 100)

  shiny::testServer(app$serverFuncSource(), {
    # Every file but one: nothing is computed yet.
    for (id in ids[-1]) do.call(session$setInputs,
                                stats::setNames(list(upload(dir, id)),
                                                paste0("file_", id)))
    session$elapse(200)
    expect_equal(calls, 0)
    expect_null(result())

    # The last file triggers the computation without any button.
    session$setInputs(file_gbd_daly = upload(dir, "gbd_daly"))
    session$elapse(200)
    expect_equal(calls, 1)
    res <- result()
    expect_equal(nrow(res$risk_score), 30)
    expect_equal(sum(res$weights$component$variance_share), 1)
    expect_false(is.null(res$decomposition))
    expect_setequal(res$comparison$status, "changed")

    # A wrong file surfaces the failing step; replacing it recovers.
    session$setInputs(file_haqi = upload(dir, "bad_haqi"))
    session$elapse(200)
    expect_match(error(), "Import HAQI")
    expect_null(result())
    session$setInputs(file_haqi = upload(dir, "haqi"))
    session$elapse(200)
    expect_null(error())
    expect_equal(calls, 3)

    session$setInputs(country = scored$country[1],
                      decomp_level = "indicator", n_rows = "25")
    expect_true(is.list(output$waterfall))
    expect_true(is.list(output$weights_component))

    xlsx <- output$download
    sheets <- readxl::excel_sheets(xlsx)
    expect_true(all(c("risk_score", "sources", "weights_component",
                      "weights_indicator", "decomposition_summary",
                      "change_vs_baseline") %in% sheets))
  })
})

test_that("the scoring app uses defaults only when given", {
  dir <- withr::local_tempdir()
  ids <- risk_input_files()$id
  defaults <- stats::setNames(file.path(dir, paste0(ids, ".csv")), ids)
  for (p in defaults) writeLines("x", p)
  calls <- 0
  app <- run_scoring_app(
    defaults = defaults, debounce_ms = 50,
    compute = function(paths, ...) {
      calls <<- calls + 1
      list(risk_score = scored_fixture(), log = data.frame())
    })
  shiny::testServer(app$serverFuncSource(), {
    session$setInputs(inform_risk_sheet = 2)
    session$elapse(100)
    expect_equal(calls, 1)
  })

  app <- run_scoring_app(debounce_ms = 50, compute = function(...) stop())
  shiny::testServer(app$serverFuncSource(), {
    session$setInputs(inform_risk_sheet = 2)
    session$elapse(100)
    expect_null(result())
    expect_null(error())
  })
})
