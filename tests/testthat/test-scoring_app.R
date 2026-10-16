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

local_inputs <- function(dir) {
  ids <- risk_input_files()$id
  p <- stats::setNames(file.path(dir, paste0("local_", ids, ".csv")), ids)
  for (f in p) writeLines("x", f)
  p
}

fake_who <- function(dir, origin = "downloaded") {
  function(dest_dir, fallback) {
    ids <- c("who_uhc", "who_doctors")
    p <- file.path(dir, paste0("dl_", ids, ".csv"))
    for (f in p) writeLines("x", f)
    data.frame(id = ids, path = p, origin = origin, detail = "url",
               stringsAsFactors = FALSE)
  }
}

test_that("only GBD and INFORM are uploaded; the rest is automatic", {
  skip_if_not_installed("writexl")
  skip_if_not_installed("readxl")
  dir <- withr::local_tempdir()
  scored <- scored_fixture()
  used <- NULL
  calls <- 0
  fake_compute <- function(paths, ...) {
    calls <<- calls + 1
    used <<- paths
    if (grepl("bad", paths[["inform_risk"]])) {
      stop("Step 'Import INFORM Risk' failed: oops")
    }
    list(risk_score = scored,
         log = data.frame(step = "Score risk", seconds = 0, rows = 30L))
  }
  app <- run_scoring_app(baseline = scored_fixture(seed = 2),
                         defaults = local_inputs(dir),
                         who_downloader = fake_who(dir),
                         compute = fake_compute, debounce_ms = 100)

  shiny::testServer(app$serverFuncSource(), {
    session$elapse(50)
    expect_equal(who()$origin, c("downloaded", "downloaded"))
    expect_length(fallback_ids(), 0)

    session$setInputs(file_gbd_daly = upload(dir, "gbd_daly"),
                      file_inform_risk = upload(dir, "inform_risk"))
    session$elapse(200)
    expect_equal(calls, 0)

    session$setInputs(file_inform_severity = upload(dir, "inform_severity"))
    session$elapse(200)
    expect_equal(calls, 1)
    expect_match(used[["who_uhc"]], "dl_who_uhc")
    expect_match(used[["crisis_modifier"]], "local_crisis_modifier")
    expect_match(used[["gbd_location_keys"]], "local_gbd_location_keys")
    expect_match(used[["gbd_daly"]], "gbd_daly.csv")
    src <- result()$sources
    expect_equal(src$origin[match("crisis_modifier", risk_input_files()$id)],
                 "static (shared folder)")
    expect_equal(src$origin[match("who_uhc", risk_input_files()$id)],
                 "downloaded")
    expect_equal(src$origin[match("gbd_daly", risk_input_files()$id)],
                 "uploaded")
    expect_equal(sum(result()$weights$component$variance_share), 1)

    # A wrong file surfaces the failing step; replacing it recovers.
    session$setInputs(file_inform_risk = upload(dir, "bad_inform_risk"))
    session$elapse(200)
    expect_match(error(), "Import INFORM Risk")
    session$setInputs(file_inform_risk = upload(dir, "inform_risk"))
    session$elapse(200)
    expect_null(error())

    session$setInputs(country = scored$country[1],
                      decomp_level = "indicator", n_rows = "25")
    expect_true(is.list(output$waterfall))
    sheets <- readxl::excel_sheets(output$download)
    expect_true(all(c("risk_score", "sources", "weights_component",
                      "change_vs_baseline") %in% sheets))
  })
})

test_that("unavailable automatic inputs ask for an upload", {
  dir <- withr::local_tempdir()
  local <- local_inputs(dir)
  local[c("crisis_modifier", "who_uhc", "who_doctors")] <- NA
  calls <- 0
  app <- run_scoring_app(
    defaults = local, debounce_ms = 50,
    who_downloader = function(...) stop("offline"),
    compute = function(paths, ...) {
      calls <<- calls + 1
      list(risk_score = scored_fixture(), log = data.frame())
    })
  shiny::testServer(app$serverFuncSource(), {
    session$elapse(50)
    expect_equal(who()$origin, c("unavailable", "unavailable"))
    expect_setequal(fallback_ids(),
                    c("crisis_modifier", "who_uhc", "who_doctors"))
    for (id in c("gbd_daly", "inform_risk", "inform_severity")) {
      do.call(session$setInputs,
              stats::setNames(list(upload(dir, id)), paste0("file_", id)))
    }
    session$elapse(100)
    expect_equal(calls, 0)
    session$setInputs(file_crisis_modifier = upload(dir, "cm"),
                      file_who_uhc = upload(dir, "uhc"),
                      file_who_doctors = upload(dir, "doc"))
    session$elapse(100)
    expect_equal(calls, 1)
  })
})

test_that("download_who_indicators() validates files and falls back", {
  dir <- withr::local_tempdir()
  src <- file.path(dir, "src")
  dir.create(src)
  good <- data.frame(GEO_NAME_SHORT = "Italy", DIM_TIME = 2022,
                     DIM_GEO_CODE_TYPE = "COUNTRY", IND_NAME = "x",
                     INDEX_N = 80)
  utils::write.csv(good, file.path(src, "9A706FD_ALL_LATEST.csv"),
                   row.names = FALSE)
  writeLines("not,the,right,layout", file.path(src, "217795A_ALL_LATEST.csv"))
  withr::local_options(countryriskindex.who_url_template =
                         paste0("file://", src, "/%s_ALL_LATEST.csv"))
  local <- c(who_doctors = file.path(dir, "local_doctors.csv"))
  writeLines("x", local)

  res <- download_who_indicators(file.path(dir, "dl"), fallback = local)
  expect_equal(res$id, c("who_uhc", "who_doctors"))
  expect_equal(res$origin, c("downloaded", "local copy"))
  expect_true(file.exists(res$path[1]))
  expect_equal(res$path[2], unname(local))
  expect_match(res$detail[2], "unexpected file layout")

  res <- download_who_indicators(file.path(dir, "dl2"), fallback = NULL)
  expect_equal(res$origin[2], "unavailable")
  expect_true(is.na(res$path[2]))
})
