# Risk score calculator: compute `risk_score` from user-chosen input files,
# outside the targets pipeline, and export it to Excel.

#' Input files consumed by the risk score
#'
#' The single list of source files behind `risk_score`, relative to the shared
#' input folder (see `get_input_data_path()`). Both `_targets.R` and
#' [run_calculator_app()] read their defaults from here, so a new data round
#' only needs updating in one place.
#'
#' @return A data frame with one row per input and columns `id`, `label`,
#'   `group`, `file` (default relative path) and `accept` (file extensions).
#' @export
risk_input_files <- function() {
  data.frame(
    id = c("gbd_daly", "gbd_location_keys", "haqi",
           "who_location_keys", "who_uhc", "who_doctors",
           "inform_risk", "inform_severity", "crisis_modifier"),
    label = c("GBD DALY rates by cause",
              "GBD location keys",
              "GBD Healthcare Access and Quality index (HAQI)",
              "WHO location keys",
              "WHO 9A706FD: UHC service coverage index",
              "WHO 217795A: density of doctors",
              "INFORM Risk (incl. Lack of Coping Capacity sheet)",
              "INFORM Severity",
              "Crisis modifier matrix"),
    group = c("GBD", "GBD", "GBD", "WHO", "WHO", "WHO",
              "INFORM", "INFORM", "Utilities"),
    file = c(paste0("GBD/IHME-GBD_", gbd_year(), "_DALY.csv"),
             paste0("GBD/IHME_GBD_", gbd_year(), "_location_keys.csv"),
             "GBD/HAQI/IHME_GBD_2019_HAQ_1990_2019_DATA_Y2022M012D21.csv",
             "WHO/WHO_loc_keys.xlsx",
             "WHO/9A706FD_ALL_LATEST.csv",
             "WHO/217795A_ALL_LATEST.csv",
             "EU/INFORM_Risk_Mid_2025_v071.xlsx",
             "EU/202512_inform_severity_mid_december_2025.xlsx",
             "crisis_modifier_matrix.xlsx"),
    accept = c(".csv", ".csv", ".csv", ".xlsx", ".csv", ".csv",
               ".xlsx", ".xlsx", ".xlsx"),
    stringsAsFactors = FALSE
  )
}

#' Path of one risk score input in the shared input folder
#'
#' @param id An input id from [risk_input_files()].
#' @return The absolute path, as built by `get_input_data_path()`.
#' @export
risk_input_path <- function(id) {
  files <- risk_input_files()
  if (!id %in% files$id) stop("Unknown risk input '", id, "'.", call. = FALSE)
  get_input_data_path(files$file[files$id == id])
}

#' Default local paths of the risk score inputs
#'
#' @param files Output of [risk_input_files()].
#' @return A named character vector (names are input ids) of absolute paths,
#'   `NA` where the file does not exist in the shared input folder.
#' @export
default_risk_input_paths <- function(files = risk_input_files()) {
  paths <- vapply(files$file, function(f) {
    p <- suppressWarnings(get_input_data_path(f))
    if (file.exists(p)) p else NA_character_
  }, character(1), USE.NAMES = FALSE)
  stats::setNames(paths, files$id)
}

#' Score merged indicators
#'
#' The scoring chain applied to the merged indicators: data completeness, the
#' three pillars against the frozen reference, the overall risk and the crisis
#' modifier. Shared by the `risk_score` target and [compute_risk_score()].
#'
#' @param merged_indicators Output of [merge_health_datasets()].
#' @param cm_data The crisis modifier matrix.
#' @param reference_quantiles Output of [read_reference_quantiles()].
#' @return The scored data frame.
#' @export
score_risk <- function(merged_indicators, cm_data, reference_quantiles) {
  merged_indicators |>
    add_data_completeness() |>
    add_hazard_score(reference = reference_quantiles) |>
    add_vulnerability_score(reference = reference_quantiles) |>
    add_capacity_score(reference = reference_quantiles) |>
    add_overall_risk() |>
    add_severity(cm_data = cm_data)
}

#' Compute the risk score from a set of input files
#'
#' Runs the same import, preprocessing and scoring steps as the `risk_score`
#' target, but on explicitly supplied files. Each step is labelled, so an error
#' says which input it came from (typically: the wrong file was selected).
#'
#' @param paths Named character vector or list of file paths, with the ids of
#'   [risk_input_files()] as names.
#' @param inform_risk_sheet,inform_lcc_sheet Sheets of the INFORM Risk workbook
#'   holding the risk indicators and the Lack of Coping Capacity indicators.
#' @param reference_quantiles Output of [read_reference_quantiles()].
#' @param progress Optional `function(step, i, n)` called before each step.
#'
#' @return A list with `risk_score` (the scored data frame) and `log` (a data
#'   frame with one row per step, its duration and the number of rows it
#'   produced).
#' @export
compute_risk_score <- function(paths,
                               inform_risk_sheet = 2,
                               inform_lcc_sheet = 5,
                               reference_quantiles = read_reference_quantiles(),
                               progress = NULL) {
  paths <- unlist(paths)
  missing <- setdiff(risk_input_files()$id, names(paths)[!is.na(paths)])
  if (length(missing) > 0) {
    stop("Missing input file(s): ", paste(missing, collapse = ", "),
         call. = FALSE)
  }

  steps <- list(
    list("Import GBD DALY rates", "gbd_data",
         function(x) import_data(paths[["gbd_daly"]])),
    list("Import GBD location keys", "location_keys",
         function(x) import_data(paths[["gbd_location_keys"]])),
    list("Import HAQI", "haqi_data",
         function(x) import_data(paths[["haqi"]])),
    list("Import WHO location keys", "who_location_keys",
         function(x) import_excel(paths[["who_location_keys"]])),
    list("Import WHO UHC coverage (9A706FD)", "who_uhc",
         function(x) import_data(paths[["who_uhc"]])),
    list("Import WHO density of doctors (217795A)", "who_doctors",
         function(x) import_data(paths[["who_doctors"]])),
    list("Import INFORM Risk", "inform_risk_data",
         function(x) import_inform_excel(paths[["inform_risk"]],
                                         sheet = inform_risk_sheet)),
    list("Import INFORM Lack of Coping Capacity", "inform_lcc_data",
         function(x) import_inform_excel(paths[["inform_risk"]],
                                         sheet = inform_lcc_sheet)),
    list("Import INFORM Severity", "inform_severity_data",
         function(x) import_severity_excel(paths[["inform_severity"]])),
    list("Import crisis modifier matrix", "cm_data",
         function(x) import_cm_matrix(paths[["crisis_modifier"]])),
    list("Preprocess GBD DALY rates", "gbd_rates",
         function(x) preprocess_gbd_rates_by_cause(x$gbd_data,
                                                   x$location_keys)),
    list("Preprocess HAQI", "haq_index",
         function(x) preprocess_haq_index(x$haqi_data, x$location_keys)),
    list("Preprocess WHO indicators", "who_indicators",
         function(x) preprocess_who_data(x$who_uhc, x$who_doctors,
                                         x$who_location_keys)),
    list("Preprocess INFORM Risk", "inform_cap",
         function(x) preprocess_inform(x$inform_risk_data,
                                       x$inform_lcc_data)),
    list("Preprocess INFORM Severity", "inform_severity",
         function(x) preprocess_severity(x$inform_severity_data)),
    list("Merge indicators", "merged_indicators",
         function(x) merge_health_datasets(x$gbd_rates, x$haq_index,
                                           x$inform_cap, x$who_indicators,
                                           x$inform_severity)),
    list("Score risk", "risk_score",
         function(x) score_risk(x$merged_indicators, x$cm_data,
                                reference_quantiles))
  )

  out <- list()
  log <- data.frame(step = character(0), seconds = numeric(0),
                    rows = integer(0), stringsAsFactors = FALSE)
  for (i in seq_along(steps)) {
    label <- steps[[i]][[1]]
    if (is.function(progress)) progress(label, i, length(steps))
    started <- Sys.time()
    out[[steps[[i]][[2]]]] <- tryCatch(
      steps[[i]][[3]](out),
      error = function(e) {
        stop("Step '", label, "' failed: ", conditionMessage(e),
             call. = FALSE)
      }
    )
    res <- out[[steps[[i]][[2]]]]
    log[i, ] <- list(label,
                     round(as.numeric(difftime(Sys.time(), started,
                                               units = "secs")), 2),
                     if (is.data.frame(res)) nrow(res) else NA_integer_)
  }

  list(risk_score = out$risk_score, log = log)
}

#' Write a computed risk score to an Excel workbook
#'
#' @param risk_score The scored data frame.
#' @param file Path of the `.xlsx` file to write.
#' @param sources Optional data frame describing the inputs used (see
#'   [describe_risk_inputs()]), written to a `sources` sheet.
#' @param reference_quantiles Optional reference distribution, written to a
#'   `reference_quantiles` sheet.
#' @param log Optional step log from [compute_risk_score()].
#'
#' @return `file`, invisibly.
#' @export
write_risk_score_xlsx <- function(risk_score,
                                  file,
                                  sources = NULL,
                                  reference_quantiles = NULL,
                                  log = NULL) {
  if (!requireNamespace("writexl", quietly = TRUE)) {
    stop("Package 'writexl' is needed to write the Excel file: ",
         "install it with renv::install(\"writexl\").", call. = FALSE)
  }

  # Headline columns first, then every diagnostic column as computed.
  first <- intersect(snapshot_columns(), names(risk_score))
  scores <- as.data.frame(risk_score)[, c(first, setdiff(names(risk_score),
                                                         first)),
                                      drop = FALSE]
  # Excel has no list columns.
  keep <- !vapply(scores, is.list, logical(1))
  scores <- scores[, keep, drop = FALSE]
  if ("severity_adjusted_risk" %in% names(scores)) {
    scores <- scores[order(-scores$severity_adjusted_risk, na.last = TRUE), ,
                     drop = FALSE]
  }

  sheets <- list(risk_score = scores)
  if (!is.null(sources)) sheets$sources <- sources
  if (!is.null(reference_quantiles) && nrow(reference_quantiles) > 0) {
    sheets$reference_quantiles <- as.data.frame(reference_quantiles)
  }
  if (!is.null(log)) sheets$log <- log

  writexl::write_xlsx(sheets, file)
  invisible(file)
}

#' Describe the input files of a run
#'
#' @param paths Named vector of paths actually read, ids as names.
#' @param names Optional named vector of display names (e.g. the original
#'   names of uploaded files); defaults to the base names of `paths`.
#' @return A data frame with one row per input: label, file, size, modification
#'   time and MD5 checksum, so a workbook can be traced back to its sources.
#' @export
describe_risk_inputs <- function(paths, names = NULL) {
  files <- risk_input_files()
  paths <- unlist(paths)[files$id]
  shown <- if (is.null(names)) basename(paths) else unlist(names)[files$id]
  info <- file.info(paths)
  data.frame(
    input = files$label,
    file = shown,
    size_kb = round(info$size / 1024, 1),
    modified = format(info$mtime, "%Y-%m-%d %H:%M"),
    md5 = unname(tools::md5sum(paths)),
    stringsAsFactors = FALSE
  )
}

#' Launch the risk score calculator
#'
#' A Shiny app to pick the source files (GBD, WHO, INFORM Risk and Severity,
#' crisis modifier matrix), compute the risk score with exactly the steps of the
#' `risk_score` target, and download it as an Excel workbook. Inputs left empty
#' fall back to the files in the shared input folder, when present.
#'
#' @param defaults Named vector of default paths; see
#'   [default_risk_input_paths()].
#' @param max_upload_mb Maximum size of a single uploaded file, in MB.
#' @param compute Function computing the score, see [compute_risk_score()].
#'   Exposed for testing.
#'
#' @return A `shiny.appobj`; printing it launches the app.
#' @export
run_calculator_app <- function(defaults = default_risk_input_paths(),
                               max_upload_mb = 500,
                               compute = compute_risk_score) {
  files <- risk_input_files()
  reference_quantiles <- read_reference_quantiles()

  file_picker <- function(i) {
    id <- files$id[i]
    default <- defaults[id]
    shiny::tagList(
      shiny::fileInput(paste0("file_", id), files$label[i],
                       accept = files$accept[i]),
      shiny::tags$div(
        style = "margin-top:-15px; margin-bottom:10px;",
        shiny::helpText(
          if (length(default) == 1 && !is.na(default)) {
            paste("Default:", basename(default))
          } else {
            "No default found: upload required."
          }
        )
      )
    )
  }

  groups <- unique(files$group)
  ui <- shiny::fluidPage(
    shiny::titlePanel("Country Risk Score Calculator"),
    shiny::sidebarLayout(
      shiny::sidebarPanel(
        width = 4,
        lapply(groups, function(g) {
          shiny::tagList(
            shiny::h4(g),
            lapply(which(files$group == g), file_picker)
          )
        }),
        shiny::tags$details(
          shiny::tags$summary("Advanced"),
          shiny::numericInput("inform_risk_sheet",
                              "INFORM Risk: indicators sheet", 2, min = 1),
          shiny::numericInput("inform_lcc_sheet",
                              "INFORM Risk: Lack of Coping Capacity sheet", 5,
                              min = 1)
        ),
        shiny::hr(),
        shiny::actionButton("calculate", "Calculate risk score",
                            class = "btn-primary")
      ),
      shiny::mainPanel(
        width = 8,
        shiny::uiOutput("status"),
        shiny::conditionalPanel(
          "output.ready",
          shiny::downloadButton("download", "Download risk_score.xlsx"),
          shiny::h4("Top 15 countries by severity-adjusted risk"),
          shiny::tableOutput("preview"),
          shiny::h4("Inputs used"),
          shiny::tableOutput("sources"),
          shiny::h4("Steps"),
          shiny::tableOutput("log")
        )
      )
    )
  )

  server <- function(input, output, session) {
    result <- shiny::reactiveVal(NULL)
    error <- shiny::reactiveVal(NULL)

    # Uploaded file if any, else the default.
    chosen <- shiny::reactive({
      picks <- lapply(files$id, function(id) {
        up <- input[[paste0("file_", id)]]
        if (!is.null(up)) {
          list(path = up$datapath[1], name = up$name[1])
        } else if (!is.na(defaults[id])) {
          list(path = unname(defaults[id]),
               name = basename(unname(defaults[id])))
        } else {
          list(path = NA_character_, name = NA_character_)
        }
      })
      list(paths = stats::setNames(vapply(picks, `[[`, "", "path"), files$id),
           names = stats::setNames(vapply(picks, `[[`, "", "name"), files$id))
    })

    shiny::observeEvent(input$calculate, {
      result(NULL)
      error(NULL)
      ch <- chosen()
      absent <- files$label[is.na(ch$paths)]
      if (length(absent) > 0) {
        error(paste("Please select:", paste(absent, collapse = "; ")))
        return()
      }

      res <- shiny::withProgress(message = "Computing risk score", value = 0, {
        tryCatch(
          compute(ch$paths,
                  inform_risk_sheet = input$inform_risk_sheet,
                  inform_lcc_sheet = input$inform_lcc_sheet,
                  reference_quantiles = reference_quantiles,
                  progress = function(step, i, n) {
                    shiny::setProgress(value = (i - 1) / n, detail = step)
                  }),
          error = function(e) {
            error(conditionMessage(e))
            NULL
          }
        )
      })
      if (is.null(res)) return()

      res$sources <- describe_risk_inputs(ch$paths, ch$names)
      res$finished <- Sys.time()
      result(res)
    })

    output$ready <- shiny::reactive(!is.null(result()))
    shiny::outputOptions(output, "ready", suspendWhenHidden = FALSE)

    output$status <- shiny::renderUI({
      if (!is.null(error())) {
        return(shiny::div(class = "alert alert-danger", error()))
      }
      res <- result()
      if (is.null(res)) {
        return(shiny::div(
          class = "alert alert-info",
          "Select the input files (empty inputs use the default shown below ",
          "them), then press ", shiny::strong("Calculate risk score"), "."))
      }
      shiny::div(
        class = "alert alert-success",
        sprintf("Risk score computed for %d countries at %s.",
                nrow(res$risk_score),
                format(res$finished, "%Y-%m-%d %H:%M"))
      )
    })

    output$preview <- shiny::renderTable({
      res <- result()
      shiny::req(res)
      d <- as.data.frame(res$risk_score)
      cols <- intersect(snapshot_columns(), names(d))
      d <- d[, cols, drop = FALSE]
      if ("severity_adjusted_risk" %in% cols) {
        d <- d[order(-d$severity_adjusted_risk, na.last = TRUE), , drop = FALSE]
      }
      utils::head(d, 15)
    }, digits = 3)

    output$sources <- shiny::renderTable({
      shiny::req(result())$sources[, c("input", "file", "size_kb", "modified")]
    })

    output$log <- shiny::renderTable({
      shiny::req(result())$log
    })

    output$download <- shiny::downloadHandler(
      filename = function() {
        paste0("risk_score_",
               format(result()$finished %||% Sys.time(), "%Y%m%d_%H%M"),
               ".xlsx")
      },
      content = function(file) {
        res <- result()
        write_risk_score_xlsx(res$risk_score, file,
                              sources = res$sources,
                              reference_quantiles = reference_quantiles,
                              log = res$log)
      }
    )
  }

  old_max <- NULL
  shiny::shinyApp(
    ui = ui,
    server = self_contained(server),
    onStart = function() {
      # GBD extracts easily exceed Shiny's 5 MB default upload limit.
      old_max <<- options(shiny.maxRequestSize = max_upload_mb * 1024^2)
      shiny::onStop(function() options(old_max))
    }
  )
}
