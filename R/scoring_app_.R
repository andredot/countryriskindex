#' Launch the live scoring app
#'
#' Upload the source files (GBD, WHO, INFORM Risk and Severity, crisis modifier
#' matrix) and the app recomputes the scores on the fly: as soon as every input
#' is present, and again whenever one is replaced. It runs exactly the steps of
#' the `risk_score` target ([compute_risk_score()]), then the decomposition
#' ([build_decomposition()]) and the effective weights
#' ([summarise_global_weights()]), and shows:
#' \itemize{
#'   \item the new scores, ranked;
#'   \item the change against a baseline run, when one is supplied;
#'   \item the effective weight of each component and indicator;
#'   \item the exact decomposition of any country's score;
#'   \item the inputs used and the time each step took.
#' }
#' Everything can be downloaded as one Excel workbook.
#'
#' Unlike [run_calculator_app()], nothing is read from the shared input folder
#' unless `defaults` is supplied: by default every file must be uploaded.
#'
#' @param baseline Optional scored data frame to compare against, typically
#'   `targets::tar_read(risk_score)`.
#' @param defaults Optional named vector of default paths (see
#'   [default_risk_input_paths()]) used for inputs left empty. `NULL` (the
#'   default) means every file must be uploaded.
#' @param max_upload_mb Maximum size of a single uploaded file, in MB.
#' @param compute Function computing the score, see [compute_risk_score()].
#'   Exposed for testing.
#' @param debounce_ms Delay after the last change before recomputing, so that
#'   several files dropped in quick succession trigger one computation.
#'
#' @return A `shiny.appobj`; printing it launches the app.
#' @export
run_scoring_app <- function(baseline = NULL,
                            defaults = NULL,
                            max_upload_mb = 500,
                            compute = compute_risk_score,
                            debounce_ms = 1000) {
  files <- risk_input_files()
  reference_quantiles <- read_reference_quantiles()

  ui <- shiny::fluidPage(
    shiny::titlePanel("Country Risk Index: live scoring"),
    shiny::sidebarLayout(
      shiny::sidebarPanel(
        width = 3,
        shiny::uiOutput("upload_status"),
        risk_input_pickers(defaults)
      ),
      shiny::mainPanel(
        width = 9,
        shiny::uiOutput("status"),
        shiny::conditionalPanel(
          "output.ready",
          shiny::downloadButton("download", "Download results (.xlsx)"),
          shiny::br(), shiny::br(),
          shiny::tabsetPanel(
            id = "tabs",
            shiny::tabPanel(
              "Scores",
              shiny::br(),
              shiny::selectInput("n_rows", "Show",
                                 c("Top 25" = 25, "Top 50" = 50,
                                   "All countries" = Inf),
                                 width = "200px"),
              shiny::tableOutput("scores")
            ),
            shiny::tabPanel(
              "Change vs baseline",
              shiny::br(),
              shiny::uiOutput("baseline_note"),
              shiny::plotOutput("comparison_plot", height = "600px"),
              shiny::tableOutput("comparison")
            ),
            shiny::tabPanel(
              "Effective weights",
              shiny::br(),
              shiny::plotOutput("weights_component", height = "320px"),
              shiny::tableOutput("weights_table"),
              shiny::plotOutput("weights_indicator", height = "520px")
            ),
            shiny::tabPanel(
              "Country decomposition",
              shiny::br(),
              shiny::fluidRow(
                shiny::column(4, shiny::selectInput("country", "Country",
                                                    choices = NULL)),
                shiny::column(4, shiny::radioButtons(
                  "decomp_level", "Detail",
                  c("Components" = "component", "Indicators" = "indicator"),
                  inline = TRUE))
              ),
              shiny::plotOutput("waterfall", height = "520px"),
              shiny::plotOutput("contributions", height = "420px")
            ),
            shiny::tabPanel(
              "Inputs & log",
              shiny::br(),
              shiny::h4("Inputs used"),
              shiny::tableOutput("sources"),
              shiny::h4("Steps"),
              shiny::tableOutput("log")
            )
          )
        )
      )
    )
  )

  server <- function(input, output, session) {
    result <- shiny::reactiveVal(NULL)
    error <- shiny::reactiveVal(NULL)

    chosen <- shiny::reactive(chosen_risk_inputs(input, defaults))

    # Recompute on the fly: whenever the set of files or the INFORM sheets
    # change, once nothing has changed for `debounce_ms`.
    trigger <- shiny::debounce(shiny::reactive(list(
      chosen()$paths,
      input$inform_risk_sheet %||% 2,
      input$inform_lcc_sheet %||% 5
    )), debounce_ms)

    shiny::observeEvent(trigger(), {
      ch <- chosen()
      if (anyNA(ch$paths)) {
        result(NULL)
        error(NULL)
        return()
      }

      res <- shiny::withProgress(message = "Computing scores", value = 0, {
        out <- tryCatch(
          compute(ch$paths,
                  inform_risk_sheet = input$inform_risk_sheet %||% 2,
                  inform_lcc_sheet = input$inform_lcc_sheet %||% 5,
                  reference_quantiles = reference_quantiles,
                  progress = function(step, i, n) {
                    shiny::setProgress(value = 0.8 * (i - 1) / n,
                                       detail = step)
                  }),
          error = function(e) {
            error(conditionMessage(e))
            NULL
          }
        )
        if (!is.null(out)) {
          shiny::setProgress(0.85, detail = "Decomposing scores")
          out$decomposition <- tryCatch(
            build_decomposition(out$risk_score,
                                reference_quantiles = reference_quantiles),
            error = function(e) {
              out$decomposition_error <<- conditionMessage(e)
              NULL
            }
          )
          if (!is.null(out$decomposition)) {
            out$weights <- summarise_global_weights(out$decomposition)
          }
          out$comparison <- compare_runs(out$risk_score, baseline)
        }
        out
      })
      if (is.null(res)) {
        result(NULL)
        return()
      }

      res$sources <- describe_risk_inputs(ch$paths, ch$names)
      res$finished <- Sys.time()
      error(NULL)
      result(res)

      countries <- sort(unique(stats::na.omit(
        res$decomposition$summary$country)))
      shiny::updateSelectInput(session, "country", choices = countries,
                               selected = res$decomposition$summary$country[1])
    })

    output$ready <- shiny::reactive(!is.null(result()))
    shiny::outputOptions(output, "ready", suspendWhenHidden = FALSE)

    output$upload_status <- shiny::renderUI({
      n_ok <- sum(!is.na(chosen()$paths))
      shiny::tags$p(
        shiny::strong(sprintf("%d of %d inputs ready", n_ok, nrow(files))),
        if (n_ok == nrow(files)) " - scores update automatically."
      )
    })

    output$status <- shiny::renderUI({
      if (!is.null(error())) {
        return(shiny::div(
          class = "alert alert-danger", error(), shiny::br(),
          "Replace the file concerned: the scores recompute automatically."))
      }
      res <- result()
      if (is.null(res)) {
        missing <- files$label[is.na(chosen()$paths)]
        return(shiny::div(
          class = "alert alert-info",
          "Upload the input files on the left. Scores are computed as soon ",
          "as all of them are present, and recomputed whenever one changes.",
          if (length(missing) > 0) {
            shiny::tagList(shiny::br(), shiny::strong("Still missing: "),
                           paste(missing, collapse = "; "))
          }))
      }
      shiny::div(
        class = "alert alert-success",
        sprintf("Scores computed for %d countries at %s.",
                sum(!is.na(res$risk_score$overall_risk)),
                format(res$finished, "%H:%M:%S")),
        if (!is.null(res$decomposition_error)) {
          shiny::tagList(shiny::br(), "Decomposition unavailable: ",
                         res$decomposition_error)
        }
      )
    })

    output$scores <- shiny::renderTable({
      res <- shiny::req(result())
      d <- as.data.frame(res$risk_score)
      d <- d[, intersect(snapshot_columns(), names(d)), drop = FALSE]
      if ("severity_adjusted_risk" %in% names(d)) {
        d <- d[order(-d$severity_adjusted_risk, na.last = TRUE), , drop = FALSE]
      }
      d <- cbind(rank = seq_len(nrow(d)), d)
      utils::head(d, as.numeric(input$n_rows %||% 25))
    }, digits = 3)

    output$baseline_note <- shiny::renderUI({
      if (is.null(baseline)) {
        shiny::div(class = "alert alert-secondary",
                   "No baseline supplied. Launch the app with ",
                   shiny::code("run_scoring_app(baseline = tar_read(risk_score))"),
                   " to compare the new scores with the current pipeline run.")
      }
    })

    output$comparison_plot <- shiny::renderPlot({
      res <- shiny::req(result())
      shiny::req(res$comparison)
      create_run_comparison_plot(res$comparison) +
        ggplot2::labs(title = "Change against the baseline",
                      subtitle = "Grey: baseline. Red: uploaded inputs.")
    })

    output$comparison <- shiny::renderTable({
      res <- shiny::req(result())
      shiny::req(res$comparison)
      utils::head(res$comparison[, c("country", "previous", "current",
                                     "difference", "status")], 30)
    }, digits = 3)

    weights <- shiny::reactive({
      res <- shiny::req(result())
      shiny::validate(shiny::need(res$weights, "Weights unavailable."))
      res$weights
    })

    output$weights_component <- shiny::renderPlot(
      create_global_weights_plot(weights(), "component"))
    output$weights_indicator <- shiny::renderPlot(
      create_global_weights_plot(weights(), "indicator"))
    output$weights_table <- shiny::renderTable({
      w <- weights()$component
      data.frame(Component = w$label,
                 `Nominal weight` = w$nominal_weight,
                 `Standardised beta` = w$std_beta,
                 `Variance share` = w$variance_share,
                 check.names = FALSE)
    }, digits = 3, na = "")

    decomposition <- shiny::reactive({
      res <- shiny::req(result())
      shiny::validate(shiny::need(res$decomposition,
                                  "Decomposition unavailable."))
      res$decomposition
    })

    output$waterfall <- shiny::renderPlot({
      shiny::req(input$country)
      create_decomposition_waterfall(decomposition(), country = input$country,
                                     level = input$decomp_level %||%
                                       "component")
    })

    output$contributions <- shiny::renderPlot({
      shiny::req(input$country)
      create_contribution_bars(decomposition(), country = input$country)
    })

    output$sources <- shiny::renderTable({
      shiny::req(result())$sources[, c("input", "file", "size_kb", "modified")]
    })

    output$log <- shiny::renderTable(shiny::req(result())$log)

    output$download <- shiny::downloadHandler(
      filename = function() {
        paste0("risk_score_",
               format(result()$finished %||% Sys.time(), "%Y%m%d_%H%M"),
               ".xlsx")
      },
      content = function(file) {
        res <- result()
        write_risk_score_xlsx(
          res$risk_score, file,
          sources = res$sources,
          reference_quantiles = reference_quantiles,
          log = res$log,
          extra = list(weights_component = res$weights$component,
                       weights_indicator = res$weights$indicator,
                       decomposition_summary = res$decomposition$summary,
                       change_vs_baseline = res$comparison)
        )
      }
    )
  }

  shiny::shinyApp(
    ui = ui,
    server = self_contained(server),
    onStart = upload_limit(max_upload_mb)
  )
}
