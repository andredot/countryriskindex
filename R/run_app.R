# `%||%` is only in base R from 4.4.0.
`%||%` <- function(x, y) if (is.null(x)) y else x

#' Launch the Country Risk Explorer Shiny Application
#'
#' This function initializes and launches an interactive Shiny web application
#' designed to explore and visualize country-level health and crisis risk data.
#' The app includes a score decomposition (waterfall and contribution bars),
#' a 3D scatterplot, a radar chart, a risk distribution histogram, and an
#' adjusted risk map.
#'
#' The decomposition is **precomputed** by the `risk_decomposition` target
#' rather than calculated here: the Shapley values behind it are the expensive
#' part, and recomputing them on every country selection would make the app
#' unusable.
#'
#' @param raw_data A data frame containing the full dataset with country-level
#' health and risk indicators. This dataset is used across all visualizations.
#' @param radar_data A data frame containing a subset of the raw data
#' specifically
#' formatted for radar chart visualization. It should include numeric indicators
#' relevant to health risk dimensions.
#' @param corrected_radar_data A data frame similar in structure to `radar_data`,
#' but with severity-adjusted values used to compare original and corrected
#'  risk profiles.
#' @param decomposition Precomputed output of [build_decomposition()]. When
#'  `NULL` the decomposition tab reports that it is unavailable rather than
#'  failing.
#'
#' The returned app object is self-contained: it carries its own copy of the
#' plotting helpers, so it still runs after being restored with
#' `targets::tar_read(shiny_explorer)` in a session where `tar_source()` has not
#' been called. See `self_contained()`.
#'
#' @return A `shiny.appobj`. Printing it (which is what happens when it is
#'   returned at the console) or passing it to [shiny::runApp()] launches the
#'   app in the user's default web browser.
#' @export
run_app <- function(raw_data,
                    radar_data,
                    corrected_radar_data,
                    decomposition = NULL) {

  country_choices <- sort(unique(stats::na.omit(raw_data$country)))

  shiny::shinyApp(
    ui = shiny::fluidPage(
      shiny::titlePanel("Country Risk Explorer"),
      shiny::sidebarLayout(
        shiny::sidebarPanel(
          shiny::selectInput("country", "Select a Country:",
                             choices = country_choices),
          shiny::conditionalPanel(
            condition = "input.tabs == 'Score Decomposition'",
            shiny::radioButtons(
              "decomp_level", "Detail:",
              choices = c("Components" = "component",
                          "Indicators" = "indicator"),
              selected = "component"
            ),
            shiny::checkboxInput("decomp_severity",
                                 "Include crisis modifier", TRUE)
          )
        ),
        shiny::mainPanel(
          shiny::tabsetPanel(
            id = "tabs",
            shiny::tabPanel(
              "Score Decomposition",
              shiny::plotOutput("waterfallPlot", height = "560px"),
              shiny::plotOutput("contributionPlot", height = "460px"),
              shiny::tableOutput("decompSummary")
            ),
            shiny::tabPanel("3D Scatterplot",
                            plotly::plotlyOutput("scatter3D", height = "800px")),
            shiny::tabPanel("Radar Plot",
                            shiny::plotOutput("radarPlot", height = "800px")),
            shiny::tabPanel("Overall Risk Histogram",
                            shiny::plotOutput("riskHistogram"), width = "400px"),
            shiny::tabPanel("Adjusted Risk Map",
                            leaflet::leafletOutput("adjRiskMap", height = "800px"))
          )
        )
      )
    ),
    server = self_contained(function(input, output) {
      # The decomposition is read, never recomputed: build_decomposition()
      # enumerates Shapley coalitions per country and belongs in the pipeline.
      no_decomp <- function(msg) {
        ggplot2::ggplot() +
          ggplot2::annotate("text", x = 0, y = 0, label = msg,
                            size = 5, colour = "grey30") +
          ggplot2::theme_void()
      }

      output$waterfallPlot <- shiny::renderPlot({
        if (is.null(decomposition)) {
          return(no_decomp(paste(
            "No decomposition supplied.",
            "Build the `risk_decomposition` target and pass it to run_app().",
            sep = "\n")))
        }
        create_decomposition_waterfall(
          decomposition,
          country = input$country,
          level = input$decomp_level %||% "component",
          include_severity = isTRUE(input$decomp_severity)
        )
      })

      output$contributionPlot <- shiny::renderPlot({
        if (is.null(decomposition)) return(no_decomp(""))
        create_contribution_bars(decomposition, country = input$country)
      })

      output$decompSummary <- shiny::renderTable({
        if (is.null(decomposition)) return(NULL)
        d <- decomposition$summary
        d <- d[d$country == input$country, , drop = FALSE]
        if (nrow(d) == 0) return(NULL)

        fmt <- function(x) if (is.numeric(x)) sprintf("%.3f", x) else as.character(x)
        keep <- intersect(
          c("reference_risk", "overall_risk", "severity_adjusted_risk",
            "severity_contribution", "dominant_driver", "mitigating_factor",
            "data_completeness", "low_confidence"),
          names(d)
        )
        data.frame(
          Measure = c(
            reference_risk = "Reference country risk",
            overall_risk = "Structural risk",
            severity_adjusted_risk = "Severity-adjusted risk",
            severity_contribution = "Crisis contribution (log)",
            dominant_driver = "Dominant driver",
            mitigating_factor = "Mitigating factor",
            data_completeness = "Data completeness",
            low_confidence = "Low confidence"
          )[keep],
          Value = vapply(keep, function(k) fmt(d[[k]][1]), character(1)),
          row.names = NULL,
          stringsAsFactors = FALSE
        )
      })

      output$riskHistogram <- shiny::renderPlot({
        selected_data <- raw_data |>
          dplyr::filter(country == input$country)
        shiny::validate(shiny::need(nrow(selected_data) == 1,
                                    "Select a single country."))

        plot_data <- raw_data |>
          tidyr::pivot_longer(cols = c(overall_risk, severity_adjusted_risk),
                              names_to = "risk_type",
                              values_to = "value")

        plot_data <- plot_data[is.finite(plot_data$value), , drop = FALSE]
        shiny::validate(shiny::need(nrow(plot_data) > 0,
                                    "No risk scores available."))
        bin_breaks <- pretty(range(plot_data$value, na.rm = TRUE), n = 5)

        binned_data <- plot_data |>
          dplyr::mutate(bin = cut(value, breaks = bin_breaks,
                                  include.lowest = TRUE)) |>
          dplyr::group_by(risk_type, bin) |>
          dplyr::summarise(count = dplyr::n(), .groups = "drop") |>
          dplyr::mutate(count = dplyr::if_else(risk_type == "overall_risk",
                                               -count, count))

        selected_overall_bin <- cut(selected_data$overall_risk,
                                    breaks = bin_breaks, include.lowest = TRUE)
        selected_severity_bin <- cut(selected_data$severity_adjusted_risk,
                                     breaks = bin_breaks, include.lowest = TRUE)

        # A country whose score falls outside the binned range yields no match;
        # pulling an empty vector into geom_segment() silently breaks the layer.
        first_or_zero <- function(x) if (length(x) == 0) 0 else x[1]

        overall_y <- binned_data |>
          dplyr::filter(risk_type == "overall_risk",
                        bin == selected_overall_bin) |>
          dplyr::pull(count) |>
          first_or_zero()

        severity_y <- binned_data |>
          dplyr::filter(risk_type == "severity_adjusted_risk",
                        bin == selected_severity_bin) |>
          dplyr::pull(count) |>
          first_or_zero()

        ggplot2::ggplot(binned_data, ggplot2::aes(x = bin, y = count,
                                                  fill = risk_type)) +
          ggplot2::geom_col(width = 1, color = "white", alpha = 0.7) +
          ggplot2::geom_hline(yintercept = 0, color = "black") +
          # annotate(), not geom_segment(aes()): a single constant segment
          # would otherwise be drawn once per row of binned_data.
          ggplot2::annotate("segment",
                            x = selected_overall_bin,
                            xend = selected_overall_bin,
                            y = 0, yend = overall_y,
                            color = "deeppink", linetype = "dashed",
                            linewidth = 1.2) +
          ggplot2::annotate("segment",
                            x = selected_severity_bin,
                            xend = selected_severity_bin,
                            y = 0, yend = severity_y,
                            color = "blue", linetype = "dashed",
                            linewidth = 1.2) +
          ggplot2::geom_text(data = data.frame(bin = selected_overall_bin,
                                               count = overall_y),
                             ggplot2::aes(
                               x = bin, y = overall_y - 1,
                               label = round(selected_data$overall_risk, 2)),
                             color = "deeppink", hjust = 1.1,
                             inherit.aes = FALSE) +
          ggplot2::geom_text(data = data.frame(bin = selected_severity_bin,
                                               count = severity_y),
                             ggplot2::aes(
                               x = bin, y = severity_y + 1,
                               label = round(selected_data$severity_adjusted_risk, 2)),
                             color = "blue", hjust = -0.1, inherit.aes = FALSE) +
          ggplot2::coord_flip() +
          ggplot2::labs(
            title = "Pyramid Plot of Risk Distributions",
            subtitle = paste0(
              "Selected Country: <b>", input$country, "</b> - <br> ",
              "<span style='color:deeppink;'>Country Health Risk (left)</span><br> ",
              "<span style='color:blue;'>Severity-Adjusted Health Risk (right)</span>"
            ),
            x = "Risk Bins", y = "Count"
          ) +
          ggplot2::theme_minimal() +
          ggplot2::theme(
            legend.position = "none",
            axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
            plot.subtitle = ggtext::element_markdown()
          )
      })

      output$scatter3D <- plotly::renderPlotly({
        plotly::plot_ly(raw_data, x = ~hazard_score, y = ~vulnerability_score, z = ~capacity_score,
                        type = "scatter3d", mode = "markers",
                        marker = list(size = 4, color = 'lightblue'),
                        text = ~country) |>
          plotly::add_trace(data = dplyr::filter(raw_data, country == input$country),
                            x = ~hazard_score, y = ~vulnerability_score, z = ~capacity_score,
                            type = "scatter3d", mode = "markers",
                            marker = list(size = 6, color = 'blue'),
                            name = "Selected Country")
      })

      output$radarPlot <- shiny::renderPlot({
        # Ensure country column is character
        radar_data$Country <- as.character(radar_data$Country)
        corrected_radar_data$Country <- as.character(corrected_radar_data$Country)
        raw_data$country <- as.character(raw_data$country)

        # Select only numeric columns for radar
        radar_vars <- radar_data |>
          dplyr::select(tidyselect::where(is.numeric)) |>
          colnames()

        # Normalize full datasets column-wise
        radar_data_norm <- radar_data |>
          dplyr::mutate(
            dplyr::across(
              .cols = dplyr::all_of(radar_vars),
              .fns = ~ scales::rescale(.x, to = c(0, 1))
            )
          )

        corrected_radar_data_norm <- corrected_radar_data |>
          dplyr::mutate(
            dplyr::across(
              .cols = dplyr::all_of(radar_vars),
              .fns = ~ scales::rescale(.x, to = c(0, 1))
            )
          )

        # Filter selected country
        selected <- radar_data_norm |>
          dplyr::filter(Country == input$country)
        corrected <- corrected_radar_data_norm |>
          dplyr::filter(Country == input$country)

        # Check for valid data
        if (nrow(selected) == 0 || all(is.na(selected[, radar_vars]))) {
          graphics::plot.new()
          graphics::title("No data available for selected country")
          return()
        }

        # Get Drivers value
        drivers_value <- if (!"CRISIS" %in% names(raw_data)) {
          character(0)
        } else {
          raw_data |>
            dplyr::filter(country == input$country) |>
            dplyr::pull(CRISIS) |>
            unique() |>
            as.character()
        }

        # `||` errors on zero-length or length > 1 operands under R >= 4.3, and
        # a country with several crises returns more than one value.
        drivers_value <- drivers_value[!is.na(drivers_value) &
                                         nzchar(drivers_value)]
        if (length(drivers_value) == 0) {
          # Only original data
          combined <- selected[, radar_vars]
          combined$Country <- "Original"


          combined |>
            dplyr::relocate(Country) |>
            ggradar::ggradar(grid.min = 0, grid.mid = 0.5, grid.max = 1,
                             values.radar = c("0%", "50%", "100%"),
                             group.line.width = 1.5,
                             group.point.size = 3,
                             group.colours = "deepskyblue",
                             fill = TRUE,
                             background.circle.colour = "grey90",
                             gridline.mid.colour = "grey",
                             legend.position = "bottom",
                             plot.title = paste("Radar Plot for", input$country))
        } else {
          # Original + Corrected data
          original_row <- selected[, radar_vars]
          original_row$Country <- "Original"

          corrected_row <- corrected[, radar_vars]
          corrected_row$Country <- "Corrected"

          combined <- dplyr::bind_rows(original_row, corrected_row)

          subtitle_text <- paste("\n Health risk corrected for",
                                 paste(drivers_value, collapse = "; "))

          combined |>
            dplyr::relocate(Country) |>
            ggradar::ggradar(grid.min = 0, grid.mid = 0.5, grid.max = 1,
                             values.radar = c("0%", "50%", "100%"),
                             group.line.width = 1.5,
                             group.point.size = 3,
                             group.colours = c("deeppink", "deepskyblue"),
                             fill = TRUE,
                             background.circle.colour = "grey90",
                             gridline.mid.colour = "grey",
                             legend.position = "bottom",
                             plot.title = paste("Radar Plot for", input$country, subtitle_text)
            )
        }

      })

      output$adjRiskMap <- leaflet::renderLeaflet({
        createmap_adjusted_risk_score(raw_data, input$country)
      })

    })
  )
}

#' Make a closure carry the functions it depends on
#'
#' Functions loaded with `targets::tar_source()` live in the global environment,
#' and R serialises the global environment *by reference*, not by value. A Shiny
#' app built inside the pipeline and restored with `tar_read()` would therefore
#' look its helpers (`create_decomposition_waterfall()`,
#' `createmap_adjusted_risk_score()`, ...) up in the *reading* session's global
#' environment, where they normally do not exist.
#'
#' This copies every function defined in the global environment into a private
#' environment that is serialised together with `fun`, alongside the local
#' variables `fun` closes over (the data passed to [run_app()]). When the
#' package is loaded as a namespace (installed, or via `devtools::load_all()`)
#' the namespace itself is restored on read, so `fun` is returned unchanged.
#'
#' @param fun A closure, typically a Shiny server function.
#' @return `fun`, with an environment that no longer depends on the caller's
#'   global environment for functions defined there.
#' @noRd
self_contained <- function(fun) {
  if (!identical(topenv(environment(fun)), globalenv())) return(fun)

  helpers <- new.env(parent = globalenv())
  for (nm in ls(globalenv(), all.names = TRUE)) {
    f <- get(nm, envir = globalenv())
    if (is.function(f) && identical(environment(f), globalenv())) {
      environment(f) <- helpers
      assign(nm, f, envir = helpers)
    }
  }

  environment(fun) <- list2env(as.list(environment(fun), all.names = TRUE),
                               parent = helpers)
  fun
}
