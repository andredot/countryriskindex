#' Launch the Country Risk Explorer Shiny Application
#'
#' This function initializes and launches an interactive Shiny web application
#' designed to explore and visualize country-level health and crisis risk data.
#' The app includes multiple interactive visualizations such as a lollipop plot,
#' 3D scatterplot, radar chart, risk distribution histogram, and an adjusted risk
#'  map.
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
#'
#' @return This function does not return a value. It launches a Shiny
#' application in the user's default web browser.
#' @export
run_app <- function(raw_data,
                    radar_data,
                    corrected_radar_data) {
  shiny::shinyApp(
    ui = shiny::fluidPage(
      shiny::titlePanel("Country Risk Explorer"),
      shiny::sidebarLayout(
        shiny::sidebarPanel(
          shiny::selectInput("country", "Select a Country:",
                             choices = unique(raw_data$country))
        ),
        shiny::mainPanel(
          shiny::tabsetPanel(
            shiny::tabPanel("Lollipop Plot",
                            shiny::plotOutput("lollipopPlot", height = "2400px")),
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
    server = function(input, output) {
      output$lollipopPlot <- shiny::renderPlot({
        # Define variable groups in desired order
        # Cause columns are derived from the data: the GBD extract now carries
        # all level-2 causes, so hard-coding a list would silently drop
        # whichever causes GBD renames between rounds.
        cause_vars <- intersect(gbd_cause_columns(raw_data), names(raw_data))

        ordered_vars <- c(
          "overall_risk", "severity_adjusted_risk",
          "hazard_score", "All causes", cause_vars,
          "vulnerability_score",
          "infrastructure", "adult_literacy", "vulnerable_groups",
          "soc_econ_vulnerability",
          "capacity_score",
          "governance", "financing", "resources", "services",
          "severity_index",
          "crisis_impact", "people_conditions", "crisis_complexity"
        )

        # Normalize and reshape data
        numeric_vars <- raw_data |>
          dplyr::select(where(is.numeric)) |>
          dplyr::select(dplyr::any_of(ordered_vars)) |>
          colnames()

        df_long <- raw_data |>
          dplyr::select(country, all_of(numeric_vars)) |>
          dplyr::mutate(dplyr::across(where(is.numeric), normalise_quantiles)) |>
          tidyr::pivot_longer(-country,
                              names_to = "Variable",
                              values_to = "Value") |>
          dplyr::group_by(Variable) |>
          dplyr::mutate(Global_Avg = mean(Value, na.rm = TRUE)) |>
          dplyr::filter(country == input$country)

        # Assign colors and sizes
        df_long <- df_long |>
          dplyr::mutate(
            Size = dplyr::case_when(
              Variable %in% c("overall_risk", "severity_adjusted_risk") ~ 10,
              Variable %in% c("hazard_score", "vulnerability_score",
                              "capacity_score", "severity_index") ~ 5,
              Variable %in% ordered_vars ~ 3,
              TRUE ~ 1
            ),
            Color = dplyr::case_when(
              Variable == "overall_risk" ~ "lightblue",
              Variable == "severity_adjusted_risk" ~ "deepskyblue",
              Variable == "hazard_score" ~ "darkorange",
              Variable %in% c("All causes", cause_vars) ~
                scales::alpha("orange", 0.67),
              Variable == "vulnerability_score" ~ "darkblue",
              Variable %in% c(
                "infrastructure", "adult_literacy", "vulnerable_groups",
                "soc_econ_vulnerability") ~ scales::alpha("blue", 0.67),
              Variable == "capacity_score" ~ "darkgreen",
              Variable %in% c(
                "governance", "financing", "resources",
                "services") ~ scales::alpha("green", 0.67),
              Variable == "severity_index" ~ "purple",
              Variable %in% c(
                "crisis_impact", "people_conditions",
                "crisis_complexity") ~ scales::alpha("purple", 0.67),
              TRUE ~ "black"
            ),
            Variable = factor(Variable,
                              levels = rev(ordered_vars))  # reverse order
          )

        # Plot
        ggplot2::ggplot(df_long, ggplot2::aes(x = Value, y = Variable)) +
          ggplot2::geom_segment(ggplot2::aes(xend = Global_Avg, yend = Variable),
                                color = "grey") +
          ggplot2::geom_point(ggplot2::aes(x = Global_Avg), color = "grey",
                              size = 3) +
          ggplot2::geom_point(ggplot2::aes(color = Color, size = Size),
                              show.legend = FALSE) +
          ggplot2::scale_color_identity() +
          ggplot2::scale_size_identity() +
          ggplot2::labs(
            title = paste("Lollipop Plot for", input$country),
            x = "Value", y = NULL,
            caption = "Blue = Selected Country, Grey = Global Average"
          ) +
          ggplot2::theme_minimal() +
          ggplot2::theme(axis.text.y = ggplot2::element_text(size = 15))
      })

      output$riskHistogram <- shiny::renderPlot({
        selected_data <- raw_data |>
          dplyr::filter(country == input$country)

        plot_data <- raw_data |>
          tidyr::pivot_longer(cols = c(overall_risk, severity_adjusted_risk),
                              names_to = "risk_type",
                              values_to = "value")

        bin_breaks <- pretty(range(plot_data$value), n = 5)

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

        overall_y <- binned_data |>
          dplyr::filter(risk_type == "overall_risk",
                        bin == selected_overall_bin) |>
          dplyr::pull(count)

        severity_y <- binned_data |>
          dplyr::filter(risk_type == "severity_adjusted_risk",
                        bin == selected_severity_bin) |>
          dplyr::pull(count)

        ggplot2::ggplot(binned_data, ggplot2::aes(x = bin, y = count,
                                                  fill = risk_type)) +
          ggplot2::geom_col(width = 1, color = "white", alpha = 0.7) +
          ggplot2::geom_hline(yintercept = 0, color = "black") +
          ggplot2::geom_segment(ggplot2::aes(x = selected_overall_bin,
                                             xend = selected_overall_bin,
                                             y = 0, yend = overall_y),
                                color = "deeppink", linetype = "dashed",
                                linewidth = 1.2, inherit.aes = FALSE) +
          ggplot2::geom_segment(ggplot2::aes(x = selected_severity_bin,
                                             xend = selected_severity_bin,
                                             y = 0, yend = severity_y),
                                color = "blue", linetype = "dashed",
                                linewidth = 1.2, inherit.aes = FALSE) +
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
          dplyr::select(where(is.numeric)) |>
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
        drivers_value <- raw_data |>
          dplyr::filter(country == input$country) |>
          dplyr::pull(CRISIS) |>
          unique()

        # Check if Drivers is available
        if (is.na(drivers_value) || length(drivers_value) == 0) {
          # Only original data
          combined <- selected[, radar_vars]
          combined$Country <- "Original"


          combined |>
            dplyr::relocate(Country) |>
            ggradar::ggradar(grid.min = 0, grid.mid = 0.5, grid.max = 1,
                             values.radar = c("1%", "99%"),
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

          subtitle_text <- paste("\n Health risk corrected for", drivers_value)

          combined |>
            dplyr::relocate(Country) |>
            ggradar::ggradar(grid.min = 0, grid.mid = 0.5, grid.max = 1,
                             values.radar = c("0%", "20%", "40%", "80%", "100%"),
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

    }
  )
}
