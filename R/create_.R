## INTERACTIVE MAPS

#' Generate a Flexible Interactive Choropleth Map
#'
#' Creates an interactive choropleth map using `leaflet`, allowing dynamic
#' specification of the data source,
#' value column, color palette, and legend. It supports optional zooming to a
#' specific country and is designed
#' to generalize mapping logic across multiple datasets.
#'
#' @param data A data frame containing the values to be mapped. Must include a
#'  country code column.
#' @param join_key Column name in the world map data to join on (default is
#'  `"iso_a3"`).
#' @param data_key Column name in `data` to join with `join_key`.
#' @param value_column Column in `data` containing the values to visualize.
#' @param palette Name of the color palette to use (e.g., `"YlOrRd"`,
#'  `"viridis"`).
#' @param legend_title Title for the map legend.
#' @param label_prefix Prefix for the tooltip label (default is `"Value:"`).
#' @param zoom_to_country Optional country name to zoom into on the map.
#'
#' @return A `leaflet` map object.
#' @export
#'
#' @import leaflet
#' @importFrom dplyr left_join
#' @importFrom rnaturalearth ne_countries
#' @importFrom sf st_as_sf
#' @export
createmap <- function(data = NULL,
                      join_key = "iso_a3",
                      data_key = "REF_AREA",
                      value_column = "value",
                      palette = "YlOrRd",
                      legend_title = "Legend",
                      label_prefix = "Value:",
                      zoom_to_country = NULL) {
  # Load world map
  world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")

  # Join data to map
  map_data <- dplyr::left_join(world, data, by = setNames(data_key, "adm0_a3"))

  # Create base leaflet map
  leafmap <- leaflet::leaflet(map_data) |>
    leaflet::addTiles() |>
    leaflet::addPolygons(
      fillColor = ~leaflet::colorNumeric(
        palette, map_data[[value_column]],
        na.color = "#cccccc")(map_data[[value_column]]),
      weight = 1,
      opacity = 1,
      color = "white",
      dashArray = "3",
      fillOpacity = 0.7,
      highlight = leaflet::highlightOptions(
        weight = 2,
        color = "#666",
        dashArray = "",
        fillOpacity = 0.7,
        bringToFront = TRUE
      ),
      label = ~paste(name, "<br>", label_prefix, map_data[[value_column]])
    ) |>
    leaflet::addLegend(
      pal = leaflet::colorNumeric(palette, map_data[[value_column]]),
      values = map_data[[value_column]],
      title = legend_title,
      position = "bottomright"
    )

  # Optional zoom to country
  if (!is.null(zoom_to_country)) {
    selected_shape <- map_data |>
      dplyr::filter(country == zoom_to_country)
    # sf::st_union() # merge multiple geometries into one
    if (nrow(selected_shape) > 0) {
      bounds <- sf::st_bbox(selected_shape)
      leafmap <- leafmap |>
        leaflet::fitBounds(
          lng1 = unname(bounds["xmin"]),
          lat1 = unname(bounds["ymin"]),
          lng2 = unname(bounds["xmax"]),
          lat2 = unname(bounds["ymax"])
        )
    }
  }
  return(leafmap)
}


#' Interactive Map of Under-5 Mortality Rate
#'
#' Displays a choropleth map of average under-5 mortality rates by country using
#'  a yellow-red color scale.
#'
#' @param u5_mr A data frame with a `"Total"` column and ISO_A3-coded countries.
#' @return A `leaflet` map object.
#' @export
createmap_u5mr_avg <- function(u5_mr = NULL) {
  createmap(
    data = u5_mr,
    data_key = "REF_AREA",
    value_column = "Total",
    palette = "YlOrRd",
    legend_title = "U5 Mortality Rate",
    label_prefix = "Value:"
  )
}

#' Interactive Map of DALY Rate
#'
#' Displays a choropleth map of DALY rates for all causes using a blue color
#' scale.
#'
#' @param gbd_rates A data frame with an `"All causes"` column and ISO_A3-coded
#' countries.
#' @return A `leaflet` map object.
#' @export
createmap_DALY_avg <- function(gbd_rates = NULL) {
  createmap(
    data = gbd_rates,
    data_key = "ISO_A3",
    value_column = "All causes",
    palette = "YlGnBu",
    legend_title = "DALY Rate",
    label_prefix = "Value:"
  )
}

#' Interactive Map of Lack of Coping Capacity
#'
#' Displays a choropleth map of INFORM coping capacity scores using a green-blue
#'  color scale.
#'
#' @param inform_cap A data frame with a `"capacity"` column and ISO3-coded
#'  countries.
#' @return A `leaflet` map object.
#' @export
createmap_cap_avg <- function(inform_cap = NULL) {
  createmap(
    data = inform_cap,
    data_key = "iso3",
    value_column = "capacity",
    palette = "PuBuGn",
    legend_title = "INFORM Lack of Coping Capacity",
    label_prefix = "Value:"
  )
}

#' Interactive Map of U5MR Intra-country Differences
#'
#' Displays a choropleth map of intra-country differences in under-5 mortality
#'  rates using a viridis color scale.
#'
#' @param u5_mr A data frame with a `"did"` column and ISO_A3-coded countries.
#' @return A `leaflet` map object.
#' @export
createmap_u5mr_did <- function(u5_mr = NULL) {
  createmap(
    data = u5_mr,
    data_key = "REF_AREA",
    value_column = "did",
    palette = "viridis",
    legend_title = "U5 Mortality Rate Intra-country Difference",
    label_prefix = "Value:"
  )
}

#' Interactive Map of DALY Quantiles
#'
#' Displays a choropleth map of DALY quantile scores using a plasma color scale.
#'
#' @param gbd_rates A data frame with an `"All causes"` column and ISO_A3-coded
#'  countries.
#' @return A `leaflet` map object.
#' @export
createmap_DALY_score <- function(gbd_rates = NULL) {
  createmap(
    data = gbd_rates,
    data_key = "ISO_A3",
    value_column = "All causes",
    palette = "plasma",
    legend_title = "DALY Quantiles",
    label_prefix = "Value:"
  )
}

#' Interactive Map of Coping Capacity Score
#'
#' Displays a choropleth map of normalized coping capacity scores using a
#' green-blue color scale.
#'
#' @param risk_score A data frame with a `"capacity_score"` column and ISO3-coded
#'  countries.
#' @return A `leaflet` map object.
#' @export
createmap_cap_score <- function(risk_score = NULL) {
  createmap(
    data = risk_score,
    data_key = "ISO_A3",
    value_column = "capacity_score",
    palette = "PuBuGn",
    legend_title = "Lack of capacity Score",
    label_prefix = "Value:"
  )
}

#' Interactive Map of Vulnerability Score
#'
#' Displays a choropleth map of normalized vulnerability scores using a viridis
#'  color scale.
#'
#' @param risk_score A data frame with a `"vulnerability_score"` column and
#' ISO3-coded countries.
#' @return A `leaflet` map object.
#' @export
createmap_vul_score <- function(risk_score = NULL) {
  createmap(
    data = risk_score,
    data_key = "ISO_A3",
    value_column = "vulnerability_score",
    palette = "viridis",
    legend_title = "Vulnerability Score",
    label_prefix = "Value:"
  )
}

#' Interactive Map of Country Health Risk Score
#'
#' Displays a choropleth map of overall health risk scores using a
#' red-yellow-green color scale.
#' Optionally zooms to a selected country.
#'
#' @param db A data frame with an `"overall_risk"` column and ISO_A3-coded
#' countries.
#' @param zoom_to_country Optional country name to zoom into.
#' @return A `leaflet` map object.
#' @export
createmap_risk_score <- function(db = NULL,
                                 zoom_to_country = NULL) {
  createmap(
    data = db,
    data_key = "ISO_A3",
    value_column = "overall_risk",
    palette = rev(RColorBrewer::brewer.pal(11, "RdYlGn")),
    legend_title = "Country Health Risk Score",
    label_prefix = "Value:",
    zoom_to_country = zoom_to_country
  )
}

#' Interactive Map of Severity-adjusted Health Risk Score
#'
#' Displays a choropleth map of severity-adjusted health risk scores using a
#' red-yellow-green color scale.
#' Optionally zooms to a selected country.
#'
#' @param db A data frame with a `"severity_adjusted_risk"` column and
#' ISO_A3-coded countries.
#' @param zoom_to_country Optional country name to zoom into.
#' @return A `leaflet` map object.
#' @export
createmap_adjusted_risk_score <- function(db = NULL,
                                          zoom_to_country = NULL) {
  createmap(
    data = db,
    data_key = "ISO_A3",
    value_column = "severity_adjusted_risk",
    palette = rev(RColorBrewer::brewer.pal(11, "RdYlGn")),
    legend_title = "Country Health Risk Score",
    label_prefix = "Value:",
    zoom_to_country = zoom_to_country
  )
}

#' Interactive Map of Crisis Severity Index
#'
#' Displays a choropleth map of INFORM Severity Index scores using an accent
#' color scale.
#'
#' @param db A data frame with a `"severity_index"` column and ISO_A3-coded
#'  countries.
#' @return A `leaflet` map object.
#' @export
createmap_severity_score<- function(db = NULL) {
  createmap(
    data = db,
    data_key = "ISO_A3",
    value_column = "severity_index",
    palette = rev(RColorBrewer::brewer.pal(11, "Accent")),
    legend_title = "Country Health Risk Score",
    label_prefix = "Value:"
  )
}

## FIGURES

#' Create a Flexible Horizontal Bar and Point Plot
#'
#' Generates a horizontal bar/point plot using `ggplot2`, with optional
#' lineranges for uncertainty intervals.
#' Supports tidy evaluation for flexible column input and reordering of
#' the y-axis.
#'
#' @param data A data frame containing the plotting data.
#' @param x_var Unquoted column name for the x-axis.
#' @param y_var Unquoted column name for the y-axis (used for reordering).
#' @param color_var Unquoted column name for coloring the points.
#' @param x_label Label for the x-axis.
#' @param y_label Label for the y-axis.
#' @param linerange Optional list with `xmin` and `xmax` as symbols for
#' linerange plotting.
#'
#' @return A `ggplot` object representing the horizontal bar/point plot.
#' @export
createimg_bars <- function(data,
                           x_var,
                           y_var,
                           color_var,
                           x_label,
                           y_label,
                           linerange = NULL) {
  p <- ggplot2::ggplot(data, ggplot2::aes(y = reorder({{ y_var }}, {{ x_var }}),
                                          color = {{ color_var }}))

  if (!is.null(linerange)) {
    p <- p + ggplot2::geom_linerange(ggplot2::aes(xmin = !!linerange$xmin,
                                                  xmax = !!linerange$xmax))
  }

  p <- p +
    ggplot2::geom_point(ggplot2::aes(x = {{ x_var }})) +
    ggplot2::theme(
      legend.position = "none",
      axis.text.x = ggplot2::element_text(angle = 90, hjust = 1)
    ) +
    ggplot2::labs(x = x_label, y = y_label)

  return(p)
}

#' Under-5 Mortality Rate plot
#'
#' @param u5_mr Data with U5MR values
#' @return A ggplot object
#' @export
createimg_u5mr_bars <- function(u5_mr = NULL) {
  u5_mr <- dplyr::filter(u5_mr, if_all(everything(), ~ !is.na(.)))
  createimg_bars(
    data = u5_mr,
    x_var = Total,
    y_var = REF_AREA,
    color_var = delta,
    x_label = "Deaths per 1,000 live births",
    y_label = "Country",
    linerange =
      list(xmin = rlang::sym("Lowest"), xmax = rlang::sym("Highest"))
  )
}

#' DALY Rate plot
#'
#' @param gbd_rates Data with DALY values
#' @return A ggplot object
#' @export
createimg_daly_bars <- function(gbd_rates = NULL) {
  createimg_bars(
    data = gbd_rates,
    x_var =  `All causes`,
    y_var = location_name,
    color_var = "All causes",
    x_label = "DALY per 1000 people per year",
    y_label = "Country"
  )
}

#' Lack of Coping Capacity plot
#'
#' @param risk_score Data with capacity values
#' @return A ggplot object
#' @export
createimg_cap_bars <- function(risk_score = NULL) {
  createimg_bars(
    data = risk_score,
    x_var = capacity_score,
    y_var = country,
    color_var = capacity,
    x_label = "Lack of Capacity score",
    y_label = "Country"
  )
}

#' Vulnerability plot
#'
#' @param risk_score Data with vulnerability values
#' @return A ggplot object
#' @export
createimg_vul_bars <- function(risk_score = NULL) {
  createimg_bars(
    data = risk_score,
    x_var = vulnerability_score,
    y_var = country,
    color_var = vulnerability_score,
    x_label = "Vulnerability score",
    y_label = "Country"
  )
}

#' Risk Score plot
#'
#' @param risk_score Data with risk values
#' @return A ggplot object
#' @export
createimg_risk_bars <- function(risk_score = NULL) {
  createimg_bars(
    data = risk_score,
    x_var = overall_risk,
    y_var = country,
    color_var = overall_risk,
    x_label = "Health Risk Score",
    y_label = "Country"
  )
}

#' Create a Histogram of Health Risk Scores
#'
#' Generates a histogram of the overall health risk scores using ggplot2.
#' The histogram is colored by the 'risk' variable and hides the legend.
#'
#' @param risk_score A data frame containing a column named 'overall_risk' and
#'  optionally 'risk'.
#'
#' @return A ggplot2 histogram object.
#' @export
createimg_risk_hist <- function(risk_score = NULL) {
  risk_score |>
    ggplot2::ggplot() +
    ggplot2::geom_histogram(
      ggplot2::aes(overall_risk, color = risk)) +
    ggplot2::theme(
      legend.position = "none") +
    ggplot2::labs(x = "Health Risk Score")
}


#' Create an Interactive 3D Scatterplot of Risk Components
#'
#' Generates a 3D scatterplot using plotly to visualize three numeric dimensions
#' (e.g., capacity, vulnerability, hazard) and color points by a fourth variable.
#'
#' @param db A data frame containing the data to plot.
#' @param x_var Name of the column to use for the x-axis (string).
#' @param y_var Name of the column to use for the y-axis (string).
#' @param z_var Name of the column to use for the z-axis (string).
#' @param color_var Name of the column to use for coloring the points (string).
#' @param label_var Name of the column to use for hover labels (string).
#' @param labels A character vector of length 3 with axis labels: c(x_label,
#' y_label, z_label).
#'
#' @return A plotly 3D scatterplot object.
#' @export
create_3d_plot <- function(db,
                           x_var,
                           y_var,
                           z_var,
                           color_var,
                           label_var,
                           labels = c("X", "Y", "Z")) {
  plotly::plot_ly(
    data = db,
    x = as.formula(paste0("~", x_var)),
    y = as.formula(paste0("~", y_var)),
    z = as.formula(paste0("~", z_var)),
    type = "scatter3d",
    mode = "markers",
    text = as.formula(paste0("~", label_var)),
    hoverinfo = "text",
    marker = list(
      size = 4,
      color = db[[color_var]],
      colorscale = "Magma",
      reversescale = FALSE,
      colorbar = list(title = color_var),
      showscale = TRUE
    )
  ) |>
    plotly::layout(
      scene = list(
        xaxis = list(title = labels[1]),
        yaxis = list(title = labels[2]),
        zaxis = list(title = labels[3])
      ),
      title = "Scatterplot of Country Health Risk Components"
    )
}

#' Create a Lollipop Plot Comparing Two Numeric Columns
#'
#' Generates a lollipop plot comparing two numeric columns for each country.
#' Optionally sorts countries by the absolute difference between the two values.
#'
#' @param df A data frame containing the data to plot.
#' @param country_col Name of the column with country names (string).
#' @param x_col Name of the first numeric column (string).
#' @param y_col Name of the second numeric column (string).
#' @param title Title of the plot.
#' @param sort_by_difference Logical. If TRUE, sorts countries by descending
#' difference.
#' @param color_x Color for the x_col points.
#' @param color_y Color for the y_col points.
#'
#' @return A ggplot2 lollipop plot.
#' @export
create_lollipop <- function(df, country_col, x_col, y_col, title,
                            sort_by_difference = TRUE,
                            color_x = "blue", color_y = "red") {
  df <- df |>
    dplyr::mutate(difference = abs(.data[[x_col]] - .data[[y_col]]))

  reorder_col <- if (sort_by_difference) {
    df[[country_col]] <- reorder(df[[country_col]], df$difference)
    df[[country_col]]
  } else {
    df[[country_col]] <- reorder(df[[country_col]], df[[x_col]])
    df[[country_col]]
  }

  ggplot2::ggplot(df, ggplot2::aes(x = .data[[country_col]])) +
    ggplot2::geom_segment(ggplot2::aes(
      xend = .data[[country_col]],
      y = .data[[x_col]], yend = .data[[y_col]])) +
    ggplot2::geom_point(ggplot2::aes(
      y = .data[[x_col]]), size = 3, color = color_x) +
    ggplot2::geom_point(ggplot2::aes(
      y = .data[[y_col]]), size = 3, color = color_y) +
    ggplot2::labs(title = title, x = "Country", y = "Value") +
    ggplot2::theme_minimal() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
}

#' Lollipop Plot Comparing Overall Risk and Severity-Adjusted Risk
#'
#' Creates a lollipop plot comparing overall risk and severity-adjusted risk
#' scores
#' for each country.
#'
#' @param risk_score A data frame with columns ISO_A3, country, overall_risk,
#' and severity_adjusted_risk.
#' @param sort_by_difference Logical. Whether to sort by difference.
#' @param color_x Color for overall_risk points.
#' @param color_y Color for severity-adjusted risk points.
#'
#' @return A ggplot2 lollipop plot.
#' @export
create_lollipop_plot_shift <- function(risk_score,
                                  sort_by_difference = FALSE,
                                  color_x = "lightblue",
                                  color_y = "deepskyblue") {
  df <- dplyr::select(risk_score, ISO_A3, country,
                      overall_risk, severity_adjusted_risk)

  create_lollipop(df,
                  country_col = "country",
                  x_col = "overall_risk",
                  y_col = "severity_adjusted_risk",
                  title = "Difference between Overall Risk and Severity-adjusted Risk",
                  sort_by_difference = sort_by_difference,
                  color_x = color_x,
                  color_y = color_y)
}

#' Correlation Matrix with Group Highlights
#'
#' Computes and visualizes a Pearson correlation matrix for selected variables,
#' with colored boxes highlighting conceptual groups (e.g., Health,
#' Vulnerability).
#'
#' @param df A data frame containing the variables to correlate.
#'
#' @return A ggplot2 heatmap of the correlation matrix with group annotations.
#' @export
create_correlation_matrix_with_groups <- function(df) {

  variables <- c(
    "overall_risk", "severity_adjusted_risk", "hazard_score",
    "Mental disorders", "Sexually transmitted infections",
    "Respiratory infections and tuberculosis", "Enteric infections",
    "All causes", "Neglected tropical diseases and malaria",
    "Cardiovascular diseases", "Transport injuries", "Other injuries",
    "Violence injuries", "Other NCDs", "vulnerability_score",
    "infrastructure", "adult_literacy", "vulnerable_groups",
    "soc_econ_vulnerability",
    "capacity_score", "governance", "financing", "resources", "services",
    "severity_index", "crisis_impact", "people_conditions", "crisis_complexity",
    "severity_adjusted_risk_delta"
  )

  group_list <- list(
    Health = c("hazard_score", "Mental disorders",
               "Sexually transmitted infections",
               "Respiratory infections and tuberculosis", "Enteric infections",
               "All causes", "Neglected tropical diseases and malaria",
               "Cardiovascular diseases", "Transport injuries", "Other injuries",
               "Violence injuries", "Other NCDs"),
    Vulnerability = c("vulnerability_score", "infrastructure", "adult_literacy",
                      "vulnerable_groups", "soc_econ_vulnerability"),
    Capacity = c("capacity_score", "governance", "financing", "resources",
                 "services"),
    Crisis = c("severity_index", "crisis_impact", "people_conditions",
               "crisis_complexity", "severity_adjusted_risk_delta")
  )

  group_colors <- c(
    Health = "darkorange",
    Vulnerability = "darkblue",
    Capacity = "darkgreen",
    Crisis = "purple"
  )

  # Not every diagnostic exists on every run; drop absent ones rather than
  # failing the whole report.
  absent <- setdiff(variables, names(df))
  if (length(absent) > 0) {
    warning("create_correlation_matrix_with_groups(): skipping absent ",
            "variables: ", paste(absent, collapse = ", "), call. = FALSE)
    variables <- intersect(variables, names(df))
    group_list <- lapply(group_list, function(g) intersect(g, variables))
  }

  corr_matrix <- df |>
    dplyr::select(dplyr::all_of(variables)) |>
    stats::cor(method = "pearson", use = "pairwise.complete.obs") |>
    as.data.frame() |>
    tibble::rownames_to_column("Var1") |>
    tidyr::pivot_longer(-Var1, names_to = "Var2", values_to = "correlation")

  # Factor levels to preserve order
  corr_matrix$Var1 <- factor(corr_matrix$Var1, levels = variables)
  corr_matrix$Var2 <- factor(corr_matrix$Var2, levels = variables)

  p <- ggplot2::ggplot(corr_matrix, ggplot2::aes(x = Var1, y = Var2,
                                                 fill = correlation)) +
    ggplot2::geom_tile(color = "grey90") +
    ggplot2::geom_text(ggplot2::aes(label = sprintf("%.2f", correlation),
                                    alpha = abs(correlation)), size = 3) +
    ggplot2::scale_fill_gradientn(
      colours = c("red", "white", "white", "white", "red"),
      values = scales::rescale(c(-1, -0.5, -0.2, 0.2, 0.5, 1)),
      limits = c(-1, 1),
      name = "Pearson\nCorrelation"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(
      angle = 45, hjust = 1, size = 14),
                   axis.text.y = ggplot2::element_text(size = 14),
                   panel.grid = ggplot2::element_blank()) +
    ggplot2::labs(title = "Pairwise Pearson Correlation Matrix",
                  x = NULL, y = NULL)

  for (group_name in names(group_list)) {
    group_vars <- group_list[[group_name]]
    color <- group_colors[[group_name]]
    indices <- which(variables %in% group_vars)
    if (length(indices) > 0) {
      min_idx <- min(indices)
      max_idx <- max(indices)
      p <- p +
        ggplot2::annotate("rect",
                          xmin = min_idx - 0.5, xmax = max_idx + 0.5,
                          ymin = min_idx - 0.5, ymax = max_idx + 0.5,
                          color = color, fill = NA, size = 1)
    }
  }

  return(p)
}

#' Create a stacked bar chart for a single disease using long-format data
#'
#' @param long The full long-format tibble from `pivot_disease_data()`.
#' @param disease_name Name of the disease to plot.
#' @param concern_levels Ordered vector of concern levels.
#'
#' @return A ggplot object.
#' @export
create_disease_plot_from_long <- function(
    long,
    disease_name,
    concern_levels = c(
      "Very unconcerned",
      "Somewhat unconcerned",
      "Neither concerned nor unconcerned",
      "Somewhat concerned",
      "Very concerned"
    )
) {
  question_labels <- c(
    "15m diagnosis delay", "1h diagnosis delay", "4h diagnosis delay", "8h diagnosis delay",
    "1h transport delay", "4h transport delay", "8h transport delay", "1 day transport delay",
    "Patient life impact", "Patient wellbeing impact"
  )

  # Define full question levels including spacers
  question_levels <- c(
    question_labels[1:4], "spacer1",
    question_labels[5:8], "spacer2",
    question_labels[9:10]
  )

  custom_labels <- c(
    "15m diagnosis delay", "1h diagnosis delay", "4h diagnosis delay", "8h diagnosis delay",
    "",  # spacer1
    "1h transport delay", "4h transport delay", "8h transport delay", "1 day transport delay",
    "",  # spacer2
    "Patient life impact", "Patient wellbeing impact"
  )

  # Filter for selected disease
  filtered <- long |>
    dplyr::filter(disease == disease_name) |>
    dplyr::mutate(
      question = factor(question, levels = question_labels, ordered = TRUE),
      response = factor(response, levels = concern_levels, ordered = TRUE)
    )

  # Add spacer rows for each concern level with prop = 0
  spacer_rows <- purrr::map_dfr(c("spacer1", "spacer2"), function(spacer) {
    tibble::tibble(
      disease = disease_name,
      question = factor(spacer, levels = question_levels, ordered = TRUE),
      response = factor(concern_levels, levels = concern_levels, ordered = TRUE),
      n = 0,
      prop = 0
    )
  })

  # Combine and relevel questions
  plot_data <- filtered |>
    dplyr::mutate(question = factor(question, levels = question_levels, ordered = TRUE)) |>
    dplyr::bind_rows(spacer_rows)

  ggplot2::ggplot(plot_data, ggplot2::aes(x = question, y = prop, fill = response)) +
    ggplot2::geom_col(
      width = 0.5,
      linewidth = 0.2,
      position = ggplot2::position_fill(reverse = TRUE)
    ) +
    viridis::scale_fill_viridis(
      discrete = TRUE,
      name = "Concern level"
    ) +
    ggplot2::geom_hline(yintercept = 0.33, linetype="dashed", color = "blue") +
    ggplot2::scale_x_discrete(labels = custom_labels) +
    ggplot2::scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
    ggplot2::labs(
      x = NULL,
      y = "Proportion of responses",
      title = disease_name,
      subtitle = "Perceived concern levels across diagnostic, transport, and impact delays"
    ) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      panel.grid.major.x = ggplot2::element_blank(),
      panel.grid.minor.x = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_text(angle = 20, hjust = 1),
      legend.position = "right"
    ) +
    ggplot2::guides(fill = ggplot2::guide_legend(reverse = TRUE))
}

#' Plot all diseases using long-format data
#'
#' @param long The full long-format tibble from `pivot_disease_data()`.
#' @param concern_levels Ordered vector of concern levels.
#'
#' @return A named list of ggplot objects.
#' @export
plot_all_diseases_from_long <- function(
    long,
    concern_levels = c(
      "Very unconcerned",
      "Somewhat unconcerned",
      "Neither concerned nor unconcerned",
      "Somewhat concerned",
      "Very concerned"
    )
) {
  diseases <- unique(long$disease)

  plots <- purrr::map(diseases, function(disease_name) {
    create_disease_plot_from_long(long, disease_name, concern_levels)
  })

  names(plots) <- diseases
  return(plots)
}

#' Create a bar chart for a single question across diseases
#'
#' @description
#' Visualizes concern levels for a specific question across all diseases.
#' Diseases are ordered left-to-right by proportion of "Very concerned" responses (descending).
#'
#' @param long The full long-format tibble from `pivot_disease_data()`.
#' @param question_index Integer from 1 to 10 (position in each block).
#' @param concern_levels Ordered vector of concern levels.
#' @param question_labels Vector of question titles (length = 10).
#'
#' @return A ggplot object.
#' @export
create_question_plot_from_long <- function(
    long,
    question_index,
    concern_levels = c(
      "Very unconcerned",
      "Somewhat unconcerned",
      "Neither concerned nor unconcerned",
      "Somewhat concerned",
      "Very concerned"
    ),
    question_labels = c(
      "15m diagnosis delay", "1h diagnosis delay", "4h diagnosis delay", "8h diagnosis delay",
      "1h transport delay", "4h transport delay", "8h transport delay", "1 day transport delay",
      "Patient life impact", "Patient wellbeing impact"
    )
) {
  stopifnot(question_index >= 1 && question_index <= length(question_labels))

  question_name <- question_labels[question_index]

  filtered <- long |>
    dplyr::filter(question == question_name, disease != "Outbreak concern")

  disease_order <- filtered |>
    dplyr::filter(response == "Very concerned") |>
    dplyr::group_by(disease) |>
    dplyr::summarise(vc_prop = sum(prop), .groups = "drop") |>
    dplyr::arrange(desc(vc_prop)) |>
    dplyr::pull(disease)

  filtered <- filtered |>
    dplyr::mutate(
      disease = factor(disease, levels = disease_order),
      response = factor(response, levels = concern_levels, ordered = TRUE)
    )

  ggplot2::ggplot(filtered, ggplot2::aes(x = disease, y = prop, fill = response)) +
    ggplot2::geom_col(position = ggplot2::position_fill(reverse = TRUE), width = 0.8) +
    viridis::scale_fill_viridis(discrete = TRUE, name = "Concern level") +
    ggplot2::geom_hline(yintercept = 0.33, linetype = "dashed", color = "blue") +
    ggplot2::labs(
      x = NULL,
      y = "Proportion of responses",
      title = question_name,
      subtitle = "Concern levels across diseases"
    ) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 30, hjust = 1),
      legend.position = "right"
    ) +
    ggplot2::guides(fill = ggplot2::guide_legend(reverse = TRUE))
}

#' Plot all 10 questions across diseases using combined long-format data
#'
#' @param long A combined tibble from multiple diseases (output of `pivot_disease_data()`),
#'             with an added `disease` column.
#' @param concern_levels Ordered vector of concern levels.
#' @param question_labels Vector of question titles (length = 10).
#'
#' @return A named list of ggplot objects.
#' @export
plot_all_questions_from_long <- function(
    long,
    concern_levels = c(
      "Very unconcerned",
      "Somewhat unconcerned",
      "Neither concerned nor unconcerned",
      "Somewhat concerned",
      "Very concerned"
    ),
    question_labels = c(
      "15m diagnosis delay", "1h diagnosis delay", "4h diagnosis delay", "8h diagnosis delay",
      "1h transport delay", "4h transport delay", "8h transport delay", "1 day transport delay",
      "Patient life impact", "Patient wellbeing impact"
    )
) {
  plots <- purrr::map(1:10, function(i) {
    create_question_plot_from_long(
      long = long,
      question_index = i,
      concern_levels = concern_levels,
      question_labels = question_labels
    )
  })

  names(plots) <- question_labels
  return(plots)
}

#' Plot a single stacked bar chart for outbreak concern questions
#'
#' @description
#' Visualizes concern levels across all outbreak-related questions in one plot.
#' Each question is a separate bar, filled by proportion of concern levels.
#'
#' @param long The full long-format tibble from `pivot_disease_data()`.
#' @param concern_levels Ordered vector of concern levels.
#' @param outbreak_labels Vector of cleaned outbreak question labels (length = 11).
#'
#' @return A ggplot object.
#' @export
create_outbreak_concern <- function(
    long,
    concern_levels = c(
      "Very unconcerned",
      "Somewhat unconcerned",
      "Neither concerned nor unconcerned",
      "Somewhat concerned",
      "Very concerned"
    ),
    outbreak_labels = c(
      "Food/Waterborne Disease Concern",
      "Food/Waterborne Outbreak Risk",
      "STDs Outbreak Concern",
      "STDs Transmission Risk",
      "Hemorrhagic Fever Outbreak Concern",
      "Hemorrhagic Fever Risk",
      "Zoonoses Concern",
      "Vector-borne Disease Outbreak",
      "Vector-borne Disease Risk",
      "Airborne Disease Outbreak",
      "Airborne Disease Risk"
    )
) {
  long |>
    dplyr::filter(disease == "Outbreak concern") |>
    dplyr::mutate(
      question = factor(question, levels = outbreak_labels, ordered = TRUE),
      response = factor(response, levels = concern_levels, ordered = TRUE)
    ) |>
    ggplot2::ggplot(ggplot2::aes(x = question, y = prop, fill = response)) +
    ggplot2::geom_col(position = ggplot2::position_fill(reverse = TRUE), width = 0.8) +
    viridis::scale_fill_viridis(discrete = TRUE, name = "Concern level") +
    ggplot2::geom_hline(yintercept = 0.33, linetype = "dashed", color = "blue") +
    ggplot2::labs(
      x = NULL,
      y = "Proportion of responses",
      title = "Concern Levels for Outbreak-Related Questions",
      subtitle = "Stacked proportions across all outbreak categories"
    ) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 30, hjust = 1),
      legend.position = "right"
    ) +
    ggplot2::guides(fill = ggplot2::guide_legend(reverse = TRUE))
}

#' Create a 3D scatterplot of concern levels by disease
#'
#' This function filters a tidyverse-style dataset to extract, for each disease,
#' the proportion of "Very concerned" responses to three specific questions:
#' "4h diagnosis delay", "4h transport delay", and "Patient life impact".
#' It returns an interactive 3D scatterplot created with plotly.
#'
#' @param data A tibble with columns: disease, question, response, n, prop
#'
#' @return A plotly object containing the 3D scatterplot
#' @export
#'
#' @examples
#' # Example data for Acute Abdominal Pain
#' ong <- tibble::tibble(
#'   disease = rep("Acute Abdominal Pain", 3),
#'   question = c(
#'     "4h diagnosis delay",
#'     "4h transport delay",
#'     "Patient wellbeing impact"
#'   ),
#'   response = rep("Very concerned", 3),
#'   n = c(25, 40, 46),
#'   prop = c(0.3521, 0.5634, 0.6479)
#' )
#'
#' create_disease_scatter(ong)
create_disease_scatter <- function(data) {
  data |>
    dplyr::filter(
      question %in% c("4h diagnosis delay", "4h transport delay", "Patient wellbeing impact"),
      response == "Very concerned"
    ) |>
    dplyr::select(disease, question, prop) |>
    tidyr::pivot_wider(
      names_from = question,
      values_from = prop,
      names_prefix = "",
    ) |>
    dplyr::rename(
      diagnosis_delay_4h = `4h diagnosis delay`,
      transport_delay_4h = `4h transport delay`,
      patient_impact = `Patient wellbeing impact`
    ) |>
    plotly::plot_ly(
      x = ~diagnosis_delay_4h,
      y = ~transport_delay_4h,
      z = ~patient_impact,
      type = "scatter3d",
      mode = "markers",
      text = ~disease,
      hoverinfo = "text",
      marker = list(size = 5, color = ~patient_impact, colorscale = "Magma", showscale = TRUE)
    ) |>
    plotly::layout(
      scene = list(
        xaxis = list(title = "4h Diagnosis Delay"),
        yaxis = list(title = "4h Transport Delay"),
        zaxis = list(title = "Patient wellbeing Impact")
      ),
      title = "Preoccupazione per malattia (Very concerned)"
    )
}



#' Interactive Map of Data Completeness
#'
#' Displays the proportion of underlying indicators actually present for each
#' country. Published alongside the risk map so that a confident-looking score
#' built on a thin evidence base is visible rather than implicit.
#'
#' @param db A data frame with a `data_completeness` column and ISO_A3-coded
#'   country identifiers.
#'
#' @return A leaflet map object.
#' @export
createmap_completeness <- function(db = NULL) {
  createmap(
    data = db,
    value_column = "data_completeness",
    palette = "RdYlGn",
    legend_title = "Share of indicators present"
  )
}

#' Bar Chart of the Severity Adjustment by Country
#'
#' Shows, for the most-affected countries, the unadjusted risk and the uplift
#' contributed by the crisis modifier, so the size of the adjustment is legible
#' rather than buried inside the headline score.
#'
#' @param db A data frame produced by `add_severity()`.
#' @param top_n Number of countries to display.
#'
#' @return A ggplot2 object.
#' @export
createimg_severity_uplift <- function(db = NULL, top_n = 30) {
  d <- db[!is.na(db$severity_uplift) & db$severity_uplift > 0, , drop = FALSE]
  d <- d[order(-d$severity_uplift), , drop = FALSE]
  d <- utils::head(d, top_n)

  if (nrow(d) == 0) {
    return(
      ggplot2::ggplot() +
        ggplot2::annotate("text", x = 0, y = 0,
                          label = "No country received a severity adjustment.",
                          size = 5, colour = "grey30") +
        ggplot2::theme_void()
    )
  }

  long <- tidyr::pivot_longer(
    d[, c("country", "overall_risk", "severity_uplift")],
    cols = c("overall_risk", "severity_uplift"),
    names_to = "part", values_to = "value"
  )
  long$part <- factor(long$part,
                      levels = c("overall_risk", "severity_uplift"),
                      labels = c("Structural risk", "Crisis uplift"))
  long$country <- factor(long$country, levels = rev(d$country))

  ggplot2::ggplot(long, ggplot2::aes(x = .data$value, y = .data$country,
                                     fill = .data$part)) +
    ggplot2::geom_col() +
    ggplot2::scale_fill_manual(
      values = c("Structural risk" = "grey60", "Crisis uplift" = "firebrick")
    ) +
    ggplot2::labs(
      title = "How much of the score is the crisis modifier?",
      subtitle = paste0("Top ", nrow(d),
                        " countries by severity uplift"),
      x = "Severity-adjusted risk", y = NULL, fill = NULL
    ) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(legend.position = "top")
}

#' Plot the severity calibration table
#'
#' Visualises how the crisis modifier behaves across baseline risks and severity
#' levels under v4.1 compared with the deprecated v4.0 formula. Included in the
#' report so that readers can see the size of the lever rather than take it on
#' trust.
#'
#' @param calibration A data frame from `severity_calibration_table()`.
#' @param n_drivers Driver count to display.
#'
#' @return A ggplot2 object.
#' @export
create_severity_calibration_plot <- function(calibration, n_drivers = 2) {
  d <- calibration[calibration$n_drivers == n_drivers, , drop = FALSE]

  long <- tidyr::pivot_longer(
    d[, c("baseline", "severity", "risk_v4_1", "risk_v4_0")],
    cols = c("risk_v4_1", "risk_v4_0"),
    names_to = "method", values_to = "risk"
  )
  long$method <- factor(long$method,
                        levels = c("risk_v4_0", "risk_v4_1"),
                        labels = c("v4.0 (convex pull)",
                                   "v4.1 (driver-routed log-odds)"))
  long$baseline_lab <- paste0("Baseline risk = ",
                              sprintf("%.2f", long$baseline))

  ggplot2::ggplot(long, ggplot2::aes(x = .data$severity, y = .data$risk,
                                     colour = .data$method)) +
    ggplot2::geom_line(linewidth = 0.9) +
    ggplot2::geom_point(size = 1.8) +
    ggplot2::geom_hline(yintercept = 1, linetype = "dotted",
                        colour = "grey50") +
    ggplot2::facet_wrap(~ .data$baseline_lab) +
    ggplot2::scale_colour_manual(
      values = c("v4.0 (convex pull)" = "firebrick",
                 "v4.1 (driver-routed log-odds)" = "steelblue")
    ) +
    ggplot2::coord_cartesian(ylim = c(0, 1.02)) +
    ggplot2::labs(
      title = "Effect of the crisis modifier on the risk score",
      subtitle = paste0(
        "At ", n_drivers, " active drivers per channel. v4.0 reaches 1.0 at ",
        "severity 5 regardless of the structural score."
      ),
      x = "INFORM Severity index", y = "Adjusted risk", colour = NULL
    ) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(legend.position = "top")
}
