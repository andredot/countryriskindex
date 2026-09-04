## INDEX VALIDATION -------------------------------------------------------
##
## The correlation matrix answers "do these indicators move together?". It does
## not answer "how many independent things is this index actually measuring?".
## A composite built from three pillars that are largely collinear has fewer
## effective dimensions than its structure implies, and the implicit equal
## weighting then over-weights whatever latent factor the pillars share.
##
## These functions add a PCA-based dimensionality check alongside the existing
## correlation analysis, so that both questions are answered and reported.

#' Variables entered into the PCA validation
#'
#' @return A named list with `components` (the three headline component scores)
#'   and `indicators` (the sub-component scores that build them).
#' @export
default_pca_vars <- function() {
  list(
    components = c("hazard_score", "vulnerability_score", "capacity_score"),
    indicators = c(
      "hazard_score",
      "infrastructure", "adult_literacy", "vulnerable_groups",
      "soc_econ_vulnerability",
      "governance", "financing", "resources", "services"
    )
  )
}

#' Run a PCA on the index components
#'
#' Performs a principal component analysis on the scaled, complete-case subset
#' of `vars`, and summarises how much of the total variance the leading
#' components explain.
#'
#' @param df A data frame containing the variables in `vars`.
#' @param vars Character vector of variable names. Defaults to the sub-component
#'   indicators from [default_pca_vars()].
#' @param scale. Logical: scale variables to unit variance before analysis.
#'   Should normally stay `TRUE` since the inputs are on different scales.
#'
#' @return A list with elements `pca` (the `prcomp` object), `variance` (a data
#'   frame of eigenvalues and explained variance), `loadings` (a long data frame
#'   of variable loadings), `scores` (observation scores with country labels),
#'   `n_complete` and `vars`.
#' @export
run_pca_validation <- function(df,
                               vars = default_pca_vars()$indicators,
                               scale. = TRUE) {

  vars <- intersect(vars, names(df))
  if (length(vars) < 2) {
    stop("run_pca_validation(): need at least two variables present in `df`.",
         call. = FALSE)
  }

  mat <- as.matrix(df[, vars, drop = FALSE])
  storage.mode(mat) <- "double"
  keep <- stats::complete.cases(mat)

  if (sum(keep) < length(vars) + 1) {
    stop("run_pca_validation(): too few complete cases (", sum(keep), ").",
         call. = FALSE)
  }

  # Drop zero-variance columns, which prcomp cannot scale
  sds <- apply(mat[keep, , drop = FALSE], 2, stats::sd)
  constant <- names(sds)[!is.finite(sds) | sds == 0]
  if (length(constant) > 0) {
    warning("run_pca_validation(): dropping zero-variance variables: ",
            paste(constant, collapse = ", "), call. = FALSE)
    vars <- setdiff(vars, constant)
    mat <- mat[, vars, drop = FALSE]
  }

  pca <- stats::prcomp(mat[keep, , drop = FALSE], center = TRUE,
                       scale. = scale.)

  eigenvalues <- pca$sdev^2
  variance <- data.frame(
    pc = paste0("PC", seq_along(eigenvalues)),
    pc_number = seq_along(eigenvalues),
    eigenvalue = eigenvalues,
    prop_variance = eigenvalues / sum(eigenvalues),
    cum_variance = cumsum(eigenvalues) / sum(eigenvalues),
    stringsAsFactors = FALSE
  )

  loadings <- as.data.frame(pca$rotation)
  loadings$variable <- rownames(pca$rotation)
  loadings <- tidyr::pivot_longer(
    loadings,
    cols = -"variable",
    names_to = "pc",
    values_to = "loading"
  )

  scores <- as.data.frame(pca$x)
  label_col <- if ("country" %in% names(df)) "country" else NULL
  if (!is.null(label_col)) {
    scores$country <- df[[label_col]][keep]
  }
  if ("ISO_A3" %in% names(df)) {
    scores$ISO_A3 <- df[["ISO_A3"]][keep]
  }

  list(
    pca = pca,
    variance = variance,
    loadings = loadings,
    scores = scores,
    n_complete = sum(keep),
    vars = vars
  )
}

#' Summarise the effective dimensionality of the index
#'
#' Reports how many principal components are needed to reach given variance
#' thresholds, and the Kaiser criterion count (eigenvalue > 1). If one component
#' explains most of the variance, the three-pillar structure is largely
#' cosmetic and the weighting deserves reconsideration.
#'
#' @param pca_result Output of [run_pca_validation()].
#' @param thresholds Numeric vector of cumulative variance thresholds.
#'
#' @return A data frame with one row per threshold plus a Kaiser row.
#' @export
summarise_dimensionality <- function(pca_result, thresholds = c(0.8, 0.9)) {
  v <- pca_result$variance

  rows <- lapply(thresholds, function(t) {
    data.frame(
      criterion = paste0(round(t * 100), "% cumulative variance"),
      n_components = min(which(v$cum_variance >= t)),
      stringsAsFactors = FALSE
    )
  })

  rows[[length(rows) + 1]] <- data.frame(
    criterion = "Kaiser (eigenvalue > 1)",
    n_components = sum(v$eigenvalue > 1),
    stringsAsFactors = FALSE
  )

  out <- do.call(rbind, rows)
  out$n_variables <- length(pca_result$vars)
  out$pc1_share <- round(v$prop_variance[1], 3)
  out
}

#' Scree plot of explained variance
#'
#' @param pca_result Output of [run_pca_validation()].
#' @param max_pc Maximum number of components to display.
#'
#' @return A ggplot2 object.
#' @export
create_pca_scree <- function(pca_result, max_pc = 10) {
  v <- utils::head(pca_result$variance, max_pc)

  ggplot2::ggplot(v, ggplot2::aes(x = .data$pc_number)) +
    ggplot2::geom_col(ggplot2::aes(y = .data$prop_variance),
                      fill = "steelblue", alpha = 0.85) +
    ggplot2::geom_line(ggplot2::aes(y = .data$cum_variance),
                       colour = "darkred", linewidth = 0.8) +
    ggplot2::geom_point(ggplot2::aes(y = .data$cum_variance),
                        colour = "darkred", size = 2) +
    ggplot2::geom_hline(yintercept = 0.8, linetype = "dashed",
                        colour = "grey40") +
    ggplot2::scale_x_continuous(breaks = v$pc_number,
                                labels = v$pc) +
    ggplot2::scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
    ggplot2::labs(
      title = "Explained variance by principal component",
      subtitle = paste0(
        "Bars: individual share. Line: cumulative. Dashed line: 80%. ",
        "n = ", pca_result$n_complete, " countries"
      ),
      x = NULL, y = "Share of total variance"
    ) +
    ggplot2::theme_minimal(base_size = 12)
}

#' Loadings heatmap for the leading principal components
#'
#' @param pca_result Output of [run_pca_validation()].
#' @param n_pc Number of components to display.
#'
#' @return A ggplot2 object.
#' @export
create_pca_loadings <- function(pca_result, n_pc = 4) {
  keep_pcs <- paste0("PC", seq_len(min(n_pc, nrow(pca_result$variance))))
  d <- pca_result$loadings[pca_result$loadings$pc %in% keep_pcs, ]
  d$pc <- factor(d$pc, levels = keep_pcs)

  ggplot2::ggplot(d, ggplot2::aes(x = .data$pc, y = .data$variable,
                                  fill = .data$loading)) +
    ggplot2::geom_tile(colour = "grey90") +
    ggplot2::geom_text(ggplot2::aes(label = sprintf("%.2f", .data$loading)),
                       size = 3) +
    ggplot2::scale_fill_gradient2(low = "#2166ac", mid = "white",
                                  high = "#b2182b", midpoint = 0,
                                  limits = c(-1, 1)) +
    ggplot2::labs(
      title = "Principal component loadings",
      subtitle = "Which indicators drive each independent dimension",
      x = NULL, y = NULL, fill = "Loading"
    ) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(panel.grid = ggplot2::element_blank())
}

#' Biplot of countries in the first two principal components
#'
#' @param pca_result Output of [run_pca_validation()].
#' @param colour_by Optional column in `pca_result$scores` used to colour points.
#' @param label_n Number of most extreme countries to label on each axis.
#'
#' @return A ggplot2 object.
#' @export
create_pca_biplot <- function(pca_result, colour_by = NULL, label_n = 8) {
  scores <- pca_result$scores
  v <- pca_result$variance

  arrows <- as.data.frame(pca_result$pca$rotation[, 1:2, drop = FALSE])
  arrows$variable <- rownames(pca_result$pca$rotation)
  scale_factor <- max(abs(scores$PC1), abs(scores$PC2), na.rm = TRUE) * 0.8
  arrows$PC1 <- arrows$PC1 * scale_factor
  arrows$PC2 <- arrows$PC2 * scale_factor

  p <- ggplot2::ggplot(scores, ggplot2::aes(x = .data$PC1, y = .data$PC2))

  if (!is.null(colour_by) && colour_by %in% names(scores)) {
    p <- p + ggplot2::geom_point(
      ggplot2::aes(colour = .data[[colour_by]]), alpha = 0.7, size = 2
    ) +
      ggplot2::scale_colour_viridis_c(option = "plasma")
  } else {
    p <- p + ggplot2::geom_point(alpha = 0.6, size = 2, colour = "grey30")
  }

  p <- p +
    ggplot2::geom_segment(
      data = arrows,
      ggplot2::aes(x = 0, y = 0, xend = .data$PC1, yend = .data$PC2),
      arrow = ggplot2::arrow(length = ggplot2::unit(0.2, "cm")),
      colour = "darkred", alpha = 0.8, inherit.aes = FALSE
    ) +
    ggplot2::geom_text(
      data = arrows,
      ggplot2::aes(x = .data$PC1, y = .data$PC2, label = .data$variable),
      colour = "darkred", size = 3, vjust = -0.4, inherit.aes = FALSE
    )

  if ("country" %in% names(scores) && label_n > 0) {
    extremes <- scores[order(-(scores$PC1^2 + scores$PC2^2)), ]
    extremes <- utils::head(extremes, label_n)
    p <- p + ggplot2::geom_text(
      data = extremes,
      ggplot2::aes(label = .data$country),
      size = 3, vjust = 1.5, colour = "grey20"
    )
  }

  p +
    ggplot2::labs(
      title = "Countries in the first two principal components",
      subtitle = sprintf(
        "PC1: %.0f%% of variance, PC2: %.0f%%",
        v$prop_variance[1] * 100, v$prop_variance[2] * 100
      ),
      x = sprintf("PC1 (%.0f%%)", v$prop_variance[1] * 100),
      y = sprintf("PC2 (%.0f%%)", v$prop_variance[2] * 100)
    ) +
    ggplot2::theme_minimal(base_size = 12)
}

#' Summarise how much the severity adjustment moved each country
#'
#' Diagnostic for the crisis modifier: reports the uplift applied to each
#' country, which channels were active, and how the result compares with the
#' deprecated v4.0 formula. This is the table to inspect when someone asks why a
#' country changed band.
#'
#' @param df A data frame produced by [add_severity()].
#' @param top_n Number of most-adjusted countries to return. `NULL` returns all.
#'
#' @return A data frame sorted by descending uplift.
#' @export
summarise_severity_effect <- function(df, top_n = 25) {
  cols <- c("country", "ISO_A3", "overall_risk", "severity_adjusted_risk",
            "severity_uplift", "severity_index",
            "w_hazard", "w_vulnerability", "w_capacity",
            "i_hazard", "i_vulnerability", "i_capacity",
            "driver_matched", "low_confidence")
  cols <- intersect(cols, names(df))

  out <- df[, cols, drop = FALSE]

  if ("overall_risk" %in% cols && "severity_index" %in% cols) {
    s <- df$severity_index
    s[is.na(s)] <- 1
    out$risk_v4_0 <- df$overall_risk + ((s - 1) / 4) * (1 - df$overall_risk)
    out$uplift_v4_0 <- out$risk_v4_0 - df$overall_risk
  }

  out <- out[order(-out$severity_uplift, na.last = NA), , drop = FALSE]
  if (!is.null(top_n)) {
    out <- utils::head(out, top_n)
  }
  out
}
