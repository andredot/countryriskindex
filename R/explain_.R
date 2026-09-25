## SCORE DECOMPOSITION ----------------------------------------------------
##
## Answers "why did this country get this score?" in a way that is exact rather
## than illustrative: every contribution reported here sums to the quantity it
## claims to explain.
##
## The index is a geometric mean, which is additive in logs, so the top level
## decomposes with no approximation at all:
##
##   log(risk_c) - log(risk_ref) = (1/3) * SUM_k [ log(x_kc) - log(x_k,ref) ]
##
## Below the top level that identity breaks, because invert_0_10() and
## normalise_quantiles() are affine rather than multiplicative. Extending the
## log decomposition through them is not merely imprecise - on realistic inputs
## it returns the wrong sign. Sub-component attribution therefore uses exact
## Shapley values, which are agnostic to what sits between input and output and
## satisfy the efficiency axiom (they sum to the total effect).
##
## The two levels compose without a fudge factor: the Shapley values within a
## pillar sum to log(pillar_c) - log(pillar_ref), so multiplying them by 1/3
## makes them sum to that pillar's exact top-level contribution.

#' Components of the index, in reporting order
#'
#' @return A character vector.
#' @export
risk_components <- function() c("hazard", "vulnerability", "capacity")

#' Resolve the normalisation bounds used by each pillar
#'
#' The decomposition re-evaluates each pillar's aggregation at counterfactual
#' inputs, so the normalisation bounds must be identical for every evaluation -
#' otherwise the function being decomposed changes between coalitions and the
#' Shapley values mean nothing. Frozen bounds satisfy that by construction;
#' without them the bounds are derived once from the whole cohort, which is what
#' run-relative normalisation effectively did.
#'
#' @param df A scored data frame.
#' @param reference_quantiles Frozen normalisation table, or `NULL`.
#'
#' @return A named list of `list(q01, q99)` per pillar.
#' @export
resolve_pillar_bounds <- function(df, reference_quantiles = NULL) {
  spec <- list(
    vulnerability = list(key = "vulnerability_raw",
                         cols = vulnerability_component_cols(),
                         invert = invert_0_10),
    capacity = list(key = "capacity_raw",
                    cols = capacity_component_cols(),
                    invert = invert_0_100)
  )

  lapply(spec, function(sp) {
    bounds <- get_reference_bounds(reference_quantiles, sp$key)
    if (!is.null(bounds)) return(bounds)

    cols <- intersect(sp$cols, names(df))
    if (length(cols) == 0) return(NULL)
    raw <- sp$invert(row_geometric_mean(df, cols, min_n = 1))
    q01 <- unname(stats::quantile(raw, 0.01, na.rm = TRUE))
    q99 <- unname(stats::quantile(raw, 0.99, na.rm = TRUE))
    if (!is.finite(q01) || !is.finite(q99) || q99 <= q01) return(NULL)
    list(q01 = q01, q99 = q99)
  })
}

#' Build the reference profile a country is compared against
#'
#' Contributions are only meaningful relative to something. This builds that
#' baseline: by default the geometric mean of each component across all scored
#' countries, so the reference is itself a coherent (if fictional) country whose
#' risk equals the geometric mean of the reference components.
#'
#' The pillar scores are **derived** from the reference sub-components rather
#' than averaged directly. Averaging the observed pillar scores would give a
#' reference that is not reachable from the reference sub-components, because
#' aggregation does not commute with averaging; the two decomposition levels
#' would then fail to reconcile.
#'
#' @param df A scored data frame.
#' @param method `"global"` (geometric mean over all countries) or `"median"`.
#' @param subset Optional logical vector selecting the reference peer group, for
#'   example a region. Recycled against `nrow(df)`.
#' @param bounds Pillar normalisation bounds from [resolve_pillar_bounds()].
#'
#' @return A named numeric vector with one entry per component score and per
#'   sub-component.
#' @export
build_reference_profile <- function(df, method = c("global", "median"),
                                    subset = NULL, bounds = NULL) {
  method <- match.arg(method)
  if (!is.null(subset)) df <- df[subset, , drop = FALSE]
  if (is.null(bounds)) bounds <- resolve_pillar_bounds(df)

  centre <- function(cl) {
    x <- suppressWarnings(as.numeric(df[[cl]]))
    x <- x[is.finite(x)]
    if (length(x) == 0) return(NA_real_)
    if (method == "median") stats::median(x) else geometric_mean(x)
  }

  sub_cols <- intersect(c(vulnerability_component_cols(),
                          capacity_component_cols()), names(df))
  ref <- vapply(sub_cols, centre, numeric(1))

  if ("hazard_score" %in% names(df)) {
    ref[["hazard_score"]] <- centre("hazard_score")
  }

  vul <- vulnerability_component_cols()
  if (all(vul %in% names(ref)) && !is.null(bounds$vulnerability)) {
    ref[["vulnerability_score"]] <- aggregate_vulnerability(
      as.data.frame(as.list(ref[vul])),
      q01 = bounds$vulnerability$q01, q99 = bounds$vulnerability$q99,
      min_n = 1
    )
  }

  cap <- capacity_component_cols()
  if (all(cap %in% names(ref)) && !is.null(bounds$capacity)) {
    ref[["capacity_score"]] <- aggregate_capacity(
      as.data.frame(as.list(ref[cap])),
      q01 = bounds$capacity$q01, q99 = bounds$capacity$q99,
      min_n = 1
    )
  }

  ref
}

#' Exact Shapley values for a scalar function of a few inputs
#'
#' Enumerates all `2^n` coalitions, so this is exact rather than sampled. Keep
#' `n` small: the index uses it per pillar (four inputs, sixteen coalitions),
#' never over all indicators at once.
#'
#' @param f A function taking a named numeric vector and returning one number.
#' @param x_country Named numeric vector of the country's values.
#' @param x_ref Named numeric vector of reference values, same names and order.
#'
#' @return A named numeric vector of contributions summing to
#'   `f(x_country) - f(x_ref)`.
#' @export
#'
#' @examples
#' f <- function(x) log(prod(x))
#' shapley_contributions(f, c(a = 2, b = 8), c(a = 1, b = 1))
shapley_contributions <- function(f, x_country, x_ref) {
  n <- length(x_country)
  stopifnot(n == length(x_ref), n >= 1)
  nm <- names(x_country)
  phi <- stats::setNames(numeric(n), nm)

  if (n > 12) {
    stop("shapley_contributions(): ", n, " inputs means ", 2^n,
         " coalitions per country. Decompose hierarchically instead.",
         call. = FALSE)
  }

  fact <- factorial(seq_len(n + 1) - 1)
  for (i in seq_len(n)) {
    others <- setdiff(seq_len(n), i)
    coalitions <- list(integer(0))
    for (k in seq_along(others)) {
      coalitions <- c(coalitions, utils::combn(others, k, simplify = FALSE))
    }
    for (S in coalitions) {
      w <- fact[length(S) + 1] * fact[n - length(S)] / fact[n + 1]
      without <- x_ref
      if (length(S)) without[S] <- x_country[S]
      with_i <- without
      with_i[i] <- x_country[i]
      phi[i] <- phi[i] + w * (f(with_i) - f(without))
    }
  }
  phi
}

#' Decompose the three component scores against a reference
#'
#' Exact: the contributions sum to `log(overall_risk) - log(reference_risk)`.
#'
#' @param df A scored data frame.
#' @param reference A named vector from [build_reference_profile()].
#' @param eps Floor applied before taking logs, since quantile normalisation can
#'   return exactly zero.
#'
#' @return A long data frame with `ISO_A3`, `country`, `component`,
#'   `country_value`, `reference_value`, `contribution` (log scale) and `factor`
#'   (the multiplicative equivalent, `exp(contribution)`).
#' @export
decompose_components <- function(df, reference, eps = 1e-4) {
  k <- length(risk_components())

  out <- lapply(risk_components(), function(comp) {
    cl <- paste0(comp, "_score")
    xc <- pmax(suppressWarnings(as.numeric(df[[cl]])), eps)
    xr <- max(reference[[cl]], eps)
    contribution <- (1 / k) * (log(xc) - log(xr))
    data.frame(
      ISO_A3 = df[["ISO_A3"]],
      country = df[["country"]],
      component = comp,
      country_value = xc,
      reference_value = xr,
      contribution = contribution,
      factor = exp(contribution),
      stringsAsFactors = FALSE
    )
  })

  do.call(rbind, out)
}

#' Decompose each pillar into its sub-components by exact Shapley value
#'
#' Scaled by `1/3` so that the sub-component contributions of a pillar sum to
#' that pillar's top-level contribution, making the two levels consistent.
#'
#' Hazard is a single indicator (the all-cause DALY rate), so its "decomposition"
#' is its whole contribution and no Shapley computation is needed.
#'
#' @param df A scored data frame.
#' @param reference A named vector from [build_reference_profile()].
#' @param bounds Pillar normalisation bounds from [resolve_pillar_bounds()].
#' @param eps Floor applied before taking logs.
#'
#' @return A long data frame with `ISO_A3`, `country`, `component`, `indicator`,
#'   `contribution`.
#' @export
decompose_indicators <- function(df, reference, bounds = NULL, eps = 1e-4) {
  if (is.null(bounds)) bounds <- resolve_pillar_bounds(df)
  k <- length(risk_components())

  pillars <- list(
    vulnerability = list(
      cols = vulnerability_component_cols(),
      key = "vulnerability_raw",
      invert = invert_0_10,
      agg = function(x, b) aggregate_vulnerability(
        as.data.frame(as.list(x)), q01 = b$q01, q99 = b$q99, min_n = 1)
    ),
    capacity = list(
      cols = capacity_component_cols(),
      key = "capacity_raw",
      invert = invert_0_100,
      agg = function(x, b) aggregate_capacity(
        as.data.frame(as.list(x)), q01 = b$q01, q99 = b$q99, min_n = 1)
    )
  )

  results <- list()

  for (comp in names(pillars)) {
    spec <- pillars[[comp]]
    cols <- intersect(spec$cols, names(df))
    if (length(cols) < 2) next

    b <- bounds[[comp]]
    if (is.null(b)) {
      warning("decompose_indicators(): no normalisation bounds for '", comp,
              "'; skipping its sub-component decomposition.", call. = FALSE)
      next
    }
    f <- function(x) log(max(spec$agg(x, b), eps))

    x_ref <- reference[cols]
    if (anyNA(x_ref)) next

    mat <- as.matrix(df[, cols, drop = FALSE])
    storage.mode(mat) <- "double"

    phi <- matrix(NA_real_, nrow = nrow(df), ncol = length(cols),
                  dimnames = list(NULL, cols))
    for (i in seq_len(nrow(mat))) {
      xc <- mat[i, ]
      if (anyNA(xc)) next
      phi[i, ] <- shapley_contributions(f, xc, x_ref) / k
    }

    results[[comp]] <- data.frame(
      ISO_A3 = rep(df[["ISO_A3"]], times = length(cols)),
      country = rep(df[["country"]], times = length(cols)),
      component = comp,
      indicator = rep(cols, each = nrow(df)),
      contribution = as.vector(phi),
      stringsAsFactors = FALSE
    )
  }

  # Hazard rests on a single indicator, so it passes through unchanged
  if ("hazard_score" %in% names(df)) {
    xc <- pmax(suppressWarnings(as.numeric(df$hazard_score)), eps)
    xr <- max(reference[["hazard_score"]], eps)
    results$hazard <- data.frame(
      ISO_A3 = df[["ISO_A3"]],
      country = df[["country"]],
      component = "hazard",
      indicator = "All causes DALY rate",
      contribution = (1 / k) * (log(xc) - log(xr)),
      stringsAsFactors = FALSE
    )
  }

  do.call(rbind, results)
}

#' Decompose the crisis severity adjustment by component
#'
#' The severity-adjusted score is the geometric mean of the adjusted components,
#' so the uplift decomposes exactly in logs as
#' `(1/3) * sum_k [ log(adj_k) - log(x_k) ]`. Note these are *log-score* effects,
#' not the `delta_*` log-odds shifts that produced them; both are reported.
#'
#' @param df A scored data frame produced by [add_severity()].
#' @param eps Floor applied before taking logs.
#'
#' @return A long data frame with `ISO_A3`, `country`, `component`,
#'   `contribution`, `delta_logodds`, `weight`, `intensity`.
#' @export
decompose_severity <- function(df, eps = 1e-4) {
  k <- length(risk_components())

  out <- lapply(risk_components(), function(comp) {
    base_col <- paste0(comp, "_score")
    adj_col <- paste0(comp, "_score_adj")
    if (!all(c(base_col, adj_col) %in% names(df))) return(NULL)

    xb <- pmax(suppressWarnings(as.numeric(df[[base_col]])), eps)
    xa <- pmax(suppressWarnings(as.numeric(df[[adj_col]])), eps)

    pull <- function(nm) {
      if (nm %in% names(df)) as.numeric(df[[nm]]) else rep(NA_real_, nrow(df))
    }

    data.frame(
      ISO_A3 = df[["ISO_A3"]],
      country = df[["country"]],
      component = comp,
      contribution = (1 / k) * (log(xa) - log(xb)),
      delta_logodds = pull(paste0("delta_", comp)),
      weight = pull(paste0("w_", comp)),
      intensity = pull(paste0("i_", comp)),
      stringsAsFactors = FALSE
    )
  })

  do.call(rbind, Filter(Negate(is.null), out))
}

#' Per-country summary of the decomposition
#'
#' Reports the reference risk, the country's scores, which component dominates,
#' which mitigates, and how much of the score rests on missing data.
#'
#' @param df A scored data frame.
#' @param components Output of [decompose_components()].
#' @param severity Output of [decompose_severity()].
#' @param reference A named vector from [build_reference_profile()].
#'
#' @return A data frame with one row per country.
#' @export
summarise_decomposition <- function(df, components, severity, reference) {
  ref_risk <- geometric_mean(
    reference[paste0(risk_components(), "_score")]
  )

  pick <- function(iso, tab, fun) {
    d <- tab[tab$ISO_A3 == iso & is.finite(tab$contribution), , drop = FALSE]
    if (nrow(d) == 0) return(NA_character_)
    d$component[fun(d$contribution)]
  }

  out <- data.frame(
    ISO_A3 = df[["ISO_A3"]],
    country = df[["country"]],
    reference_risk = ref_risk,
    overall_risk = df[["overall_risk"]],
    severity_adjusted_risk = df[["severity_adjusted_risk"]],
    stringsAsFactors = FALSE
  )

  out$dominant_driver <- vapply(out$ISO_A3, pick, character(1),
                                tab = components, fun = which.max)
  out$mitigating_factor <- vapply(out$ISO_A3, pick, character(1),
                                  tab = components, fun = which.min)

  sev <- stats::aggregate(contribution ~ ISO_A3, data = severity, FUN = sum)
  names(sev)[2] <- "severity_contribution"
  out <- merge(out, sev, by = "ISO_A3", all.x = TRUE)

  for (nm in c("data_completeness", "low_confidence", "n_missing_total")) {
    if (nm %in% names(df)) {
      out[[nm]] <- df[[nm]][match(out$ISO_A3, df[["ISO_A3"]])]
    }
  }

  out[order(-out$severity_adjusted_risk, na.last = TRUE), , drop = FALSE]
}

#' Attach a data-confidence flag to indicator contributions
#'
#' A large contribution resting on imputed or missing inputs is exactly what a
#' reviewer should see questioned. This marks each contribution with the
#' completeness of the component it belongs to.
#'
#' @param indicators Output of [decompose_indicators()].
#' @param df The scored data frame, carrying `n_missing_<component>`.
#'
#' @return `indicators` with added `component_completeness` and
#'   `low_confidence` columns.
#' @export
attach_confidence <- function(indicators, df) {
  sets <- default_indicator_sets()

  indicators$component_completeness <- NA_real_
  for (comp in names(sets)) {
    col <- paste0("n_missing_", comp)
    if (!col %in% names(df)) next
    n_tot <- length(sets[[comp]])
    completeness <- 1 - (df[[col]] / n_tot)
    idx <- indicators$component == comp
    indicators$component_completeness[idx] <-
      completeness[match(indicators$ISO_A3[idx], df[["ISO_A3"]])]
  }

  if ("low_confidence" %in% names(df)) {
    indicators$low_confidence <-
      df[["low_confidence"]][match(indicators$ISO_A3, df[["ISO_A3"]])]
  } else {
    indicators$low_confidence <- NA
  }

  indicators
}

#' Build the full score decomposition
#'
#' The expensive part of the explanation, computed once in the pipeline so the
#' Shiny app and the report read a precomputed object rather than recalculating
#' Shapley values on every interaction.
#'
#' @param risk_score A scored data frame.
#' @param reference_quantiles Frozen normalisation table.
#' @param method Reference profile method, see [build_reference_profile()].
#'
#' @return A list of data frames: `components`, `indicators`, `severity`,
#'   `summary`, plus the `reference` vector used.
#' @export
build_decomposition <- function(risk_score, reference_quantiles = NULL,
                                method = c("global", "median")) {
  method <- match.arg(method)

  scored <- risk_score[!is.na(risk_score[["overall_risk"]]), , drop = FALSE]
  bounds <- resolve_pillar_bounds(scored, reference_quantiles)
  reference <- build_reference_profile(scored, method = method, bounds = bounds)

  components <- decompose_components(scored, reference)
  indicators <- decompose_indicators(scored, reference, bounds)
  indicators <- attach_confidence(indicators, scored)
  severity <- decompose_severity(scored)

  list(
    components = components,
    indicators = indicators,
    severity = severity,
    summary = summarise_decomposition(scored, components, severity, reference),
    reference = reference,
    bounds = bounds,
    method = method
  )
}

## PLOTS -------------------------------------------------------------------

#' Human-readable labels for decomposition items
#'
#' @return A named character vector.
#' @export
decomposition_labels <- function() {
  c(
    hazard = "Hazard",
    vulnerability = "Vulnerability",
    capacity = "Capacity",
    vc_infrastructure = "Infrastructure",
    vc_education = "Education",
    vc_vulnerable_groups = "Vulnerable groups",
    vc_socioeconomic = "Socio-economic",
    governance = "Governance",
    financing = "Financing",
    resources = "Resources",
    services = "Services",
    `All causes DALY rate` = "All-cause DALY rate"
  )
}

#' Relabel a vector using [decomposition_labels()]
#' @param x Character vector of item keys.
#' @return A character vector of display labels.
#' @keywords internal
#' @noRd
pretty_label <- function(x) {
  lab <- decomposition_labels()
  out <- unname(lab[x])
  ifelse(is.na(out), x, out)
}

#' Waterfall chart of how a country's score was reached
#'
#' Reads the precomputed decomposition and shows the exact multiplicative path
#' from the reference country to the severity-adjusted score. Because the
#' geometric mean is additive in logs, the bars are not an approximation: the
#' running product reproduces the published score.
#'
#' @param decomposition Output of [build_decomposition()].
#' @param country Country name to plot.
#' @param level `"component"` for the three pillars, or `"indicator"` to expand
#'   them into sub-components.
#' @param include_severity Whether to append the crisis modifier steps.
#'
#' @return A ggplot2 object.
#' @export
create_decomposition_waterfall <- function(decomposition,
                                           country,
                                           level = c("component", "indicator"),
                                           include_severity = TRUE) {
  level <- match.arg(level)

  empty <- function(msg) {
    ggplot2::ggplot() +
      ggplot2::annotate("text", x = 0, y = 0, label = msg,
                        size = 5, colour = "grey30") +
      ggplot2::theme_void()
  }

  smry <- decomposition$summary
  smry <- smry[smry$country == country, , drop = FALSE]
  if (nrow(smry) == 0) {
    return(empty(paste0("No decomposition available for ", country, ".")))
  }

  tab <- if (level == "component") decomposition$components else
    decomposition$indicators
  tab <- tab[tab$country == country, , drop = FALSE]
  if (nrow(tab) == 0) return(empty("No contributions available."))

  if (level == "component") {
    steps <- data.frame(item = pretty_label(tab$component),
                        contribution = tab$contribution,
                        block = "Structural",
                        stringsAsFactors = FALSE)
  } else {
    ord <- order(match(tab$component, risk_components()), -tab$contribution)
    tab <- tab[ord, , drop = FALSE]
    steps <- data.frame(item = pretty_label(tab$indicator),
                        contribution = tab$contribution,
                        block = pretty_label(tab$component),
                        stringsAsFactors = FALSE)
  }

  if (isTRUE(include_severity) && !is.null(decomposition$severity)) {
    sev <- decomposition$severity
    sev <- sev[sev$country == country & is.finite(sev$contribution) &
                 abs(sev$contribution) > 1e-10, , drop = FALSE]
    if (nrow(sev) > 0) {
      steps <- rbind(steps, data.frame(
        item = paste0("Crisis: ", pretty_label(sev$component)),
        contribution = sev$contribution,
        block = "Crisis modifier",
        stringsAsFactors = FALSE
      ))
    }
  }

  steps <- steps[is.finite(steps$contribution), , drop = FALSE]
  if (nrow(steps) == 0) return(empty("No finite contributions."))

  start <- smry$reference_risk[1]
  ends <- start * exp(cumsum(steps$contribution))
  starts <- c(start, utils::head(ends, -1))

  plot_df <- data.frame(
    idx = seq_len(nrow(steps)) + 1,
    item = steps$item,
    block = steps$block,
    xmin = starts,
    xmax = ends,
    direction = ifelse(steps$contribution >= 0, "Increases risk",
                       "Reduces risk"),
    stringsAsFactors = FALSE
  )

  anchors <- data.frame(
    idx = c(1, nrow(steps) + 2),
    item = c("Reference country", "Final score"),
    block = "Total",
    xmin = c(0, 0),
    xmax = c(start, utils::tail(ends, 1)),
    direction = "Total",
    stringsAsFactors = FALSE
  )

  all_df <- rbind(anchors[1, ], plot_df, anchors[2, ])
  all_df$item <- factor(all_df$item, levels = rev(all_df$item))

  ggplot2::ggplot(all_df) +
    ggplot2::geom_rect(
      ggplot2::aes(xmin = .data$xmin, xmax = .data$xmax,
                   ymin = as.numeric(.data$item) - 0.38,
                   ymax = as.numeric(.data$item) + 0.38,
                   fill = .data$direction)
    ) +
    ggplot2::geom_text(
      ggplot2::aes(x = pmax(.data$xmin, .data$xmax),
                   y = as.numeric(.data$item),
                   label = sprintf("%+.3f", .data$xmax - .data$xmin)),
      hjust = -0.15, size = 3.2, colour = "grey25"
    ) +
    ggplot2::scale_y_continuous(
      breaks = seq_along(levels(all_df$item)),
      labels = levels(all_df$item)
    ) +
    ggplot2::scale_fill_manual(values = c(
      "Increases risk" = "#b2182b",
      "Reduces risk" = "#2166ac",
      "Total" = "grey45"
    )) +
    ggplot2::expand_limits(x = max(all_df$xmax, na.rm = TRUE) * 1.15) +
    ggplot2::labs(
      title = paste0("How ", country, " reached its score"),
      subtitle = sprintf(
        paste0("Reference %.3f -> structural %.3f -> adjusted %.3f. ",
               "Dominant driver: %s. Bars multiply exactly to the final score."),
        start, smry$overall_risk[1], smry$severity_adjusted_risk[1],
        pretty_label(smry$dominant_driver[1])
      ),
      x = "Risk score", y = NULL, fill = NULL
    ) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(legend.position = "top",
                   panel.grid.major.y = ggplot2::element_blank())
}

#' Contribution bars with a data-confidence overlay
#'
#' Same numbers as the waterfall, shown as signed bars, with contributions that
#' rest on incomplete inputs hatched in outline so a large contribution built on
#' thin evidence is visible rather than implicit.
#'
#' @param decomposition Output of [build_decomposition()].
#' @param country Country name to plot.
#'
#' @return A ggplot2 object.
#' @export
create_contribution_bars <- function(decomposition, country) {
  tab <- decomposition$indicators
  tab <- tab[tab$country == country & is.finite(tab$contribution), ,
             drop = FALSE]
  if (nrow(tab) == 0) {
    return(
      ggplot2::ggplot() +
        ggplot2::annotate("text", x = 0, y = 0,
                          label = paste0("No contributions for ", country, "."),
                          size = 5, colour = "grey30") +
        ggplot2::theme_void()
    )
  }

  tab$label <- pretty_label(tab$indicator)
  tab$component_label <- pretty_label(tab$component)
  tab <- tab[order(tab$contribution), , drop = FALSE]
  tab$label <- factor(tab$label, levels = tab$label)

  if (!"component_completeness" %in% names(tab)) {
    tab$component_completeness <- NA_real_
  }
  tab$confident <- is.na(tab$component_completeness) |
    tab$component_completeness >= 0.999

  ggplot2::ggplot(tab, ggplot2::aes(x = .data$contribution, y = .data$label,
                                    fill = .data$component_label)) +
    ggplot2::geom_col(ggplot2::aes(alpha = .data$confident), width = 0.7) +
    ggplot2::geom_col(
      data = tab[!tab$confident, , drop = FALSE],
      colour = "grey20", linetype = "dashed", fill = NA, width = 0.7
    ) +
    ggplot2::geom_vline(xintercept = 0, colour = "grey30") +
    ggplot2::scale_alpha_manual(values = c(`TRUE` = 1, `FALSE` = 0.45),
                                guide = "none") +
    ggplot2::scale_fill_brewer(palette = "Set2") +
    ggplot2::labs(
      title = paste0("Contribution of each indicator - ", country),
      subtitle = paste(
        "Log-scale contributions relative to the reference country.",
        "Dashed outline: component has missing inputs."
      ),
      x = "Contribution (log scale)", y = NULL, fill = NULL
    ) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(legend.position = "top")
}
