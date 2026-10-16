## SCALE AND AGGREGATION DIAGNOSTICS -----------------------------------------
##
## Why can a pillar with a nominal weight of 1/3 end up explaining almost none
## of the differences between countries? Because the index is a geometric
## mean, effective weights live in log space, and log space magnifies values
## close to zero:
##
##   * quantile normalisation maps every country at or below the reference
##     1st percentile to exactly 0, and the geometric mean clamps 0 to 1e-4,
##     i.e. log = -9.2 against roughly -1 for a typical country;
##   * a component whose best performers are bunched just above its lower
##     bound produces many values like 0.01-0.05 (log -4.6 to -3.0).
##
## Either way a handful of near-zero countries can dominate the log variance of
## one component, and with it the cross-country variance of the index, leaving
## the other components with little effective weight. The same happens inside
## a pillar when one sub-component is 0 (e.g. an INFORM score of 10, inverted).
##
## The functions below measure this on the actual data and compare the current
## formulation with alternatives that keep the framework but change the scale
## or the aggregation.

#' Diagnose the scale of each risk component
#'
#' For each component, reports the normalisation bounds used, how many
#' countries sit on or near the bounds of the normalised scale (before the
#' floor, see [scale_floor()]), the spread of the published score and of its
#' logarithm, how much of that log spread comes from countries near the lower
#' bound, and the component's share of the cross-country variance of log
#' structural risk.
#'
#' @param risk_score A scored data frame (output of [score_risk()]).
#' @param reference_quantiles Frozen normalisation table, or `NULL`.
#' @param eps Floor applied before taking logs, as in [geometric_mean()].
#' @param near Threshold on the normalised (unfloored) scale below which a
#'   country counts as "near the lower bound".
#' @param floor The floor the scores were computed with.
#'
#' @return A data frame with one row per component.
#' @export
diagnose_component_scales <- function(risk_score,
                                      reference_quantiles = NULL,
                                      eps = 1e-4,
                                      near = 0.05,
                                      floor = scale_floor()) {
  comps <- risk_components()
  scores <- as.data.frame(risk_score)[, paste0(comps, "_score"), drop = FALSE]
  complete <- stats::complete.cases(scores)
  logs <- log(pmax(as.matrix(scores[complete, , drop = FALSE]), eps)) /
    length(comps)
  shares <- variance_shares(logs)$variance_share

  rows <- lapply(comps, function(comp) {
    x <- risk_score[[paste0(comp, "_score")]]
    raw <- risk_score[[paste0(comp, "_raw")]]
    b <- get_reference_bounds(reference_quantiles, paste0(comp, "_raw"))
    source <- if (is.null(b)) "this run" else "frozen reference"
    if (is.null(b) && !is.null(raw)) {
      b <- list(q01 = unname(stats::quantile(raw, 0.01, na.rm = TRUE)),
                q99 = unname(stats::quantile(raw, 0.99, na.rm = TRUE)))
    }
    ok <- is.finite(x)
    x0 <- unfloor(x[ok], floor)
    lx <- log(pmax(x[ok], eps))
    dev2 <- (lx - mean(lx))^2
    data.frame(
      component = comp,
      n = sum(ok),
      bounds = source,
      q01 = b$q01 %||% NA_real_,
      q99 = b$q99 %||% NA_real_,
      at_lower_bound = sum(x0 <= 0),
      near_lower_bound = sum(x0 < near),
      at_upper_bound = sum(x0 >= 1),
      median = stats::median(x[ok]),
      sd_score = stats::sd(x[ok]),
      sd_log = stats::sd(lx),
      log_var_from_near_lower = sum(dev2[x0 < near]) / sum(dev2),
      variance_share = unname(shares[paste0(comp, "_score")]),
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows)
}

#' Undo the scale floor
#' @noRd
unfloor <- function(x, floor = scale_floor()) {
  pmin(pmax((x - floor) / (1 - floor), 0), 1)
}

#' Diagnose the sub-components inside each pillar
#'
#' Within a pillar the sub-components are combined by geometric mean, so a
#' sub-component equal to 0 collapses the pillar for that country. This reports,
#' per sub-component, its range, how many countries are at 0, its correlation
#' with the pillar score, and - when available - how the pillar ranks countries
#' compared with INFORM's own headline dimension.
#'
#' @param risk_score A scored data frame.
#'
#' @return A data frame with one row per sub-component, plus one benchmark row
#'   per pillar (`subcomponent = "INFORM benchmark"`, Spearman correlation in
#'   `cor_with_pillar`).
#' @export
diagnose_pillar_internals <- function(risk_score) {
  spec <- list(
    vulnerability = list(cols = vulnerability_component_cols(), scale = 10,
                         benchmark = "inform_vulnerability"),
    capacity = list(cols = capacity_component_cols(), scale = 100,
                    benchmark = "inform_lack_coping")
  )
  rows <- list()
  for (pillar in names(spec)) {
    sp <- spec[[pillar]]
    score <- risk_score[[paste0(pillar, "_score")]]
    for (cl in intersect(sp$cols, names(risk_score))) {
      x <- suppressWarnings(as.numeric(risk_score[[cl]]))
      ok <- is.finite(x)
      rows[[length(rows) + 1]] <- data.frame(
        pillar = pillar,
        subcomponent = cl,
        scale_max = sp$scale,
        n = sum(ok),
        min = min(x[ok]),
        median = stats::median(x[ok]),
        max = max(x[ok]),
        at_zero = sum(x[ok] <= 0),
        cor_with_pillar = suppressWarnings(
          stats::cor(x, score, use = "complete.obs", method = "spearman")),
        stringsAsFactors = FALSE
      )
    }
    if (sp$benchmark %in% names(risk_score) &&
        sum(is.finite(risk_score[[sp$benchmark]])) > 2) {
      rows[[length(rows) + 1]] <- data.frame(
        pillar = pillar, subcomponent = "INFORM benchmark",
        scale_max = 10, n = sum(is.finite(risk_score[[sp$benchmark]])),
        min = NA_real_, median = NA_real_, max = NA_real_, at_zero = NA_real_,
        cor_with_pillar = stats::cor(risk_score[[sp$benchmark]], score,
                                     use = "complete.obs",
                                     method = "spearman"),
        stringsAsFactors = FALSE
      )
    }
  }
  do.call(rbind, rows)
}

#' Compare alternative scale and aggregation choices
#'
#' Recomputes the structural score under alternatives that keep the three
#' pillars but change how they are put on a common scale or combined, starting
#' from the normalised component scores (the floor removed):
#' \describe{
#'   \item{published}{Components on `[published_floor, 1]`, geometric mean:
#'     the method used for the published scores (see [scale_floor()]).}
#'   \item{no floor}{Components on `[0, 1]`, zeros clamped to `1e-4`,
#'     geometric mean: the v4.10 method, where the few countries nearest the
#'     lower bound dominate.}
#'   \item{percentile}{Each component replaced by its percentile rank among
#'     countries (from the risk-oriented raw value), then geometric mean.
#'     Every component has the same distribution, so effective weights are
#'     close to nominal by construction, at the cost of discarding the size of
#'     the gaps between countries.}
#'   \item{arithmetic}{Arithmetic mean of the normalised components: fully
#'     compensatory, no log-space magnification.}
#' }
#'
#' Effective weights are computed in each variant's own aggregation space (log
#' for geometric means, linear for the arithmetic mean) with
#' [variance_shares()]. Rank agreement is measured against `published`.
#'
#' @param risk_score A scored data frame.
#' @param published_floor The floor the published scores were computed with.
#' @param eps Floor applied before taking logs in the `no floor` variant.
#' @param top_n Size of the high-risk group used for the overlap statistic.
#'
#' @return A list with `scores` (one row per country, the overall score and
#'   rank under each variant), `weights` (variant x component variance shares
#'   and standardised betas), `summary` (per variant: Spearman correlation and
#'   rank changes against `published`, overlap of the top `top_n`) and `n`.
#' @export
compare_scoring_variants <- function(risk_score,
                                     published_floor = scale_floor(),
                                     eps = 1e-4, top_n = 20) {
  prep <- variant_inputs(risk_score, published_floor)
  x0 <- prep$x0

  variants <- list(
    published = list(terms = log(apply_scale_floor(x0, published_floor)) / 3,
                     geometric = TRUE),
    `no floor` = list(terms = log(pmax(x0, eps)) / 3, geometric = TRUE),
    percentile = list(terms = log(prep$pct) / 3, geometric = TRUE),
    arithmetic = list(terms = x0 / 3, geometric = FALSE)
  )

  scores <- data.frame(ISO_A3 = prep$d$ISO_A3, country = prep$d$country,
                       stringsAsFactors = FALSE)
  weights <- list()
  for (v in names(variants)) {
    tm <- variants[[v]]$terms
    total <- rowSums(tm)
    scores[[v]] <- if (variants[[v]]$geometric) exp(total) else total
    scores[[paste0("rank_", v)]] <- rank(-scores[[v]], ties.method = "min")
    vs <- variance_shares(tm)
    weights[[v]] <- data.frame(variant = v, component = colnames(tm),
                               variance_share = unname(vs$variance_share),
                               std_beta = unname(vs$std_beta),
                               stringsAsFactors = FALSE)
  }
  weights <- do.call(rbind, weights)

  top_ref <- scores$ISO_A3[scores$rank_published <= top_n]
  summary <- do.call(rbind, lapply(names(variants), function(v) {
    shift <- abs(scores[[paste0("rank_", v)]] - scores$rank_published)
    top_v <- scores$ISO_A3[scores[[paste0("rank_", v)]] <= top_n]
    data.frame(
      variant = v,
      spearman_vs_published = stats::cor(scores[[v]], scores$published,
                                         method = "spearman"),
      median_rank_change = stats::median(shift),
      max_rank_change = max(shift),
      top_overlap = length(intersect(top_ref, top_v)) / top_n,
      stringsAsFactors = FALSE
    )
  }))

  list(scores = scores, weights = weights, summary = summary,
       n = nrow(scores), top_n = top_n, floor = published_floor)
}

#' Complete-case normalised components and percentile ranks
#' @noRd
variant_inputs <- function(risk_score, published_floor = scale_floor()) {
  comps <- risk_components()
  d <- as.data.frame(risk_score)
  x <- as.matrix(d[, paste0(comps, "_score"), drop = FALSE])
  keep <- stats::complete.cases(x)
  d <- d[keep, , drop = FALSE]
  x0 <- unfloor(x[keep, , drop = FALSE], published_floor)
  colnames(x0) <- comps

  raw_cols <- paste0(comps, "_raw")
  raw <- if (all(raw_cols %in% names(d))) as.matrix(d[, raw_cols]) else x0
  pct <- apply(raw, 2, function(v) {
    rank(v, ties.method = "average", na.last = "keep") / (sum(!is.na(v)) + 1)
  })
  pct[is.na(pct)] <- 0.5
  colnames(pct) <- comps

  list(d = d, x0 = x0, pct = pct)
}

#' Plot effective weights under each scoring variant
#'
#' @param comparison Output of [compare_scoring_variants()].
#' @return A ggplot2 object.
#' @export
create_variant_weights_plot <- function(comparison) {
  w <- comparison$weights
  w$component <- factor(pretty_label(w$component),
                        levels = pretty_label(risk_components()))
  w$variant <- factor(w$variant, levels = unique(w$variant))

  ggplot2::ggplot(w, ggplot2::aes(x = .data$variance_share,
                                  y = .data$variant,
                                  fill = .data$component)) +
    ggplot2::geom_col(position = ggplot2::position_dodge(width = 0.8),
                      width = 0.75) +
    ggplot2::geom_text(
      ggplot2::aes(x = pmax(.data$variance_share, 0),
                   label = scales::percent(.data$variance_share,
                                           accuracy = 1)),
      position = ggplot2::position_dodge(width = 0.8),
      hjust = -0.15, size = 3.3, colour = "grey20") +
    ggplot2::geom_vline(xintercept = 1 / 3, linetype = "dashed",
                        colour = "grey40") +
    ggplot2::scale_x_continuous(labels = scales::percent,
                                expand = ggplot2::expansion(c(0.02, 0.12))) +
    ggplot2::scale_y_discrete(limits = rev) +
    ggplot2::scale_fill_brewer(palette = "Set2") +
    ggplot2::labs(
      title = "Effective weight of each component under alternative methods",
      subtitle = paste0("Share of the cross-country variance of the ",
                        "structural score. Dashed line: nominal 1/3. ",
                        comparison$n, " countries."),
      x = NULL, y = NULL, fill = NULL
    ) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(legend.position = "top",
                   panel.grid.major.y = ggplot2::element_blank())
}

#' Effective weights as a function of the scale floor
#'
#' Recomputes the structural score with the normalised components mapped onto
#' `[floor, 1]` for several floors. A floor of 0 is the unfloored v4.10 scale
#' (zeros clamped to `1e-4`). A strong dependence of the weights on the floor
#' shows that they are driven by the few countries closest to the lower bound
#' rather than by the bulk of the distribution.
#'
#' @param risk_score A scored data frame.
#' @param floors Numeric vector of floors in `[0, 1)`.
#' @param published_floor The floor the published scores were computed with.
#' @param eps Clamp applied before taking logs when `floor = 0`.
#' @return A data frame with one row per floor and one column of variance
#'   share per component.
#' @export
floor_sensitivity <- function(risk_score,
                              floors = c(0, 0.05, 0.1, 0.2),
                              published_floor = scale_floor(),
                              eps = 1e-4) {
  x0 <- variant_inputs(risk_score, published_floor)$x0
  rows <- lapply(floors, function(f) {
    tm <- log(pmax(apply_scale_floor(x0, f), eps)) / 3
    vs <- variance_shares(tm)$variance_share
    out <- data.frame(floor = f, published = isTRUE(all.equal(f,
                                                              published_floor)))
    for (k in risk_components()) out[[k]] <- unname(vs[k])
    out
  })
  do.call(rbind, rows)
}
