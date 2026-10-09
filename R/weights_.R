## GLOBAL (EFFECTIVE) WEIGHTS ----------------------------------------------
##
## The per-country decomposition in R/explain_.R says why one country got its
## score. This file answers the cross-country question: across all countries,
## how much does each component actually weigh in the index?
##
## Nominally the geometric mean weights the three pillars equally (1/3 each in
## log space). The effective weight also depends on how much each pillar varies
## between countries: a pillar on which every country scores alike cannot move
## a ranking, whatever its nominal weight.
##
## Because the decomposition is exact, the published log score is an exact sum
## of terms, T = sum_j c_j (one term per component or indicator, plus the
## crisis modifier). For an exact linear identity two summaries follow with no
## estimation error:
##
##   standardised beta   b_j = sd(c_j) / sd(T)
##   variance share      s_j = cov(c_j, T) / var(T) = b_j * cor(c_j, T)
##
## b_j is the coefficient of the identity rescaled to standard deviations (at
## component level it equals (1/3) * sd(log x_j) / sd(log risk), i.e. what an
## OLS of log risk on the log components returns, with R^2 = 1). s_j is Pratt's
## measure: the shares sum to exactly 1 and split the cross-country variance of
## the score between its terms, correlated terms included.

#' Effective weights of the index components across all countries
#'
#' Summarises the precomputed decomposition into one row per component (and per
#' indicator), reporting the nominal weight, the standardised beta coefficient
#' and the share of the cross-country variance of the score each term accounts
#' for. See the comment block in `R/weights_.R` for the derivation.
#'
#' @param decomposition Output of [build_decomposition()].
#' @param include_severity Whether the target is the published
#'   severity-adjusted score (adding the crisis modifier as a term) or the
#'   structural `overall_risk` only.
#'
#' @return A list with data frames `component` and `indicator`, each with
#'   columns `component`, `term`, `label`, `nominal_weight`, `std_beta`,
#'   `variance_share`, `mean_abs_contribution`; the number of countries used at
#'   each level (`n_component`, `n_indicator`); and `target`.
#' @export
summarise_global_weights <- function(decomposition, include_severity = TRUE) {
  comps <- risk_components()
  k <- length(comps)

  wide <- function(tab, key) {
    tab <- tab[is.finite(tab$contribution), , drop = FALSE]
    keys <- unique(tab[[key]])
    isos <- unique(tab$ISO_A3)
    m <- matrix(NA_real_, nrow = length(isos), ncol = length(keys),
                dimnames = list(isos, keys))
    m[cbind(match(tab$ISO_A3, isos), match(tab[[key]], keys))] <-
      tab$contribution
    m
  }

  crisis <- NULL
  if (include_severity && !is.null(decomposition$severity) &&
      nrow(decomposition$severity) > 0) {
    crisis <- wide(decomposition$severity, "component")
    colnames(crisis) <- paste0("crisis_", colnames(crisis))
  }

  summarise_terms <- function(m, meta) {
    m <- m[stats::complete.cases(m), , drop = FALSE]
    vs <- variance_shares(m)
    out <- meta[match(colnames(m), meta$term), , drop = FALSE]
    out$std_beta <- vs$std_beta
    out$variance_share <- vs$variance_share
    out$mean_abs_contribution <- unname(colMeans(abs(m)))
    rownames(out) <- NULL
    list(table = out, n = nrow(m))
  }

  crisis_meta <- data.frame(
    component = "crisis modifier",
    term = paste0("crisis_", comps),
    label = paste("Crisis modifier:", tolower(pretty_label(comps))),
    nominal_weight = NA_real_,
    stringsAsFactors = FALSE
  )

  # Component level: the three pillars, plus the crisis modifier as one term.
  m_comp <- wide(decomposition$components, "component")[, comps, drop = FALSE]
  meta_comp <- data.frame(component = comps, term = comps,
                          label = pretty_label(comps),
                          nominal_weight = 1 / k, stringsAsFactors = FALSE)
  if (!is.null(crisis)) {
    m_comp <- cbind(m_comp,
                    crisis = rowSums(crisis)[match(rownames(m_comp),
                                                   rownames(crisis))])
    meta_comp <- rbind(meta_comp, data.frame(
      component = "crisis modifier", term = "crisis",
      label = "Crisis modifier", nominal_weight = NA_real_,
      stringsAsFactors = FALSE))
  }
  comp_res <- summarise_terms(m_comp, meta_comp)

  # Indicator level: Shapley contributions, which sum exactly to the pillar
  # contributions, plus the crisis modifier split by the channel it acts on.
  ind <- decomposition$indicators
  m_ind <- wide(ind, "indicator")
  ind_comp <- ind$component[match(colnames(m_ind), ind$indicator)]
  n_in_pillar <- table(ind_comp)
  meta_ind <- data.frame(
    component = ind_comp,
    term = colnames(m_ind),
    label = pretty_label(colnames(m_ind)),
    nominal_weight = unname(1 / k / as.numeric(n_in_pillar[ind_comp])),
    stringsAsFactors = FALSE
  )
  if (!is.null(crisis)) {
    m_ind <- cbind(m_ind, crisis[match(rownames(m_ind), rownames(crisis)), ,
                                 drop = FALSE])
    meta_ind <- rbind(meta_ind,
                      crisis_meta[crisis_meta$term %in% colnames(crisis), ])
  }
  ind_res <- summarise_terms(m_ind, meta_ind)
  order_ind <- order(match(ind_res$table$component,
                           c(comps, "crisis modifier")),
                     -ind_res$table$variance_share)
  ind_res$table <- ind_res$table[order_ind, , drop = FALSE]
  rownames(ind_res$table) <- NULL

  list(
    component = comp_res$table,
    indicator = ind_res$table,
    n_component = comp_res$n,
    n_indicator = ind_res$n,
    target = if (include_severity) "severity_adjusted_risk" else "overall_risk"
  )
}

#' Standardised betas and variance shares of an additive score
#'
#' For a score that is the exact sum of the columns of `m` (one row per
#' country), returns each column's standardised beta `sd(c_j) / sd(T)` and its
#' share of the variance of the total, `cov(c_j, T) / var(T)`, which sum to 1.
#'
#' @param m Numeric matrix of complete contributions, one column per term.
#' @return A list with named numeric vectors `std_beta` and `variance_share`
#'   (`NA` when fewer than three rows or a constant total).
#' @export
variance_shares <- function(m) {
  m <- as.matrix(m)
  total <- rowSums(m)
  sd_t <- if (nrow(m) >= 3) stats::sd(total) else NA_real_
  if (!is.finite(sd_t) || sd_t <= 0) {
    na <- stats::setNames(rep(NA_real_, ncol(m)), colnames(m))
    return(list(std_beta = na, variance_share = na))
  }
  list(
    std_beta = apply(m, 2, stats::sd) / sd_t,
    variance_share = apply(m, 2, function(x) stats::cov(x, total)) /
      stats::var(total)
  )
}

#' Plot the effective weights of the index
#'
#' Bars show each term's share of the cross-country variance of the score;
#' diamonds show its nominal weight. A bar much longer than its diamond means
#' the term moves rankings more than its design weight suggests, usually
#' because countries differ more on it.
#'
#' @param weights Output of [summarise_global_weights()].
#' @param level `"component"` or `"indicator"`.
#'
#' @return A ggplot2 object.
#' @export
create_global_weights_plot <- function(weights,
                                       level = c("component", "indicator")) {
  level <- match.arg(level)
  d <- weights[[level]]
  n <- weights[[paste0("n_", level)]]
  if (is.null(d) || nrow(d) == 0 || all(is.na(d$variance_share))) {
    return(
      ggplot2::ggplot() +
        ggplot2::annotate("text", x = 0, y = 0,
                          label = "Not enough countries to estimate weights.",
                          size = 5, colour = "grey30") +
        ggplot2::theme_void()
    )
  }

  d$label <- factor(d$label, levels = rev(d$label))
  d$component_label <- ifelse(d$component == "crisis modifier",
                              "Crisis modifier", pretty_label(d$component))
  d$component_label <- factor(
    d$component_label,
    levels = c(pretty_label(risk_components()), "Crisis modifier"))

  ggplot2::ggplot(d, ggplot2::aes(y = .data$label)) +
    ggplot2::geom_col(ggplot2::aes(x = .data$variance_share,
                                   fill = .data$component_label),
                      width = 0.7) +
    ggplot2::geom_point(ggplot2::aes(x = .data$nominal_weight),
                        data = d[is.finite(d$nominal_weight), , drop = FALSE],
                        shape = 23, size = 3, fill = "white",
                        colour = "grey20") +
    ggplot2::geom_text(ggplot2::aes(x = pmax(.data$variance_share, 0),
                                    label = scales::percent(
                                      .data$variance_share, accuracy = 1)),
                       hjust = -0.2, size = 3.5, colour = "grey20") +
    ggplot2::geom_vline(xintercept = 0, colour = "grey30") +
    ggplot2::scale_x_continuous(labels = scales::percent,
                                expand = ggplot2::expansion(c(0.02, 0.12))) +
    ggplot2::scale_fill_brewer(palette = "Set2", drop = FALSE) +
    ggplot2::labs(
      title = if (level == "component") {
        "Effective weight of each component"
      } else {
        "Effective weight of each indicator"
      },
      subtitle = paste0(
        "Bars: share of the cross-country variance of log ",
        if (weights$target == "overall_risk") "structural risk" else
          "severity-adjusted risk",
        ". Diamonds: nominal weight. ", n, " countries."
      ),
      x = NULL, y = NULL, fill = NULL
    ) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(legend.position = "top",
                   panel.grid.major.y = ggplot2::element_blank())
}
