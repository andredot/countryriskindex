## CRISIS SEVERITY ROUTING -----------------------------------------------
##
## The v4.0 severity adjustment was
##
##   adjusted = risk + ((severity - 1) / 4) * (1 - risk)
##
## which is a convex pull towards 1.0 rather than a modifier: at severity 5 it
## returns exactly 1.0 for every country, erasing hazard, vulnerability and
## capacity entirely, and at severity 3 it imposes a floor of 0.5. Empirically
## half a point of INFORM Severity moved the index more than doubling measured
## vulnerability, so the crisis modifier dominated the structural model.
##
## v4.1 replaces it with driver-routed, log-odds adjustment. Each INFORM
## Severity sub-dimension is routed onto the risk component it actually acts
## on, gated by which crisis drivers are present in that country, and applied
## as a bounded shift on the log-odds scale. Because the components are then
## combined with a geometric mean, a shift applied to a single component is
## automatically damped to roughly one third in log space - the modifier
## becomes compensatory rather than dominant, which is the intended behaviour.

#' Default mapping from crisis-modifier channels to risk components
#'
#' The crisis modifier matrix (`crisis_modifier_matrix.xlsx`) marks, for each
#' INFORM Severity driver, which thematic channels it acts on. This function
#' declares how those channels roll up into the three components of the index.
#'
#' @return A named list of character vectors: `hazard`, `vulnerability`,
#'   `capacity`.
#' @export
default_channel_map <- function() {
  list(
    hazard = c("Communicable diseases", "Traumatic/Violent injuries",
               "WASH", "Environmental"),
    vulnerability = c("Vulnerability"),
    capacity = c("Capacity")
  )
}

#' Default mapping from risk components to INFORM Severity sub-dimensions
#'
#' Each component is driven by the severity sub-dimension that is conceptually
#' responsible for it:
#' \itemize{
#'   \item \strong{hazard} <- `conditions_impact`, the geometric mean of
#'     "Conditions of people affected" and "Impact of the crisis" (computed in
#'     [preprocess_severity()]). This is the acute health burden of the crisis.
#'   \item \strong{vulnerability} <- `Society and safety`, covering social
#'     cohesion, safety and protection of the affected population.
#'   \item \strong{capacity} <- `operating_environment`, covering access
#'     constraints and the ability to deliver services, which is what degrades
#'     effective health system capacity during a crisis.
#' }
#'
#' All three are on the INFORM Severity 1-5 scale.
#'
#' @return A named character vector.
#' @export
default_severity_sources <- function() {
  c(
    hazard = "conditions_impact",
    vulnerability = "Society and safety",
    capacity = "operating_environment"
  )
}

#' Default log-odds coefficients for the severity adjustment
#'
#' `beta` is the log odds ratio applied to a component when its channel is
#' fully active (weight 1) at maximum severity (index 5). These defaults are a
#' *policy choice*, not an estimate, and are calibrated so that a country facing
#' the most severe crisis in the world on all three channels moves by about
#' +0.17 on the 0-1 scale (roughly one band on a five-band scale) from a
#' mid-range baseline, while a minor single-driver crisis moves it by about
#' +0.02, rather than +0.65 and +0.16 respectively under v4.0. Change them
#' deliberately and record the reason: see `severity_calibration_table()`.
#'
#' @return A named numeric vector.
#' @export
#'
#' @examples
#' severity_betas()
severity_betas <- function() {
  c(hazard = 0.8, vulnerability = 1.0, capacity = 1.0)
}

#' Compute per-country severity channel weights from crisis drivers
#'
#' For each risk component, counts how many of the country's active INFORM
#' Severity drivers act on that component according to the crisis modifier
#' matrix, then converts the count to a saturating weight in `[0, 1)`:
#'
#' \deqn{w = n / (n + \kappa)}
#'
#' so that one driver gives 0.33, two give 0.50 and four give 0.67 (at the
#' default `kappa = 2`). Saturation prevents countries with long driver lists
#' from accumulating unbounded uplift.
#'
#' Countries that have a severity record but no driver matching a channel
#' receive `w_floor` rather than zero, so that a coding gap in the driver matrix
#' cannot silently switch the crisis modifier off.
#'
#' @param df A data frame with one row per country, containing the one-hot
#'   `Driver *` columns produced by [preprocess_severity()] and a
#'   `severity_index` column.
#' @param cm_data The crisis modifier matrix: a data frame with a `Driver`
#'   column and one logical column per channel.
#' @param channel_map Named list mapping components to channel column names.
#'   Defaults to [default_channel_map()].
#' @param kappa Saturation constant; higher values mean slower saturation.
#' @param w_floor Minimum weight applied to countries that have a severity
#'   record but no matched driver.
#'
#' @return A data frame with columns `w_hazard`, `w_vulnerability`,
#'   `w_capacity`, the driver counts `n_drivers_*`, and a logical
#'   `driver_matched` diagnostic.
#' @export
severity_channel_weights <- function(df,
                                     cm_data = NULL,
                                     channel_map = default_channel_map(),
                                     kappa = 2,
                                     w_floor = 0.25) {

  n_rows <- nrow(df)
  has_severity <- !is.na(df[["severity_index"]])

  components <- names(channel_map)
  counts <- matrix(0, nrow = n_rows, ncol = length(components),
                   dimnames = list(NULL, components))

  driver_cols <- grep("^Driver ", names(df), value = TRUE)

  if (!is.null(cm_data) && "Driver" %in% names(cm_data) &&
        length(driver_cols) > 0) {

    for (comp in components) {
      channels <- intersect(channel_map[[comp]], names(cm_data))
      if (length(channels) == 0) next

      # Drivers in the matrix that act on at least one channel of this component
      acts <- rowSums(
        as.matrix(cm_data[, channels, drop = FALSE]) > 0,
        na.rm = TRUE
      ) > 0
      relevant_drivers <- as.character(cm_data$Driver[acts])

      # Match to the one-hot columns, which are prefixed "Driver "
      matching_cols <- intersect(paste0("Driver ", relevant_drivers),
                                 driver_cols)
      if (length(matching_cols) == 0) next

      counts[, comp] <- rowSums(
        as.matrix(df[, matching_cols, drop = FALSE]) > 0,
        na.rm = TRUE
      )
    }
  }

  driver_matched <- rowSums(counts) > 0

  weights <- counts / (counts + kappa)
  # Fall back to the floor where a crisis exists but no driver was matched
  for (comp in components) {
    needs_floor <- has_severity & weights[, comp] < w_floor
    weights[needs_floor, comp] <- w_floor
  }
  weights[!has_severity, ] <- 0

  out <- as.data.frame(weights)
  names(out) <- paste0("w_", components)
  for (comp in components) {
    out[[paste0("n_drivers_", comp)]] <- counts[, comp]
  }
  out$driver_matched <- driver_matched & has_severity

  out
}

#' Convert INFORM Severity sub-dimensions to intensities in [0, 1]
#'
#' Rescales each 1-5 severity sub-dimension to `(S - 1) / 4`, clamped to
#' `[0, 1]`, treating a missing value as no crisis (0). Because "no listed
#' crisis" and "listed at the minimum severity" both map to 0, the discontinuity
#' present in v4.0 - where merely appearing in the INFORM Severity list was
#' worth a step change in risk - is removed.
#'
#' @param df A data frame containing the severity sub-dimension columns.
#' @param sources Named character vector mapping components to column names.
#'   Defaults to [default_severity_sources()].
#'
#' @return A data frame with one `i_<component>` column per component.
#' @export
severity_intensities <- function(df, sources = default_severity_sources()) {
  out <- lapply(names(sources), function(comp) {
    col <- sources[[comp]]
    if (!col %in% names(df)) {
      warning(
        "severity_intensities(): column '", col, "' not found; ",
        "intensity for component '", comp, "' set to 0.",
        call. = FALSE
      )
      return(rep(0, nrow(df)))
    }
    x <- suppressWarnings(as.numeric(df[[col]]))
    i <- (x - 1) / 4
    i[is.na(i)] <- 0
    pmin(pmax(i, 0), 1)
  })

  names(out) <- paste0("i_", names(sources))
  as.data.frame(out)
}

#' Apply crisis severity to the individual risk components
#'
#' Routes each INFORM Severity sub-dimension onto the risk component it acts on
#' and applies a bounded log-odds shift:
#'
#' \deqn{component_{adj} = \mathrm{logistic}\left(\mathrm{logit}(component) +
#'   \beta_k \, w_k \, i_k\right)}
#'
#' where \eqn{w_k} is the driver-derived channel weight
#' ([severity_channel_weights()]), \eqn{i_k} the severity intensity
#' ([severity_intensities()]) and \eqn{\beta_k} the policy coefficient
#' ([severity_betas()]).
#'
#' The adjustment is bounded (a component can never reach 1), monotone in
#' severity, and expressed as a log odds ratio so it is interpretable and
#' comparable across baselines.
#'
#' @param df A data frame containing `hazard_score`, `vulnerability_score`,
#'   `capacity_score`, the severity sub-dimensions and the one-hot driver
#'   columns.
#' @param cm_data The crisis modifier matrix. If `NULL`, channel weights fall
#'   back to `w_floor` for every country with a severity record.
#' @param betas Named numeric vector of log-odds coefficients.
#' @param channel_map Named list mapping components to channel columns.
#' @param sources Named character vector mapping components to severity columns.
#' @param kappa Saturation constant for driver counts.
#' @param w_floor Minimum channel weight where a crisis exists but no driver
#'   matched.
#'
#' @return The input data frame with added columns: `w_*`, `n_drivers_*`,
#'   `driver_matched`, `i_*`, `delta_*` (the log-odds shift actually applied)
#'   and `hazard_score_adj`, `vulnerability_score_adj`, `capacity_score_adj`.
#' @export
add_severity_components <- function(df,
                                    cm_data = NULL,
                                    betas = severity_betas(),
                                    channel_map = default_channel_map(),
                                    sources = default_severity_sources(),
                                    kappa = 2,
                                    w_floor = 0.25) {

  components <- names(sources)
  score_cols <- stats::setNames(paste0(components, "_score"), components)
  missing_scores <- setdiff(score_cols, names(df))
  if (length(missing_scores) > 0) {
    stop(
      "add_severity_components(): missing component scores: ",
      paste(missing_scores, collapse = ", "),
      ". Run add_hazard_score(), add_vulnerability_score() and ",
      "add_capacity_score() first.",
      call. = FALSE
    )
  }

  weights <- severity_channel_weights(
    df, cm_data,
    channel_map = channel_map, kappa = kappa, w_floor = w_floor
  )
  intensities <- severity_intensities(df, sources = sources)

  df <- dplyr::bind_cols(df, weights, intensities)

  for (comp in components) {
    beta <- if (comp %in% names(betas)) betas[[comp]] else 0
    delta <- beta * df[[paste0("w_", comp)]] * df[[paste0("i_", comp)]]
    delta[is.na(delta)] <- 0

    df[[paste0("delta_", comp)]] <- delta
    df[[paste0(comp, "_score_adj")]] <- logit_shift(df[[score_cols[[comp]]]],
                                                    delta)
  }

  df
}

#' Compute the severity-adjusted risk score
#'
#' Combines the severity-adjusted components into the headline score using the
#' same geometric mean as the unadjusted [add_overall_risk()], so that the two
#' are directly comparable and their difference (`severity_uplift`) is
#' attributable entirely to the crisis modifier.
#'
#' The legacy v4.0 behaviour is retained under `method = "convex"` purely so
#' that the change can be quantified; it should not be used for reporting.
#'
#' @param df A data frame with component scores and severity data.
#' @param cm_data The crisis modifier matrix.
#' @param method Either `"components"` (default, driver-routed log-odds) or
#'   `"convex"` (deprecated v4.0 formula).
#' @param min_n Minimum number of non-missing components required, passed to
#'   [row_geometric_mean()].
#' @param ... Further arguments passed to [add_severity_components()].
#'
#' @return The input data frame with added columns `severity_adjusted_risk` and
#'   `severity_uplift`, plus the component diagnostics.
#' @export
add_severity <- function(df,
                         cm_data = NULL,
                         method = c("components", "convex"),
                         min_n = 3,
                         ...) {
  method <- match.arg(method)

  if (method == "convex") {
    warning(
      "add_severity(method = 'convex') reproduces the deprecated v4.0 ",
      "formula, which returns 1.0 for every country at severity 5. ",
      "Use it for comparison only.",
      call. = FALSE
    )
    df$severity_adjusted_risk <- update_score(
      base_value = df$overall_risk,
      multiplier = df$severity_index
    )
    df$severity_uplift <- df$severity_adjusted_risk - df$overall_risk
    return(df)
  }

  df <- add_severity_components(df, cm_data = cm_data, ...)

  adj_cols <- c("hazard_score_adj", "vulnerability_score_adj",
                "capacity_score_adj")
  df$severity_adjusted_risk <- row_geometric_mean(df, adj_cols, min_n = min_n)
  df$severity_uplift <- df$severity_adjusted_risk - df$overall_risk

  df
}

#' Tabulate the effect of the severity adjustment across scenarios
#'
#' Produces the calibration table used to justify the choice of
#' [severity_betas()]. For a grid of baseline risks, severity indices and driver
#' counts it reports the adjusted score under both the v4.1 driver-routed
#' log-odds method and the deprecated v4.0 convex formula, so the change in
#' behaviour is auditable rather than asserted.
#'
#' @param baseline Numeric vector of baseline component scores to evaluate. Each
#'   value is used for all three components, so the unadjusted geometric mean
#'   equals the baseline itself.
#' @param severity Numeric vector of INFORM Severity index values (1-5).
#' @param n_drivers Integer vector of active driver counts per channel.
#' @param betas Named numeric vector of log-odds coefficients.
#' @param kappa Saturation constant for driver counts.
#' @param w_floor Minimum channel weight.
#'
#' @return A data frame with one row per scenario and columns `baseline`,
#'   `severity`, `n_drivers`, `weight`, `risk_v4_1`, `risk_v4_0`,
#'   `uplift_v4_1`, `uplift_v4_0`.
#' @export
#'
#' @examples
#' severity_calibration_table(baseline = 0.35, severity = c(1, 3, 5),
#'                            n_drivers = c(0, 2, 6))
severity_calibration_table <- function(baseline = c(0.20, 0.35, 0.50, 0.65),
                                       severity = c(1, 2, 3, 4, 5),
                                       n_drivers = c(0, 1, 2, 4, 6),
                                       betas = severity_betas(),
                                       kappa = 2,
                                       w_floor = 0.25) {

  grid <- expand.grid(
    baseline = baseline,
    severity = severity,
    n_drivers = n_drivers,
    KEEP.OUT.ATTRS = FALSE
  )

  has_crisis <- grid$severity > 1
  weight <- grid$n_drivers / (grid$n_drivers + kappa)
  weight <- ifelse(has_crisis, pmax(weight, w_floor), 0)
  intensity <- pmin(pmax((grid$severity - 1) / 4, 0), 1)

  adj <- vapply(seq_len(nrow(grid)), function(i) {
    parts <- vapply(names(betas), function(comp) {
      logit_shift(grid$baseline[i], betas[[comp]] * weight[i] * intensity[i])
    }, numeric(1))
    geometric_mean(parts)
  }, numeric(1))

  grid$weight <- weight
  grid$risk_v4_1 <- adj
  grid$risk_v4_0 <- grid$baseline + intensity * (1 - grid$baseline)
  grid$uplift_v4_1 <- grid$risk_v4_1 - grid$baseline
  grid$uplift_v4_0 <- grid$risk_v4_0 - grid$baseline

  grid
}
