## SCORE CALCULATION

#' Compute Composite Vulnerability Score
#'
#' Calculates a composite vulnerability score using indicators from WHO and
#' INFORM datasets.
#' The score is based on four conceptual components:
#' \itemize{
#'   \item \strong{Socioeconomic vulnerability}: \code{soc_econ_vulnerability},
#'   inverted on a 0-10 scale.
#'   \item \strong{Infrastructure}: Computed as the geometric mean of:
#'     \itemize{
#'       \item \code{comms} - average of \code{connectivity}, \code{electricity},
#'        \code{internet}, \code{mobile}
#'       \item \code{physical} - average of \code{road_density},
#'        \code{improved_water}, \code{improved_sanitation}
#'     }
#'     Both components are inverted on a 0-10 scale before aggregation.
#'   \item \strong{Vulnerable groups}: Geometric mean of \code{uprooted} and
#'   \code{food_sec}, inverted on a 0-10 scale.
#'   \item \strong{Education}: \code{adult_literacy}, inverted on a 0-10 scale.
#' }
#'
#' The final vulnerability score is the normalised geometric mean of the four
#' components. Normalisation uses frozen reference bounds when `reference` is
#' supplied, so that a country's score does not move merely because other
#' countries moved (see [normalise_quantiles()]).
#'
#' @param df A data frame with the required columns for vulnerability
#'   computation.
#' @param reference Optional reference quantile table from
#'   [compute_reference_quantiles()]. When `NULL`, bounds are computed from the
#'   current run and scores are *not* comparable across releases.
#' @param min_n Minimum number of non-missing sub-components required before a
#'   score is produced; below this the score is `NA`.
#'
#' @return The input data frame with added columns:
#' \itemize{
#'   \item \code{vulnerable_groups}, \code{comms}, \code{physical},
#'   \code{infrastructure}
#'   \item \code{vulnerability_score} - normalised composite score.
#' }
#' @export
add_vulnerability_score <- function(df, reference = NULL, min_n = 3) {

  df$vulnerable_groups <- rowMeans(
    df[, c("uprooted", "food_sec")],
    na.rm = TRUE
  )

  df$comms <- rowMeans(
    df[, c("connectivity", "electricity", "internet", "mobile")],
    na.rm = TRUE
  )

  df$physical <- rowMeans(
    df[, c("road_density", "improved_water", "improved_sanitation")],
    na.rm = TRUE
  )

  inverted_infra <- data.frame(
    comms = invert_0_10(df$comms),
    physical = invert_0_10(df$physical)
  )
  df$infrastructure <- row_geometric_mean(inverted_infra,
                                          c("comms", "physical"),
                                          min_n = 1)

  # Final vulnerability components, all oriented so that higher = better
  raw_components <- data.frame(
    infrastructure = df$infrastructure,
    adult_literacy = invert_0_10(df$adult_literacy),
    vulnerable_groups = invert_0_10(df$vulnerable_groups),
    soc_econ_vulnerability = invert_0_10(df$soc_econ_vulnerability)
  )

  raw_geom <- row_geometric_mean(raw_components, names(raw_components),
                                 min_n = min_n)
  inverted_geom <- invert_0_10(raw_geom)

  bounds <- get_reference_bounds(reference, "vulnerability_raw")
  df$vulnerability_raw <- inverted_geom
  df$vulnerability_score <- normalise_quantiles(
    inverted_geom,
    q01 = bounds$q01,
    q99 = bounds$q99
  )

  df
}

#' Compute Hazard Score from GBD Rates
#'
#' Calculates a normalised hazard score from the "All causes" DALY rate.
#'
#' v4.0 divided by the observed maximum, which made every country's score
#' hostage to a single extreme country and compressed the usable range to
#' roughly `[0.27, 1]`. v4.1 uses the same frozen quantile normalisation as the
#' vulnerability and capacity components, so all three are on a common footing
#' and the score is stable across releases.
#'
#' @param df A data frame containing a column named \code{"All causes"}.
#' @param reference Optional reference quantile table from
#'   [compute_reference_quantiles()].
#' @param method Either `"quantile"` (default) or `"max"` (deprecated v4.0
#'   behaviour, retained for comparison only).
#'
#' @return The input data frame with added columns \code{hazard_raw} and
#'   \code{hazard_score}.
#' @export
add_hazard_score <- function(df, reference = NULL,
                             method = c("quantile", "max")) {
  method <- match.arg(method)
  df$hazard_raw <- df[["All causes"]]

  if (method == "max") {
    warning(
      "add_hazard_score(method = 'max') reproduces the deprecated v4.0 ",
      "normalisation, which is not comparable across releases.",
      call. = FALSE
    )
    df$hazard_score <- df$hazard_raw / max(df$hazard_raw, na.rm = TRUE)
    return(df)
  }

  bounds <- get_reference_bounds(reference, "hazard_raw")
  df$hazard_score <- normalise_quantiles(df$hazard_raw,
                                         q01 = bounds$q01,
                                         q99 = bounds$q99)
  df
}

#' Compute Health System Capacity Score
#'
#' Calculates a composite health system capacity score using four components
#' aligned to the HSPA framework:
#' \itemize{
#'   \item \strong{Governance}: Inverted \code{gov_effectivess} (0-10 rescaled
#'   to 0-100).
#'   \item \strong{Financing}: Average of \code{uhc_coverage} and inverted
#'   \code{health_per_capita} (rescaled to 0-100).
#'   \item \strong{Resources}: \code{doctor_density}, normalised to 0-100.
#'   \item \strong{Services}: \code{haqi}.
#' }
#'
#' In v4.0 `resources` was the raw WHO physicians-per-10,000 figure, spanning
#' roughly 0.1 to 85 while the other three components spanned 0-100. Inside a
#' geometric mean the relevant quantity is spread in log space, where the raw
#' density carried about four times the influence of the other three combined
#' and countries near zero drove the aggregate on their own. v4.1 normalises it
#' onto the same 0-100 scale, on a log axis because the distribution is heavily
#' right-skewed.
#'
#' @param df A data frame with the required columns for capacity computation.
#' @param reference Optional reference quantile table from
#'   [compute_reference_quantiles()].
#' @param min_n Minimum number of non-missing sub-components required.
#'
#' @return The input data frame with added columns:
#' \itemize{
#'   \item \code{governance}, \code{financing}, \code{resources}, \code{services}
#'   \item \code{capacity_score} - normalised composite score.
#' }
#' @export
add_capacity_score <- function(df, reference = NULL, min_n = 3) {
  df$governance <- invert_0_100(df$gov_effectivess * 10)

  df$financing <- rowMeans(
    cbind(
      df$uhc_coverage,
      invert_0_100(df$health_per_capita * 10)
    ),
    na.rm = TRUE
  )

  # Normalise doctor density onto the same 0-100 scale as the other three
  # components. log1p first: the raw distribution is heavily right-skewed.
  res_bounds <- get_reference_bounds(reference, "resources_raw")
  df$resources_raw <- log1p(df$doctor_density)
  df$resources <- 100 * normalise_quantiles(df$resources_raw,
                                            q01 = res_bounds$q01,
                                            q99 = res_bounds$q99)

  df$services <- df$haqi

  raw_components <- c("governance", "financing", "resources", "services")
  raw_geometric <- row_geometric_mean(df, raw_components, min_n = min_n)
  inverted_geom <- invert_0_100(raw_geometric)

  bounds <- get_reference_bounds(reference, "capacity_raw")
  df$capacity_raw <- inverted_geom
  df$capacity_score <- normalise_quantiles(inverted_geom,
                                           q01 = bounds$q01,
                                           q99 = bounds$q99)

  df
}

#' Compute Overall Risk Score
#'
#' Calculates the overall (pre-crisis) risk score as the geometric mean of
#' \code{hazard_score}, \code{vulnerability_score} and \code{capacity_score}.
#'
#' @param df A data frame containing the three component scores.
#' @param min_n Minimum number of non-missing components required. Defaults to
#'   3, i.e. all three, so that a country missing an entire dimension is
#'   reported as `NA` rather than scored on the remainder.
#'
#' @return The input data frame with an added column \code{overall_risk}.
#' @export
add_overall_risk <- function(df, min_n = 3) {
  df$overall_risk <- row_geometric_mean(
    df,
    c("hazard_score", "vulnerability_score", "capacity_score"),
    min_n = min_n
  )
  df
}

#' Indicator sets underlying each risk component
#'
#' Declares which input indicators feed each component. Used by
#' [add_data_completeness()] to count how much of the evidence base is actually
#' present for a given country.
#'
#' @return A named list of character vectors.
#' @export
default_indicator_sets <- function() {
  list(
    hazard = c("All causes"),
    vulnerability = c("soc_econ_vulnerability", "connectivity", "electricity",
                      "internet", "mobile", "road_density", "improved_water",
                      "improved_sanitation", "uprooted", "food_sec",
                      "adult_literacy"),
    capacity = c("gov_effectivess", "uhc_coverage", "health_per_capita",
                 "doctor_density", "haqi")
  )
}

#' Flag countries with insufficient underlying data
#'
#' Counts missing input indicators per component and flags countries whose score
#' rests on too thin an evidence base.
#'
#' This matters because data sparsity correlates with fragility: without an
#' explicit flag, the countries most likely to be scored from a partial variable
#' set are exactly the high-risk ones, and their scores are silently not
#' comparable with those of well-measured countries.
#'
#' @param df A data frame of merged indicators.
#' @param indicator_sets Named list of indicator names per component. Defaults
#'   to [default_indicator_sets()].
#' @param max_missing Maximum number of missing indicators tolerated before a
#'   country is flagged as low confidence.
#' @param max_missing_component Maximum proportion of a single component's
#'   indicators that may be missing before the country is flagged, regardless of
#'   the overall count.
#'
#' @return The input data frame with added columns `n_missing_<component>`,
#'   `n_missing_total`, `data_completeness` (proportion present in `[0, 1]`) and
#'   `low_confidence` (logical).
#' @export
add_data_completeness <- function(df,
                                  indicator_sets = default_indicator_sets(),
                                  max_missing = 3,
                                  max_missing_component = 0.5) {

  n_total <- 0
  n_missing_total <- rep(0, nrow(df))
  component_breach <- rep(FALSE, nrow(df))

  for (comp in names(indicator_sets)) {
    cols <- intersect(indicator_sets[[comp]], names(df))
    absent <- setdiff(indicator_sets[[comp]], names(df))
    if (length(absent) > 0) {
      warning(
        "add_data_completeness(): indicators not found in data and treated ",
        "as missing for every country: ", paste(absent, collapse = ", "),
        call. = FALSE
      )
    }

    n_comp <- length(indicator_sets[[comp]])
    n_total <- n_total + n_comp

    if (length(cols) > 0) {
      n_miss <- rowSums(is.na(df[, cols, drop = FALSE]))
    } else {
      n_miss <- rep(0, nrow(df))
    }
    n_miss <- n_miss + length(absent)

    df[[paste0("n_missing_", comp)]] <- n_miss
    n_missing_total <- n_missing_total + n_miss
    component_breach <- component_breach | (n_miss / n_comp >
                                              max_missing_component)
  }

  df$n_missing_total <- n_missing_total
  df$data_completeness <- 1 - (n_missing_total / n_total)
  df$low_confidence <- (n_missing_total > max_missing) | component_breach

  df
}

#' Add localised risk to country-level risk data
#'
#' @description
#' This function merges proximity adjustment scores with a country-level risk table
#' and computes a new column `localised_risk` using an odds-based adjustment.
#'
#' @param df A tibble with one row per country, including `ISO_A3` and `severity_adjusted_risk`
#' @param proximity_adjustments A tibble with columns: ISO_A3, disease_score, outbreak_score, people_score, final_score
#' @export
#'
#' @return A tibble with all original columns plus `localised_risk`
#'
#' @examples
#' df <- tibble::tibble(
#'   ISO_A3 = c("NGA", "TUV", "UGA"),
#'   severity_adjusted_risk = c(0.5, 0.4, 0.6)
#' )
#'
#' proximity_adjustments <- tibble::tibble(
#'   ISO_A3 = c("NGA", "TUV", "UGA"),
#'   disease_score = c(0.3, 0.3, 0.3),
#'   outbreak_score = c(0.0892, 0.0892, 0.0892),
#'   people_score = c(0.301, 0.301, 0.301),
#'   final_score = c(0.690, 0.690, 0.690)
#' )
#'
#' add_localised_risk(df, proximity_adjustments)
add_localised_risk <- function(df, proximity_adjustments) {
  proximity_adjustments |>
    dplyr::left_join(df, by = "ISO_A3") |>
    dplyr::rowwise() |>
    dplyr::mutate(
      localised_risk = {
        baseline <- severity_adjusted_risk
        adjustment <- final_score

        # Edge case handling
        if (baseline <= 0) baseline <- baseline + 0.01
        if (baseline >= 1) baseline <- baseline - 0.01

        odds <- baseline / (1 - baseline)
        adjusted_odds <- odds + adjustment
        adjusted_odds / (1 + adjusted_odds)
      }
    ) |>
    dplyr::ungroup()
}


#' Add proximity adjustment risk scores from a reporting form
#'
#' @description
#' Calculates risk adjustment scores for each row in a reporting form based on:
#' - Disease-specific delay responses (diagnosis and transport)
#' - Outbreak concern responses
#' - Population-based score using log-transformed local residents, expatriates, and families
#'
#' The final score is the mean of:
#' - Disease-specific score (±0.1 per matched delay question)
#' - Outbreak concern score (weighted by concern level and outbreak proportions)
#' - People concern score: log10-transformed population values adjusted and summed
#'
#' @param long A tibble: disease, question, response, n, prop
#' @param form A tibble: reporting form rows
#' @param threshold Numeric, threshold for disease scoring (default 0.5)
#' @return Tibble with: ISO_A3, disease_score, outbreak_score, people_score, final_score
#' @export
add_proximity_score <- function(long, form, threshold = 0.5) {
  disease_columns <- c(
    "Acute Abdominal Pain",
    "Non-traumatic Chest Pain",
    "Focal neurological symptoms suggestive of Stroke",
    "Sudden Febrile illness in malaria-prone area",
    "Resting dyspnea"
  )
  delay_columns <- paste0(disease_columns, "2")

  outbreak_pool <- c(
    "Food/Waterborne Outbreak Risk",
    "STDs Transmission Risk",
    "Hemorrhagic Fever Risk",
    "Hemorrhagic Fever Outbreak Concern",
    "Zoonoses Concern",
    "Vector-borne Disease Risk",
    "Airborne Disease Risk",
    "Airborne Disease Outbreak"
  )
  outbreak_cols <- base::intersect(outbreak_pool, base::names(form))

  purrr::map_dfr(seq_len(nrow(form)), function(i) {
    row <- form[i, ]
    disease_score <- 0
    unmatched <- c()

    # Disease-specific scoring: diagnosis delays
    for (j in seq_along(disease_columns)) {
      disease <- disease_columns[j]
      # Diagnosis delay column
      if (disease %in% base::names(row)) {
        question_diag <- row[[disease]]
        if (!is.na(question_diag) && !is.null(question_diag) && question_diag != "") {
          prop_diag <- long |>
            dplyr::filter(disease == !!disease, question == !!question_diag, response == "Very concerned") |>
            dplyr::pull(prop)
          if (length(prop_diag) == 1 && !is.na(prop_diag)) {
            disease_score <- disease_score + base::ifelse(prop_diag > threshold, 0.1, -0.1)
          } else {
            unmatched <- c(unmatched, paste0("Disease: ", disease, " / Diagnosis: ", question_diag))
          }
        }
      }
      # Transport delay column
      delay_col <- delay_columns[j]
      if (delay_col %in% base::names(row)) {
        question_trans <- row[[delay_col]]
        if (!is.na(question_trans) && !is.null(question_trans) && question_trans != "") {
          prop_trans <- long |>
            dplyr::filter(disease == !!disease, question == !!question_trans, response == "Very concerned") |>
            dplyr::pull(prop)
          if (length(prop_trans) == 1 && !is.na(prop_trans)) {
            disease_score <- disease_score + base::ifelse(prop_trans > threshold, 0.1, -0.1)
          } else {
            unmatched <- c(unmatched, paste0("Disease: ", disease, " / Transport: ", question_trans))
          }
        }
      }
    }
    disease_score <- base::round(disease_score, 3)

    # Outbreak concern score
    outbreak_score <- 0
    for (outbreak in outbreak_cols) {
      response <- if (outbreak %in% base::names(row)) base::as.character(row[[outbreak]]) else NA
      if (is.na(response) || response == "") next
      prop_vc <- long |>
        dplyr::filter(
          disease == "Outbreak risk",
          question == outbreak,
          response == "Very concerned"
        ) |>
        dplyr::pull(prop)
      prop_cf <- long |>
        dplyr::filter(
          disease == "Outbreak risk",
          question == outbreak,
          response == "Confirmed"
        ) |>
        dplyr::pull(prop)
      prop_vc <- base::ifelse(length(prop_vc) == 1, prop_vc, 0)
      prop_cf <- base::ifelse(length(prop_cf) == 1, prop_cf, 0)
      weight <- dplyr::case_when(
        response == "Non endemic but an outbreak is plausible" ~ 0.25 * prop_vc,
        response == "Endemic" ~ 0.5 * prop_vc,
        response == "Endemic with high risk of outbreak" ~ 1.0 * prop_vc,
        response == "Endemic with frequent outbreaks" ~ prop_cf,
        TRUE ~ 0
      )
      outbreak_score <- outbreak_score + weight
    }
    outbreak_score <- outbreak_score / 6

    # People score
    local <- base::suppressWarnings(base::log10(base::as.numeric(row[["Approximate Number of Local Residents"]])))
    expat <- base::suppressWarnings(base::log10(base::as.numeric(row[["Approximate Number of Expatriates"]])))
    families <- base::suppressWarnings(base::log10(base::as.numeric(row[["Approximate Number of Expatriate Families"]])))
    local_adj <- base::max(0, local - 3)
    expat_adj <- base::max(0, expat - 2)
    families_adj <- base::min(expat_adj, base::max(0, families - 2))
    people_score <- base::mean(c(local_adj, expat_adj, families_adj), na.rm = TRUE)
    if (!base::is.finite(people_score)) people_score <- 0

    if (length(unmatched) > 0) {
      base::warning(sprintf("Row %d - unmatched inputs: %s", i, paste(unmatched, collapse = "; ")))
    }

    tibble::tibble(
      ISO_A3 = row[["ISO_A3"]],
      disease_score = disease_score,
      outbreak_score = outbreak_score,
      people_score = people_score,
      final_score = base::mean(c(disease_score, outbreak_score, people_score))
    )
  })
}
