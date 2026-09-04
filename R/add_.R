## SCORE CALCULATION

#' Compute Composite Vulnerability Score
#'
#' Calculates a composite vulnerability score using indicators from WHO and
#' INFORM datasets.
#' The score is based on four conceptual components:
#' \itemize{
#'   \item \strong{Socioeconomic vulnerability}: \code{soc_econ_vulnerability},
#'   inverted on a 0–10 scale.
#'   \item \strong{Infrastructure}: Computed as the geometric mean of:
#'     \itemize{
#'       \item \code{comms} — average of \code{connectivity}, \code{electricity},
#'        \code{internet}, \code{mobile}
#'       \item \code{physical} — average of \code{road_density},
#'        \code{improved_water}, \code{improved_sanitation}
#'     }
#'     Both components are inverted on a 0–10 scale before aggregation.
#'   \item \strong{Vulnerable groups}: Geometric mean of \code{uprooted} and
#'   \code{food_sec}, inverted on a 0–10 scale.
#'   \item \strong{Education}: \code{adult_literacy}, inverted on a 0–10 scale.
#' }
#'
#' The final vulnerability score is the normalized geometric mean of the four
#' components.
#'
#' @param df A data frame with the required columns for vulnerability
#' computation.
#'
#' @return The input data frame with added columns:
#' \itemize{
#'   \item \code{vulnerable_groups}, \code{comms}, \code{physical},
#'   \code{infrastructure}
#'   \item \code{vulnerability_score} — normalized composite score.
#' }
#' @export
add_vulnerability_score <- function(df) {

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

  df$infrastructure <- apply(
    df[, c("comms", "physical")] |>
      dplyr::mutate(dplyr::across(dplyr::everything(), invert_0_10)),
    1,
    geometric_mean
  )

  # Final vulnerability components
  raw_components <- df[, c("infrastructure",
                           "adult_literacy",
                           "vulnerable_groups",
                           "soc_econ_vulnerability")]

  # Apply inversion only to columns not already inverted
  raw_components$adult_literacy <- invert_0_10(raw_components$adult_literacy)
  raw_components$soc_econ_vulnerability <- invert_0_10(
    raw_components$soc_econ_vulnerability)
  raw_components$vulnerable_groups <- invert_0_10(
    raw_components$vulnerable_groups)

  raw_geom <- apply(raw_components, 1, geometric_mean)
  inverted_geom <- invert_0_10(raw_geom)
  df$vulnerability_score <- normalise_quantiles(inverted_geom)

  return(df)
}

#' Compute Hazard Score from GBD Rates
#'
#' Calculates a normalized hazard score based on the "All causes" DALY rate from
#'  GBD data.
#' The score is computed by dividing each value by the maximum observed value.
#'
#' @param df A data frame containing a column named \code{"All causes"}.
#'
#' @return The input data frame with an added column \code{hazard_score}.
#' @export
add_hazard_score <- function(df) {
  df$hazard_score <- df$"All causes"/max(df$"All causes", na.rm = TRUE)
  return(df)
}


#' Compute Health System Capacity Score
#'
#' Calculates a composite health system capacity score using four components:
#' \itemize{
#'   \item \strong{Governance}: Inverted \code{gov_effectivess} (scaled from
#'   0–10 to 0–100).
#'   \item \strong{Financing}: Average of \code{uhc_coverage} and inverted
#'   \code{health_per_capita} (scaled to 0–100).
#'   \item \strong{Resources}: \code{doctor_density}.
#'   \item \strong{Services}: \code{haqi}.
#' }
#'
#' The final score is the normalized geometric mean of the four components.
#'
#' @param df A data frame with the required columns for capacity computation.
#'
#' @return The input data frame with added columns:
#' \itemize{
#'   \item \code{governance}, \code{financing}, \code{resources}, \code{services}
#'   \item \code{capacity_score} — normalized composite score.
#' }
#' @export
add_capacity_score <- function(df) {
  df$governance <- invert_0_100(df$gov_effectivess * 10)

  df$financing <- rowMeans(
    cbind(
      df$uhc_coverage,
      invert_0_100(df$health_per_capita * 10)
    ),
    na.rm = TRUE
  )
  df$resources <- df$doctor_density
  df$services <- df$haqi

  # Compute geometric mean of raw components
  raw_components <- df[, c("governance", "financing", "resources", "services")]
  raw_geometric <- apply(raw_components, 1, geometric_mean)
  inverted_geom <- invert_0_100(raw_geometric)

  # Normalize the final score
  df$capacity_score <- normalise_quantiles(inverted_geom)

  return(df)
}

#' Compute Overall Risk Score
#'
#' Calculates the overall risk score as the geometric mean of:
#' \itemize{
#'   \item \code{hazard_score}
#'   \item \code{vulnerability_score}
#'   \item \code{capacity_score}
#' }
#'
#' This score reflects the combined impact of hazard, vulnerability, and system
#'  capacity.
#'
#' @param df A data frame containing the three component scores.
#'
#' @return The input data frame with an added column \code{overall_risk}.
#' @export
add_overall_risk <- function(df) {
  df$overall_risk <- apply(
    df[, c("hazard_score", "vulnerability_score", "capacity_score")],
    1,
    geometric_mean
  )

  return(df)
}

#' Compute Severity-Adjusted Risk Score
#'
#' Adjusts the overall risk score using the INFORM Severity Index.
#' The adjustment formula is:
#' \deqn{overall\_risk + ((severity\_index - 1) / 4) * (1 - overall\_risk)}
#' This increases the risk score proportionally to the severity index
#' (range 1–5).
#'
#' @param df A data frame with columns \code{overall_risk} and
#' \code{severity_index}.
#'
#' @return The input data frame with an added column
#'  \code{severity_adjusted_risk}.
#' @export
add_severity <- function(df) {
  df$severity_adjusted_risk <- update_score(
    base_value = df$overall_risk,
    multiplier = df$severity_index
  )
  return(df)
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
