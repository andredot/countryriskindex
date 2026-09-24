
# Here below put your main project's functions ---------------------


#' Null function
#'
#' Use this as template for your first function, and delete it!
#'
#' @note You can add all this documentation infrastructure pressing
#'   `CTRL + SHIFT + ALT + R` from anywhere inside the function's body.
#'
#' @param x (default, NULL)
#'
#' @return NULL
#'
#' @examples
#' \dontrun{
#'   null()
#'   null(1)
#' }
null <- function(x = NULL) {
  if (!is.null(x)) NULL else x
}

#' Merge Global Health and Vulnerability Datasets
#'
#' Merges multiple datasets containing health and risk indicators by ISO3
#' country codes.
#' The function:
#' \itemize{
#'   \item Removes `location_name` columns from GBD and HAQ datasets.
#'   \item Renames ISO code columns in INFORM and WHO datasets to `ISO_A3`.
#'   \item Merges all datasets using left joins on `ISO_A3`.
#'   \item Resolves duplicate `country` columns by coalescing them into one.
#' }
#'
#' @param gbd_rates A data frame with GBD rates and `ISO_A3` codes.
#' @param haq_index A data frame with HAQ index values and `ISO_A3` codes.
#' @param inform_cap A data frame with INFORM indicators and `iso3` codes.
#' @param who_indicators A data frame with WHO indicators and `iso_a3` codes.
#' @param inform_severity A data frame with INFORM severity index and `ISO3`
#' codes.
#'
#' @return A merged data frame with all indicators joined by `ISO_A3`.
#' @export
merge_health_datasets <- function(gbd_rates,
                                  haq_index,
                                  inform_cap,
                                  who_indicators,
                                  inform_severity) {
  merged <- gbd_rates |>
    dplyr::select(-location_name) |>
    dplyr::left_join(
      haq_index |> dplyr::select(-location_name),
      by = "ISO_A3"
    ) |>
    dplyr::left_join(
      inform_cap |> dplyr::rename(ISO_A3 = iso3),
      by = "ISO_A3"
    ) |>
    dplyr::left_join(
      inform_severity |> dplyr::rename(ISO_A3 = ISO3),
      by = "ISO_A3",
      # INFORM Severity can list several crises per country. A many-to-many
      # join would silently duplicate country rows and corrupt every downstream
      # aggregate, so fail loudly instead.
      relationship = "one-to-one"
    ) |>
    dplyr::left_join(
      who_indicators |> dplyr::rename(ISO_A3 = iso_a3),
      by = "ISO_A3"
    ) |>
    dplyr::mutate(country = dplyr::coalesce(country.y, country.x)) |>
    dplyr::select(-country.y, -country.x)

  return(merged)
}

#' Extract Radar Data for Selected Health Indicators
#'
#' Prepares a subset of health indicators for radar chart visualization.
#' The function:
#' \itemize{
#'   \item Aggregates communicable disease indicators into a single score.
#'   \item Aggregates injury-related indicators into a single score.
#'   \item Combines water and sanitation indicators into a WASH score.
#'   \item Includes capacity and vulnerability scores.
#'   \item Normalizes all scores to a 0–1 scale by dividing by their column
#'   maxima.
#' }
#'
#' @param df A data frame with health and risk indicators, including a `country`
#'  column.
#'
#' @return A normalized data frame with radar-specific indicators and a `Country`
#'  column.
#' @export
extract_radar_data <- function(df, cause_groups = default_cause_groups()) {

  sum_present <- function(data, cols, label) {
    present <- intersect(cols, names(data))
    absent <- setdiff(cols, names(data))
    if (length(absent) > 0) {
      warning(
        "extract_radar_data(): causes not found in the GBD extract and ",
        "omitted from '", label, "': ", paste(absent, collapse = ", "),
        call. = FALSE
      )
    }
    if (length(present) == 0) {
      return(rep(NA_real_, nrow(data)))
    }
    rowSums(as.matrix(data[, present, drop = FALSE]), na.rm = TRUE)
  }

  out <- tibble::tibble(
    Country = df[["country"]],
    `Communicable diseases` = sum_present(df, cause_groups$communicable,
                                          "Communicable diseases"),
    `Traumatic/Violent injuries` = sum_present(df, cause_groups$injuries,
                                               "Traumatic/Violent injuries"),
    WASH = rowMeans(
      cbind(df[["improved_water"]], df[["improved_sanitation"]]),
      na.rm = TRUE
    ),
    Capacity = df[["capacity_score"]],
    Vulnerability = df[["vulnerability_score"]]
  )

  dplyr::mutate(
    out,
    dplyr::across(
      .cols = -Country,
      .fns = function(x) {
        m <- suppressWarnings(max(x, na.rm = TRUE))
        if (!is.finite(m) || m == 0) x else x / m
      }
    )
  )
}

#' Default GBD cause groupings for the radar chart
#'
#' Maps level-2 GBD cause names onto the thematic channels used by the radar
#' chart and the crisis modifier matrix. Causes absent from the extract are
#' skipped with a warning rather than causing an error, so the grouping survives
#' GBD renaming causes between rounds.
#'
#' @return A named list of character vectors.
#' @export
default_cause_groups <- function() {
  list(
    communicable = c(
      "Respiratory infections and tuberculosis",
      "Enteric infections",
      "Neglected tropical diseases and malaria",
      "Sexually transmitted infections",
      "HIV/AIDS and sexually transmitted infections",
      "Other infectious diseases"
    ),
    injuries = c(
      "Transport injuries",
      "Unintentional injuries",
      "Self-harm and interpersonal violence",
      "Violence injuries",
      "Other injuries"
    ),
    noncommunicable = c(
      "Cardiovascular diseases",
      "Neoplasms",
      "Chronic respiratory diseases",
      "Diabetes and kidney diseases",
      "Mental disorders",
      "Substance use disorders",
      "Digestive diseases",
      "Neurological disorders"
    )
  )
}

#' Adjust Radar Data Using Crisis Modifier Drivers
#'
#' Updates radar chart indicators by adjusting them based on crisis drivers and
#'  severity.
#' The function:
#' \itemize{
#'   \item Selects relevant columns from the risk score data.
#'   \item Computes a CoPA-IotC score from people conditions and crisis impact.
#'   \item Replaces missing values with defaults.
#'   \item Renames driver columns and ensures all logical indicators from
#'   `cm_data` are present.
#'   \item For each driver, increments logical indicators based on presence.
#'   \item Applies the `update_score()` function to adjust radar indicators
#'   using CoPA-IotC and driver counts.
#' }
#'
#' @param cm_data A data frame with driver names and associated logical
#' indicators.
#' @param risk_score A data frame with country-level risk and driver data.
#' @param radar_data A data frame with original radar scores.
#'
#' @return A data frame with adjusted radar scores for each country.
#' @export
update_risk_with_cm <- function(cm_data, risk_score, radar_data) {
  # Identify logical columns in cm_data
  logical_cols <- cm_data |>
    dplyr::select(where(is.logical)) |>
    colnames()

  # Get driver columns from cm_data
  driver_cols <- cm_data$Driver

  # Prepare risk_score
  risk_updated <- risk_score |>
    dplyr::select(
      country,
      people_conditions,
      crisis_impact,
      operating_environment,
      `Society and safety`,
      dplyr::starts_with("Driver"),
      # Cause columns are matched, not named: the GBD extract now carries all
      # level-2 causes, whose names differ from the ten v4.00 hard-coded (for
      # instance "Self-harm and interpersonal violence" rather than "Violence
      # injuries"). dplyr::any_of()/matches() tolerate an absent cause instead
      # of erroring, which is what we want when GBD renames one.
      dplyr::matches("infections|malaria|tuberculosis"),
      dplyr::matches("injur|violence|self-harm"),
      improved_water, improved_sanitation,
      capacity_score,
      vulnerability_score
    ) |>
    dplyr::mutate(
      people_conditions = tidyr::replace_na(people_conditions, 1),
      crisis_impact = tidyr::replace_na(crisis_impact, 1),
      operating_environment = tidyr::replace_na(operating_environment, 1),
      `Society and safety` = tidyr::replace_na(`Society and safety`, 1),
      `CoPA-IotC` = 5 - sqrt((5 - people_conditions) * (5 - crisis_impact))
    ) |>
    dplyr::select(-people_conditions, -crisis_impact) |>
    dplyr::mutate(dplyr::across(
      .cols = dplyr::starts_with("Driver"),
      .fns = ~ tidyr::replace_na(., 0)
    )) |>
    dplyr::rename_with(
      .fn = ~ stringr::str_remove(., "^Driver "),
      .cols = dplyr::starts_with("Driver")
    )

  # Add missing logical columns to risk_score, initialized to 0
  for (col in logical_cols) {
    if (!col %in% colnames(risk_updated)) {
      risk_updated[[col]] <- 1
    }
  }

  # For each driver column in risk_score
  for (driver in driver_cols) {
    if (driver %in% colnames(risk_updated)) {
      # Get logical values from cm_data for this driver
      cm_row <- cm_data |>
        dplyr::filter(.data$Driver == driver)

      # Update logical columns in risk_score where driver == 1
      for (col in logical_cols) {
        risk_updated[[col]] <- risk_updated[[col]] +
          ifelse(risk_updated[[driver]] == 1, as.integer(cm_row[[col]]), 0)
      }
    }
  }

  radar_corrected <- risk_updated |>
    dplyr::transmute(
      Country = country,
      `Communicable diseases` =
        update_score(base_value = radar_data$`Communicable diseases`,
                    multiplier = risk_updated$`CoPA-IotC`,
                    times = risk_updated$`Communicable diseases`),
      `Traumatic/Violent injuries` = update_score(
                     base_value = radar_data$`Traumatic/Violent injuries`,
                     multiplier = `CoPA-IotC`,
                     times = risk_updated$`Traumatic/Violent injuries`),
      WASH = update_score(base_value = radar_data$WASH,
                          multiplier = `CoPA-IotC`,
                          times = risk_updated$WASH),
      Capacity = update_score(capacity_score, operating_environment),
      Vulnerability = update_score(vulnerability_score, `Society and safety`)
    )

  return(radar_corrected)
}

#' Update a Score Based on a Severity Multiplier and Repetition Count
#'
#' Adjusts a base score using a 1-5 severity multiplier, applied `times` times:
#'
#' \deqn{score \leftarrow score + \frac{m - 1}{4}(1 - score)}
#'
#' Note that this is a convex pull towards 1: at `m = 5` a single application
#' returns exactly 1 regardless of the base value. It is retained for the radar
#' chart, where the intent is a saturating visual emphasis, but it is no longer
#' used for the headline index - see [add_severity()] and the log-odds
#' [logit_shift()].
#'
#' The v4.0 implementation contained an off-by-one: `times` was decremented
#' whenever it exceeded 1, so `times = 1` and `times = 2` behaved identically
#' and `times = 3` and above collapsed to two applications. v4.1 applies the
#' adjustment exactly `times` times, capped at `max_times`.
#'
#' @param base_value A numeric vector of base scores in `[0, 1]`.
#' @param multiplier A numeric vector of severity multipliers in `[1, 5]`. `NA`
#'   leaves the score unchanged.
#' @param times An integer vector giving how many times to apply the adjustment.
#'   Values below 0 are treated as 0; `NA` is treated as 0.
#' @param max_times Maximum number of applications (default 3).
#'
#' @return A numeric vector of adjusted scores.
#' @export
#'
#' @examples
#' update_score(base_value = c(0.6, 0.7), multiplier = c(3, NA), times = c(2, 1))
#' # times is now honoured exactly:
#' update_score(0.5, 3, times = 1) < update_score(0.5, 3, times = 2)
update_score <- function(base_value, multiplier, times = 1, max_times = 3) {
  times <- as.numeric(times)
  times[is.na(times)] <- 0
  times <- pmin(pmax(round(times), 0), max_times)

  # Recycled to a common length, then applied element-wise. Base R rather than
  # purrr::pmap_dbl(): this is on the hot path and needs no dependency.
  n <- max(length(base_value), length(multiplier), length(times))
  base_value <- rep_len(base_value, n)
  multiplier <- rep_len(multiplier, n)
  times <- rep_len(times, n)

  vapply(seq_len(n), function(i) {
    b <- base_value[i]
    m <- multiplier[i]
    t <- times[i]
    if (is.na(b)) return(NA_real_)
    if (is.na(m)) return(as.numeric(b))
    score <- as.numeric(b)
    for (k in seq_len(t)) {
      score <- score + ((m - 1) / 4) * (1 - score)
    }
    score
  }, numeric(1))
}

#' Process AMI survey responses enforcing monotonic concern by group
#'
#' @description
#' Enforces non-decreasing concern within two groups:
#' (1) diagnosis delay (first 4 columns) and (2) transport delay (next 4 columns).
#' The last two columns (health/wellbeing) are independent.
#'
#' Within each of the first two groups and within each row, scanning left-to-right,
#' any entry lower than the highest seen so far is raised to that highest. `NA`s are
#' treated the same (filled with the highest seen so far). Leading `NA`s remain `NA`
#' until a level appears later in that group.
#'
#' "Not relevant for this condition" (case-insensitive) is converted to `NA` by default.
#' All columns are returned as ordered factors with the same `concern_levels`.
#'
#' @param data A data frame containing the 10 AMI columns (in any order).
#' @param diag_cols Columns (names or positions) for the diagnosis delay group (length 4).
#'                  Default: first 4 columns.
#' @param transport_cols Columns (names or positions) for the transport delay group (length 4).
#'                       Default: next 4 columns.
#' @param impact_cols Columns (names or positions) for health/wellbeing (length 2).
#'                    Default: last 2 columns.
#' @param concern_levels Character vector of ordered concern levels (low → high).
#' @param treat_not_relevant_as_na Logical; turn strings with "Not relevant" into `NA` (default TRUE).
#'
#' @return A data frame with the same columns, all as ordered factors with uniform levels,
#'         and with monotonic constraints enforced within the first two groups.
#'
process_ami_survey <- function(
    data,
    diag_cols = 1:4,
    transport_cols = 5:8,
    impact_cols = 9:10,
    concern_levels = c(
      "Very unconcerned",
      "Somewhat unconcerned",
      "Neither concerned nor unconcerned",
      "Somewhat concerned",
      "Very concerned"
    ),
    treat_not_relevant_as_na = TRUE
) {
  to_names <- function(df, cols) if (is.numeric(cols)) colnames(df)[cols] else as.character(cols)

  enforce_monotone_row <- function(int_vec) {
    max_so_far <- NA_integer_
    out <- int_vec
    for (i in seq_along(int_vec)) {
      val <- int_vec[i]
      if (is.na(max_so_far) && is.na(val)) {
        out[i] <- NA_integer_
      } else {
        if (!is.na(val)) {
          max_so_far <- if (is.na(max_so_far)) val else max(max_so_far, val)
        }
        out[i] <- max_so_far
      }
    }
    out
  }

  enforce_monotone_group <- function(df, cols, levels_vec) {
    if (length(cols) == 0) return(df)
    mat <- df[, cols, drop = FALSE] |>
      dplyr::mutate(dplyr::across(
        dplyr::everything(),
        ~ as.integer(factor(.x, levels = levels_vec))
      )) |>
      as.matrix()

    new_mat <- if (nrow(mat) > 0) t(apply(mat, 1, enforce_monotone_row)) else mat

    new_df <- as.data.frame(new_mat, stringsAsFactors = FALSE)
    colnames(new_df) <- cols
    new_df <- new_df |>
      dplyr::mutate(dplyr::across(
        dplyr::everything(),
        ~ factor(.x, levels = seq_along(levels_vec), labels = levels_vec, ordered = TRUE)
      ))
    df[cols] <- new_df
    df
  }

  # 1) Optional: normalize "Not relevant ..." to NA
  if (treat_not_relevant_as_na) {
    data <- data |>
      dplyr::mutate(dplyr::across(
        dplyr::everything(),
        ~ dplyr::if_else(
          stringr::str_detect(as.character(.x), stringr::regex("not\\s*relevant", ignore_case = TRUE)),
          NA_character_,
          as.character(.x)
        )
      ))
  }

  # 2) Coerce all to ordered factors using the provided levels (values not in levels → NA)
  data <- data |>
    dplyr::mutate(dplyr::across(
      dplyr::everything(),
      ~ factor(.x, levels = concern_levels, ordered = TRUE)
    ))

  diag_cols      <- to_names(data, diag_cols)
  transport_cols <- to_names(data, transport_cols)
  impact_cols    <- to_names(data, impact_cols)

  if (length(diag_cols) != 4L) stop("`diag_cols` must contain exactly 4 columns.")
  if (length(transport_cols) != 4L) stop("`transport_cols` must contain exactly 4 columns.")
  if (length(impact_cols) != 2L) stop("`impact_cols` must contain exactly 2 columns.")

  # 3) Enforce monotonicity within diagnosis and transport groups
  data <- enforce_monotone_group(data, diag_cols, concern_levels)
  data <- enforce_monotone_group(data, transport_cols, concern_levels)

  # 4) Final hardening: **uniform ordered levels across all columns**
  data |>
    dplyr::mutate(dplyr::across(
      dplyr::everything(),
      ~ factor(as.character(.x), levels = concern_levels, ordered = TRUE)
    ))
}

#' Pivot the entire survey into long format with disease and question info
#'
#' @description
#' Converts columns 12 to 151 of the survey into long format.
#' Adds disease and question identifiers, calculates proportions.
#'
#' @param survey A tibble with the full survey (154 columns).
#' @param concern_levels Ordered vector of concern levels (low → high).
#' @param titles A character vector of 14 disease titles.
#' @param outbreak_labels A character vector of outbreak question labels.
#' @param question_labels A character vector of 10 question titles.
#'
#' @return A tibble with columns: disease, question, response, prop.
#' @export
pivot_disease_data <- function(
    survey,
    concern_levels = c(
      "Very unconcerned",
      "Somewhat unconcerned",
      "Neither concerned nor unconcerned",
      "Somewhat concerned",
      "Very concerned"
    ),
    titles = c(
      "Non-traumatic Chest Pain", "Snakebite", "Sudden Febrile illness in malaria-prone area",
      "Sudden Febrile illness in malaria-free area", "Mental Health Crisis",
      "Focal neurological symptoms suggestive of Stroke", "Open Wounds with Hemorrage",
      "Acute Abdominal Pain", "Lower Back Pain suggestive of renal stones",
      "Head injury or Polytrauma", "Resting dyspnea",
      "Coma", "Second degree burns", "Eye emergency"
    ),
    outbreak_labels = c(
      "Food/Waterborne Outbreak Risk A",
      "Food/Waterborne Outbreak Risk C",
      "STDs Transmission Risk A",
      "STDs Transmission Risk C",
      "Hemorrhagic Fever Outbreak Concern A",
      "Hemorrhagic Fever Outbreak Concern C",
      "Zoonoses Concern A",
      "Vector-borne Disease Risk A",
      "Vector-borne Disease Risk C",
      "Airborne Disease Risk A",
      "Airborne Disease Risk C"
    ),
    question_labels = c(
      "15m diagnosis delay", "1h diagnosis delay", "4h diagnosis delay", "8h diagnosis delay",
      "1h transport delay", "4h transport delay", "8h transport delay", "1 day transport delay",
      "Patient life impact", "Patient wellbeing impact"
    )
) {
  stopifnot(length(titles) == 14)
  stopifnot(length(question_labels) == 10)

  long_all <- purrr::map2_dfr(seq_along(titles), titles, function(i, disease_name) {
    start_col <- 12 + (i - 1) * 10
    end_col <- start_col + 9
    data_block <- survey[, start_col:end_col]
    colnames(data_block) <- question_labels

    data_block |>
      dplyr::mutate(disease = disease_name) |>
      tidyr::pivot_longer(
        cols = all_of(question_labels),
        names_to = "question",
        values_to = "response"
      ) |>
      dplyr::mutate(
        response = factor(response, levels = concern_levels, ordered = TRUE),
        question = factor(question, levels = question_labels, ordered = TRUE)
      )
  }) |>
    dplyr::count(disease, question, response, name = "n") |>
    tidyr::complete(
      disease, question,
      response = factor(concern_levels, levels = concern_levels, ordered = TRUE),
      fill = list(n = 0)
    ) |>
    dplyr::group_by(disease, question) |>
    dplyr::mutate(prop = n / sum(n)) |>
    dplyr::ungroup() |>
    dplyr::mutate(question = as.character(question))


  # Pivot outbreak concern columns
  outbreak_block <- survey[, 1:11]
  colnames(outbreak_block) <- outbreak_labels
  long_outbreak <- summarise_outbreak_data(outbreak_block)

  # Combine both
  dplyr::bind_rows(long_all, long_outbreak)
}

#' Summarise outbreak concern proportions with counts
#'
#' @description
#' This function:
#' - Computes the number and proportion of "Very concerned" responses per column
#' - Splits column names into disease and response type
#' - Relabels type A as "Very concerned" and type C as "Confirmed"
#' - Ensures Zoonoses - Confirmed matches Zoonoses - Very concerned
#'
#' @param outbreak_block A tibble with multiple outbreak concern columns
#'
#' @return A tibble with columns: disease, question, response, n, prop
#' @export
summarise_outbreak_data <- function(outbreak_block) {
  # Step 1: Compute counts and proportions
  proportions <- purrr::map_dfr(names(outbreak_block), function(col_name) {
    values <- outbreak_block[[col_name]]
    total <- length(values)
    very_concerned <- sum(values == "Very concerned", na.rm = TRUE)
    tibble::tibble(
      col_name,
      n = total,
      prop = very_concerned / total
    )
  })

  # Step 2: Split column names into disease and response
  proportions_clean <- proportions |>
    tidyr::separate(col_name,
                    into = c("question", "response"),
                    sep = " (?=[A-Z]$)",
                    remove = TRUE) |>
    dplyr::mutate(
      disease = "Outbreak risk",
      response = dplyr::case_when(
        response == "A" ~ "Very concerned",
        response == "C" ~ "Confirmed",
        TRUE ~ response
      )
    )

  # Step 3: Ensure Zoonoses - Confirmed matches Zoonoses - Very concerned
  zoonoses_row <- proportions_clean |>
    dplyr::filter(question == "Zoonoses Concern", response == "Very concerned")

  proportions_clean <- dplyr::add_row(
      proportions_clean,
      disease = "Outbreak risk",
      question = "Zoonoses Concern",
      response = "Confirmed",
      n = zoonoses_row$n,
      prop = zoonoses_row$prop
  )

  return(proportions_clean |>
           dplyr::select(disease, question, response, n, prop))
}
