#' Preprocess Under-5 Mortality Rate Data
#'
#' Filters and processes under-five mortality rate data from the UN IGME
#' database.
#' The function:
#' \itemize{
#'   \item Filters for total population, valid observation status, and relevant
#'    wealth quintiles.
#'   \item Removes unnecessary metadata columns.
#'   \item Selects the most recent observation per country and wealth quintile.
#'   \item Calculates the difference in mortality between the highest and lowest
#'    wealth quintiles.
#'   \item Normalizes this disparity using quantile normalization.
#'   \item Filters out aggregate or non-country entries based on REF_AREA codes.
#' }
#'
#' @param db A data frame containing UN IGME under-five mortality rate data.
#'
#' @return A data frame with country-level under-five mortality disparities and
#' normalized indicators.
#' @export
preprocess_u5mr <- function(db = NULL) {
  db |>
    dplyr::filter(
      Indicator == "Under-five mortality rate",
      Sex == "Total",
      `Observation Status` %in% c("Normal value", "Included in IGME"),
      `Wealth Quintile` %in% c("Total", "Lowest","Highest"),
      !is.na(`Observation Value`)
    ) |>
    dplyr::select(
      -`Regional group`,
      -tidyselect::starts_with("Series"),
      -`Age Group of Women`,
      -`Time Since First Birth`,
      -`Definition`,
      -`Interval`,
      -`Indicator`,
      -`Sex`,
      -`Observation Status`,
      -`Unit of measure`
    ) |>
    dplyr::group_by(REF_AREA, `Wealth Quintile`) |>
    dplyr::slice_max(`Reference Date`, n = 1) |>
    dplyr::summarise(
      Country = dplyr::first(`Geographic area`),
      value = mean(`Observation Value`, na.rm = TRUE),
      .groups = "drop"
    ) |>
    tidyr::pivot_wider(
      names_from = `Wealth Quintile`,
      values_from = value
    ) |>
    dplyr::mutate(
      delta_diff = `Highest` - `Lowest`,
      did = delta_diff/mean(delta_diff, na.rm = TRUE),
      delta = normalise_quantiles(delta_diff)
    ) |>
    dplyr::filter(
      !stringr::str_starts(REF_AREA, "UNICEF"),
      !stringr::str_starts(REF_AREA, "UNSDG"),
      !stringr::str_starts(REF_AREA, "WB"),
      !stringr::str_starts(REF_AREA, "WORLD")
    )
}

#' Preprocess GBD DALY Rates by Cause
#'
#' Processes Global Burden of Disease (GBD) DALY data to extract
#' disease-specific rates.
#' The function:
#' \itemize{
#'   \item Maps GBD location IDs to ISO A3 codes.
#'   \item Filters for selected cause IDs and reshapes the data to wide format.
#'   \item Computes a composite "Other NCDs" category by summing DALYs for a
#'   predefined set of causes.
#'   \item Merges the selected causes and the composite category into a single
#'    data frame.
#' }
#'
#' @param db A data frame with GBD DALY data including `location_id`,
#'  `cause_id`, and `val`.
#' @param loc_keys A data frame mapping `Location ID` to `ISO_A3` codes.
#'
#' @return A data frame with DALY rates by cause for each country.
#' @export
preprocess_gbd_rates_by_cause <- function(db = NULL,
                                          loc_keys = NULL) {
  if (is.null(db)) stop(
    "The 'db' argument is NULL. Please provide a valid data frame.")
  if (is.null(loc_keys)) stop(
    "The 'loc_keys' argument is NULL. Please provide a valid data frame.")

  # Define cause IDs and labels
  selected_causes <- c(
    "294" = "All causes",
    "956" = "Respiratory infections and tuberculosis",
    "957" = "Enteric infections",
    "344" = "Neglected tropical diseases and malaria",
    "955" = "Sexually transmitted infections",
    "491" = "Cardiovascular diseases",
    "558" = "Mental disorders",
    "688" = "Transport injuries",
    "717" = "Violence injuries",
    "696" = "Other injuries"
  )

  other_ncd_ids <- c(410, 508, 526, 542, 973, 974, 653, 669, 626, 640)

  # Prepare base data
  base <- loc_keys |>
    dplyr::select(`Location ID`, ISO_A3) |>
    dplyr::right_join(db, by = c("Location ID" = "location_id"))

  # Extract selected causes
  selected <- base |>
    dplyr::filter(cause_id %in% as.numeric(names(selected_causes))) |>
    dplyr::mutate(cause_label = selected_causes[as.character(cause_id)]) |>
    dplyr::select(location_name, ISO_A3, cause_label, val) |>
    tidyr::pivot_wider(names_from = cause_label, values_from = val)

  # Compute Other NCDs
  other_ncds <- base |>
    dplyr::filter(cause_id %in% other_ncd_ids) |>
    dplyr::group_by(location_name, ISO_A3) |>
    dplyr::summarise(`Other NCDs` = sum(val, na.rm = TRUE), .groups = "drop")

  # Combine and return
  dplyr::left_join(selected, other_ncds, by = c("location_name", "ISO_A3"))
}

#' Preprocess HAQ Index Data
#'
#' Extracts the most recent Healthcare Access and Quality (HAQ) Index value per
#' country.
#' The function:
#' \itemize{
#'   \item Filters for the HAQ Index indicator.
#'   \item Selects the most recent year per country.
#'   \item Joins with ISO A3 codes using location metadata.
#'   \item Returns a simplified data frame with country name, ISO code, and HAQ
#'    index value.
#' }
#'
#' @param db A data frame with GBD data including `location_id`, `year_id`, and
#'  `val`.
#' @param loc_keys A data frame with `Location ID` and `ISO_A3` columns.
#'
#' @return A data frame with HAQ Index values and ISO codes.
#' @export
preprocess_haq_index <- function(db = NULL, loc_keys = NULL) {
  loc_keys <- loc_keys |>
    dplyr::select(`Location ID`, ISO_A3)

  if (is.null(db)) stop("The 'db' argument is NULL. Please provide a valid data
                        frame.")
  if (is.null(loc_keys)) stop("The 'loc_keys' argument is NULL. Please provide a
                              valid data frame.")

  db |>
    dplyr::filter(indicator_name == "HAQ Index") |>
    dplyr::group_by(location_id) |>
    dplyr::arrange(desc(year_id), .by_group = TRUE) |>
    dplyr::slice(1) |>
    dplyr::ungroup() |>
    dplyr::left_join(loc_keys, by = c("location_id" = "Location ID")) |>
    dplyr::transmute(
      location_name = location_name,
      ISO_A3 = ISO_A3,
      haqi = val
    )
}

#' Preprocess INFORM Risk and Coping Capacity Data
#'
#' Merges INFORM Risk and Lack of Coping Capacity (LCC) datasets to extract and
#'  transform
#' indicators relevant to vulnerability assessment. The function:
#' \itemize{
#'   \item Extracts numeric indicators from the INFORM Risk dataset.
#'   \item Removes metadata rows from the LCC dataset and converts key
#'    indicators to numeric.
#'   \item Joins both datasets by ISO3 country code.
#' }
#'
#' @param db_risk A data frame with INFORM Risk data.
#' @param db_lcc A data frame with Lack of Coping Capacity data.
#'
#' @return A merged data frame with risk, vulnerability, and infrastructure
#'  indicators.
#' @export
preprocess_inform <- function(db_risk = NULL,
                              db_lcc = NULL) {
  db_risk <- db_risk |>
    dplyr::transmute(
      country = `COUNTRY`,
      iso3 = `ISO3`,
      risk = as.double(`INFORM RISK`),
      hazard = as.double(`HAZARD & EXPOSURE`),
      vulnerability = as.double(`VULNERABILITY`),
      capacity = as.double(`LACK OF COPING CAPACITY`),
      reliability = as.double(`Lack of Reliability (*)`),
      soc_econ_vulnerability = as.double(`Socio-Economic Vulnerability`),# 23-28
      connectivity = as.double(`Physical infrastructure`),               # 48-50
      uprooted = as.double(`Uprooted people`),                           # 29-30
      food_sec = as.double(`Food Security`),                             # 37-40
      gov_effectivess = as.double(`Governance`),                         # 42-43
    )

  db_lcc <- db_lcc |>
    dplyr::slice(-1, -2)

  # All of these are INFORM 0-10 component scores (higher = worse), not the raw
  # underlying indicators. pick_column() asserts that, so a workbook layout
  # change fails at import instead of silently mixing scales.
  lcc_range <- c(0, 10)
  db_lcc <- tibble::tibble(
    iso3                = db_lcc[["...2"]],
    adult_literacy      = pick_column(db_lcc, "Adult literacy rate",
                                      lcc_range),                       # 47
    electricity         = pick_column(db_lcc, "Access to electricity",
                                      lcc_range),                       # 44
    internet            = pick_column(db_lcc, "Internet users",
                                      lcc_range),                       # 45
    mobile              = pick_column(db_lcc, "Mobile cellular subscriptions",
                                      lcc_range),                       # 46
    road_density        = pick_column(db_lcc, "Road density",
                                      lcc_range),                       # 48
    improved_water      = pick_column(db_lcc, "Drinking water",
                                      lcc_range),                       # 49
    improved_sanitation = pick_column(db_lcc, "Sanitation",
                                      lcc_range),                       # 50
    health_per_capita   = pick_column(
      db_lcc,
      "per capita public and private expenditure on health care",
      lcc_range)                                                        # 52
  )

  dplyr::left_join(db_risk, db_lcc, by = setNames("iso3", "iso3"))
}

#' Preprocess WHO Health Indicator Data
#'
#' Filters and joins multiple WHO datasets to extract the most recent values for
#'  selected indicators.
#' The function:
#' \itemize{
#'   \item Filters each dataset for country-level data and the most recent year.
#'   \item Applies additional filters for urbanization, sex, and AMR awareness
#'    where applicable.
#'   \item Renames each indicator column using the indicator name.
#'   \item Joins all datasets by country name.
#'   \item Optionally joins ISO A3 codes using a location key table.
#'   \item Returns a tidy data frame with one row per country and one column per
#'    indicator.
#' }
#'
#' @param who_location_keys Optional data frame with `GEO_NAME_SHORT` and
#' `ISO_A3` for country code mapping.
#' @param relay_may2023_wide WHO dataset on household health expenditure.
#' @param x9a706fd_all_latest WHO dataset on UHC coverage index.
#' @param x19e688d_all_latest WHO dataset on antibiotic consumption.
#' @param x217795a_all_latest WHO dataset on doctor density.
#' @param b9c6c79_all_latest WHO dataset on government health expenditure.
#' @param bbf3a64_all_latest WHO dataset on ODA to health.
#' @param d2a45a5_all_latest WHO dataset on access to medicines.
#' @param ed50112_all_latest WHO dataset on unsafe WASH deaths.

#'
#' @return A data frame with selected WHO health indicators by country.
#' @export
preprocess_who_data <- function(
    relay_may2023_wide = NULL,    # household expenditure on health
    x9a706fd_all_latest = NULL,   # UHC coverage index
    x19e688d_all_latest = NULL,   # antibiotic consumption
    x217795a_all_latest = NULL,   # density of doctors
    b9c6c79_all_latest = NULL,    # government expenditure on health
    bbf3a64_all_latest = NULL,    # ODA to health
    d2a45a5_all_latest = NULL,    # access to medicines
    ed50112_all_latest = NULL,    # unsafe WASH deaths
    who_location_keys = NULL
) {
  filter_latest <- function(df, value_col, filters = list()) {
    df <- df |> dplyr::filter(DIM_GEO_CODE_TYPE == "COUNTRY")
    for (f in names(filters)) {
      if (f %in% names(df)) {
        df <- df |> dplyr::filter(.data[[f]] == filters[[f]])
      }
    }
    df <- df |>
      dplyr::group_by(GEO_NAME_SHORT) |>
      dplyr::filter(DIM_TIME == max(DIM_TIME, na.rm = TRUE)) |>
      dplyr::ungroup()
    ind_name <- unique(df$IND_NAME)[1]
    df |>
      dplyr::select(GEO_NAME_SHORT, !!value_col) |>
      dplyr::rename(!!ind_name := !!value_col)
  }

  df1 <- filter_latest(relay_may2023_wide, "PERCENT_POP_N",
                       list(DIM_DEG_URB = "TOTAL"))
  df2 <- filter_latest(x9a706fd_all_latest, "INDEX_N")
  df3 <- filter_latest(x19e688d_all_latest, "RATE_PER_100_N",
                       list(DIM_AMR_GLASS_AWARE = "RESERVE"))
  df4 <- filter_latest(x217795a_all_latest, "RATE_PER_10000_N")
  df5 <- filter_latest(b9c6c79_all_latest, "RATE_PER_100_N")
  df6 <- filter_latest(bbf3a64_all_latest, "MONEY_N")
  df7 <- filter_latest(d2a45a5_all_latest, "RATE_PER_100_N")
  df8 <- filter_latest(ed50112_all_latest, "RATE_PER_100000_N",
                       list(DIM_SEX = "TOTAL"))

  final_data <- df1 |>
    dplyr::full_join(df2, by = "GEO_NAME_SHORT") |>
    dplyr::full_join(df3, by = "GEO_NAME_SHORT") |>
    dplyr::full_join(df4, by = "GEO_NAME_SHORT") |>
    dplyr::full_join(df5, by = "GEO_NAME_SHORT") |>
    dplyr::full_join(df6, by = "GEO_NAME_SHORT") |>
    dplyr::full_join(df7, by = "GEO_NAME_SHORT") |>
    dplyr::full_join(df8, by = "GEO_NAME_SHORT")


  final_data$GEO_NAME_SHORT <- stringi::stri_trans_general(
    final_data$GEO_NAME_SHORT,
    "Latin-ASCII"
  )

  if (!is.null(who_location_keys)) {
    final_data <- final_data |>
      dplyr::left_join(who_location_keys |>
                         dplyr::select(GEO_NAME_SHORT, ISO_A3),
                       by = "GEO_NAME_SHORT")
  }

  final_data |>
    dplyr::transmute(
      country = `GEO_NAME_SHORT`,
      iso_a3 = `ISO_A3`,
      hh_exp_health = `Household health expenditure greater than 25% of household budget`,
      uhc_coverage = `UHC Service coverage index`,
      antibiotic_consumption = `Antibiotic consumption pattern`,
      doctor_density = `Density of doctors`,
      gov_exp_health = `General government expenditure on domestic health`,
      oda_to_health = `Development assistance to medical research and basic health`,
      access_to_medicines = `Access to essential medicines at health facilites`,
      unsafe_water_deaths = `Unsafe water, sanitation and hygiene services deaths`
    )
}

#' Preprocess INFORM Severity Index Data
#'
#' Cleans and transforms INFORM Severity Index data for humanitarian crises.
#' The function:
#' \itemize{
#'   \item Removes the first row and columns with only NA values.
#'   \item One-hot encodes the `DRIVERS` column by splitting comma-separated
#'   values.
#'   \item Renames key columns for clarity.
#'   \item Computes a new variable `conditions_impact` as the geometric mean of
#'   `people_conditions` and `crisis_impact`.
#' }
#'
#' @param df A data frame containing INFORM Severity Index data.
#'
#' @return A processed data frame with renamed columns, one-hot encoded drivers,
#'  and derived indicators.
#' @export
preprocess_severity <- function(df) {
  out <- df |>
    dplyr::slice(-1) |>
    dplyr::select(where(~ !all(is.na(.)))) |>
    # One-hot encode the DRIVERS column
    tidyr::separate_longer_delim(DRIVERS, delim = ",") |>
    dplyr::mutate(dummy = 1) |>
    tidyr::pivot_wider(names_from = DRIVERS,
                       names_prefix = "Driver ",
                       values_from = dummy,
                       values_fn = mean,
                       values_fill = 0) |>
    # Rename selected columns
    dplyr::rename(
      severity_index = `INFORM Severity Index`,
      operating_environment = `Operating environment`,
      crisis_complexity = `Complexity of the crisis`,
      people_conditions = `Conditions of people affected`,
      crisis_impact = `Impact of the crisis`
    ) |>
    # Compute geometric mean of people_conditions and crisis_impact
    dplyr::mutate(conditions_impact = sqrt(people_conditions * crisis_impact))

  collapse_severity_by_country(out)
}

#' Collapse multiple crises to one row per country
#'
#' INFORM Severity records one row per *crisis*, and a country may host several.
#' Left-joining that directly onto the country table duplicates rows silently.
#' This function reduces to one row per ISO3 code: severity dimensions take the
#' country's **maximum** (the binding constraint is the worst concurrent
#' crisis, not their average), and the one-hot driver flags take the union, so
#' that a country with two crises is credited with the drivers of both.
#'
#' @param df A data frame from [preprocess_severity()], one row per crisis.
#'
#' @return A data frame with one row per `ISO3`, plus an `n_crises` column.
#' @export
collapse_severity_by_country <- function(df) {
  if (!"ISO3" %in% names(df)) {
    return(df)
  }

  driver_cols <- grep("^Driver ", names(df), value = TRUE)
  numeric_cols <- setdiff(
    names(df)[vapply(df, is.numeric, logical(1))],
    driver_cols
  )

  df |>
    dplyr::group_by(ISO3) |>
    dplyr::summarise(
      COUNTRY = dplyr::first(COUNTRY),
      n_crises = dplyr::n(),
      dplyr::across(dplyr::all_of(numeric_cols),
                    ~ if (all(is.na(.x))) NA_real_ else max(.x, na.rm = TRUE)),
      dplyr::across(dplyr::all_of(driver_cols),
                    ~ as.numeric(any(.x > 0, na.rm = TRUE))),
      .groups = "drop"
    )
}

#' Preprocess the survey dataset
#'
#' This function:
#' - standardizes column names to snake_case
#' - removes metadata columns from ID to "Which is your role"
#' - converts all remaining columns to factors with a predefined level order
#'
#' @param data A tibble or data frame containing the raw survey data.
#'
#' @return A cleaned tibble with standardized variable names and factor columns.
#' @export
#'
#' @examples
#' # Example survey data
#' survey <- tibble::tibble(
#'   disease = c("Acute Abdominal Pain", "Acute Abdominal Pain"),
#'   question = c("4h diagnosis delay", "4h transport delay"),
#'   response = c("Very concerned", "Somewhat concerned"),
#'   n = c(25, 30),
#'   prop = c(0.35, 0.45)
#' )
#'
#' cleaned <- preprocess_survey(survey)
preprocess_survey <- function(data) {
  levels_order <- c(
    "Very unconcerned",
    "Somewhat unconcerned",
    "Neither concerned nor unconcerned",
    "Somewhat concerned",
    "Very concerned",
    "Not relevant for this condition"
  )

  data |>
    janitor::clean_names() |>
    dplyr::select(-dplyr::any_of(c(
      "id", "ora_di_inizio", "ora_di_completamento", "posta_elettronica", "nome",
      "ora_ultima_modifica", "your_name_and_surname", "which_is_your_role"
    ))) |>
    dplyr::mutate(dplyr::across(dplyr::everything(), ~ factor(.x, levels = levels_order)))
}

#' Preprocess raw proximity assessment survey data
#'
#' @description
#' This function:
#' - Keeps only the most recent record per country
#' - Appends ISO_A3 codes from `risk_country`
#' - Standardizes diagnosis and transport delay values
#' - Dynamically renames outbreak concern columns based on response content
#' - Removes metadata and free-text columns not needed for scoring
#' - Renames the timestamp column to "time"
#'
#' @param form A tibble containing raw proximity assessment data
#' @param risk_country A tibble with columns `country` and `ISO_A3`
#'
#' @return A cleaned tibble ready for scoring
#' @export
preprocess_proximity_assessment <- function(form, risk_country) {
  disease_columns <- c(
    "Acute Abdominal Pain",
    "Non-traumatic Chest Pain",
    "Focal neurological symptoms suggestive of Stroke",
    "Sudden Febrile illness in malaria-prone area",
    "Resting dyspnea"
  )

  delay_columns <- c(
    "Acute Abdominal Pain2",
    "Non-traumatic Chest Pain2",
    "Focal neurological symptoms suggestive of Stroke2",
    "Sudden Febrile illness in malaria-prone area2",
    "Resting dyspnea2"
  )

  outbreak_columns <- c(
    "Food & water borne diseases (Cholera, Polio, Typhoid fever)",
    "STDs (HIV, Syphilis, Gonorrhea, etc.)",
    "Hemorrhagic fever/direct contact (Ebola, Monkeypox, etc.)",
    "Zoonoses (Crimean Congo Fever, Leptospirosis, etc.)",
    "Vector borne (Malaria, Dengue, Chikungunya, etc.)",
    "Air borne (SARS/COVID/MERS, Flu, Measles, TBC, etc.)"
  )

  outbreak_risk_labels <- c(
    "Food/Waterborne Outbreak Risk",
    "STDs Transmission Risk",
    "Hemorrhagic Fever Risk",
    "Zoonoses Concern",
    "Vector-borne Disease Risk",
    "Airborne Disease Risk"
  )

  outbreak_confirmed_labels <- c(
    "Food/Waterborne Disease Concern",
    "STDs Outbreak Concern",
    "Hemorrhagic Fever Outbreak Concern",
    "Zoonoses Concern",
    "Vector-borne Disease Outbreak",
    "Airborne Disease Outbreak"
  )

  diagnosis_map <- c(
    "< 15m" = "15m diagnosis delay",
    "< 1h" = "1h diagnosis delay",
    "< 4h" = "4h diagnosis delay",
    "< 8h" = "8h diagnosis delay"
  )

  transport_map <- c(
    "< 1h" = "1h transport delay",
    "< 4h" = "4h transport delay",
    "< 8h" = "8h transport delay",
    "< 1d" = "1 day transport delay"
  )

  columns_to_drop <- c(
    "ID",
    "Ora di inizio",
    "Posta elettronica",
    "Nome",
    "Ora ultima modifica",
    "Your Name",
    "If you think an important factor should be taken into account before finalising the risk, please describe it"
  )

  # Ensure timestamp is in POSIXct format
  form <- form |>
    dplyr::mutate(`Ora di completamento` = as.POSIXct(`Ora di completamento`, format = "%Y-%m-%d %H:%M:%S", tz = "UTC"))

  # Drop unused columns
  form <- form |>
    dplyr::select(-any_of(columns_to_drop))

  # Append ISO_A3 code only
  form_latest <- form |>
    dplyr::left_join(risk_country |>
                       dplyr::select(country, ISO_A3),
                     by = c("Your Location" = "country"))

  if (any(is.na(form_latest$ISO_A3))) {
    missing <- form_latest |>
      dplyr::filter(is.na(ISO_A3)) |>
      dplyr::pull(`Your Location`) |>
      unique()
    stop(paste("Unmatched countries:", paste(missing, collapse = ", ")))
  }

  # Replace diagnosis delay values
  for (col in disease_columns) {
    if (col %in% names(form_latest)) {
      form_latest[[col]] <- dplyr::recode(form_latest[[col]], !!!diagnosis_map)
    }
  }

  # Replace transport delay values
  for (col in delay_columns) {
    if (col %in% names(form_latest)) {
      form_latest[[col]] <- dplyr::recode(form_latest[[col]], !!!transport_map)
    }
  }

  # Dynamically rename outbreak columns based on content
  for (i in seq_along(outbreak_columns)) {
    col <- outbreak_columns[i]
    if (col %in% names(form_latest)) {
      values <- form_latest[[col]]
      if (any(grepl("concerned", values, ignore.case = TRUE), na.rm = TRUE)) {
        new_name <- outbreak_risk_labels[i]
      } else if (any(grepl("confirmed", values, ignore.case = TRUE), na.rm = TRUE)) {
        new_name <- outbreak_confirmed_labels[i]
      } else {
        new_name <- outbreak_confirmed_labels[i]  # fallback
      }
      form_latest[[new_name]] <- values
      form_latest[[col]] <- NULL
    }
  }

  return(form_latest)
}
