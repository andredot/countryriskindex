import_data <- function(.data_path) {
  file.path(.data_path) |>
    normalizePath() |>
    readr::read_csv()
}

import_inform_excel <- function(.data_path, sheet = 2) {
  file.path(.data_path) |>
    normalizePath() |>
    readxl::read_excel(sheet = sheet, na = "x") |>
    dplyr::slice(-1)
}

import_cm_matrix <- function(.data_path) {
  file.path(.data_path) |>
    normalizePath() |>
    readxl::read_excel()
}

import_severity_excel <- function(.data_path) {
  col_names <- file.path(.data_path) |>
    normalizePath() |>
    readxl::read_excel(sheet = "INFORM Severity - country",
                       na = "x",
                       skip = 1,
                       n_max = 0) |>
    names()

  col_types <- ifelse(col_names %in% c("COUNTRY", "CRISIS", "ISO3", "DRIVERS"),
                        "text",
                        "numeric")

  file.path(.data_path) |>
    normalizePath() |>
    readxl::read_excel(sheet = "INFORM Severity - country",
                       na = "x",
                       skip = 1,
                       col_types = col_types) |>
    dplyr::slice(-1)
}

#' Import Excel from previous risk assessment
#'
#' @param path Path to the Excel file.
#' @return A tibble with the imported data.
#' @export
import_crpi_excel <- function(path) {
  readxl::read_excel(
    path = path,
    sheet = "List",
    range = "A1:E192",
    col_names = TRUE
  ) |>
    dplyr::as_tibble() |>
    dplyr::rename( "iso_a3" = "...4")
}


import_excel <- function(.data_path) {
  df <- file.path(.data_path) |>
    normalizePath() |>
    readxl::read_excel()

  df$GEO_NAME_SHORT <- stringi::stri_trans_general(df$GEO_NAME_SHORT,
                                                   "Latin-ASCII")
  return(df)
}
