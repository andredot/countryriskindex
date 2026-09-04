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

import_excel <- function(.data_path) {
  df <- file.path(.data_path) |>
    normalizePath() |>
    readxl::read_excel()

  df$GEO_NAME_SHORT <- stringi::stri_trans_general(df$GEO_NAME_SHORT,
                                                   "Latin-ASCII")
  return(df)
}


#' Pick a column by name with a range assertion
#'
#' `readxl` disambiguates duplicated headers positionally, producing names like
#' `Road density...16`. Selecting those by their positional suffix is fragile:
#' if the source workbook gains or loses a column, a different variable is
#' silently picked up and the index changes for reasons nobody can trace.
#'
#' This helper selects by *prefix* instead, requires the match to be
#' unambiguous, and asserts that the values fall in the expected range - so a
#' layout change fails loudly at import rather than quietly at interpretation.
#'
#' @param df A data frame.
#' @param prefix The column name, or the stem of a positionally-suffixed name.
#' @param range Numeric length-2 vector giving the plausible `c(min, max)`.
#' @param which If several columns share the prefix, which to take: `"first"`,
#'   `"last"`, or an integer index. Defaults to `"first"`, and warns.
#' @param required Whether a missing column is an error (`TRUE`) or returns
#'   `NA` (`FALSE`).
#'
#' @return A numeric vector.
#' @export
#'
#' @examples
#' d <- data.frame(`Road density...16` = c(1, 5), check.names = FALSE)
#' pick_column(d, "Road density", range = c(0, 10))
pick_column <- function(df, prefix, range = NULL, which = "first",
                        required = TRUE) {

  exact <- which(names(df) == prefix)
  suffixed <- grep(paste0("^", prefix, "\\.\\.\\.[0-9]+$"), names(df))
  idx <- if (length(exact) > 0) exact else suffixed

  if (length(idx) == 0) {
    if (required) {
      stop("pick_column(): no column matching '", prefix, "'. Available: ",
           paste(utils::head(names(df), 40), collapse = ", "),
           call. = FALSE)
    }
    return(rep(NA_real_, nrow(df)))
  }

  if (length(idx) > 1) {
    chosen <- switch(
      as.character(which),
      "first" = idx[1],
      "last"  = idx[length(idx)],
      idx[as.integer(which)]
    )
    warning(
      "pick_column(): '", prefix, "' matches ", length(idx), " columns (",
      paste(names(df)[idx], collapse = ", "), "); taking '",
      names(df)[chosen], "'. Check the source workbook layout.",
      call. = FALSE
    )
    idx <- chosen
  }

  x <- suppressWarnings(as.numeric(df[[idx]]))

  if (!is.null(range)) {
    observed <- range(x, na.rm = TRUE)
    if (all(is.finite(observed)) &&
          (observed[1] < range[1] || observed[2] > range[2])) {
      stop(
        "pick_column(): '", names(df)[idx], "' has range [",
        signif(observed[1], 4), ", ", signif(observed[2], 4),
        "] but [", range[1], ", ", range[2], "] was expected. ",
        "The source workbook layout has probably changed, or a raw indicator ",
        "is being read where a normalised 0-10 score was intended.",
        call. = FALSE
      )
    }
  }

  x
}
