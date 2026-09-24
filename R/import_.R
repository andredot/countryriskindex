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


#' Pick a column by name, disambiguating duplicates by expected range
#'
#' `readxl` disambiguates duplicated headers positionally, producing names like
#' `Road density...15` and `Road density...16`. Selecting by positional suffix
#' is fragile: if the source workbook gains or loses a column, a different
#' variable is silently picked up and the index changes for reasons nobody can
#' trace.
#'
#' The INFORM Lack of Coping Capacity sheet carries road density twice - once as
#' the raw value (roughly 1 to 850 km per unit area) and once as the normalised
#' INFORM 0-10 score. The two are cleanly separable by range, so this function
#' selects the candidate that *satisfies the contract* rather than the first or
#' last one. That is stable across workbook revisions in a way that a position
#' is not.
#'
#' Resolution order:
#' \enumerate{
#'   \item Collect columns whose name is `prefix` or `prefix...N`.
#'   \item If `range` is supplied, keep only candidates whose observed values
#'     fall inside it.
#'   \item Exactly one survivor: use it. None: error, reporting each
#'     candidate's observed range. Several: prefer an exact name match, and
#'     otherwise error as ambiguous.
#' }
#'
#' @param df A data frame.
#' @param prefix The column name, or the stem of a positionally-suffixed name.
#' @param range Numeric length-2 vector giving the plausible `c(min, max)`.
#'   Strongly recommended: it is what makes the selection stable.
#' @param required Whether a missing column is an error (`TRUE`) or returns
#'   `NA` (`FALSE`).
#'
#' @return A numeric vector.
#' @export
#'
#' @examples
#' d <- data.frame(
#'   `Road density...15` = c(1.5, 844),   # raw
#'   `Road density...16` = c(0.2, 9.8),   # INFORM 0-10 score
#'   check.names = FALSE
#' )
#' pick_column(d, "Road density", range = c(0, 10))
pick_column <- function(df, prefix, range = NULL, required = TRUE) {

  exact <- which(names(df) == prefix)
  suffixed <- grep(paste0("^", prefix, "\\.\\.\\.[0-9]+$"), names(df))
  candidates <- union(exact, suffixed)

  if (length(candidates) == 0) {
    if (required) {
      stop("pick_column(): no column matching '", prefix, "'. Available: ",
           paste(utils::head(names(df), 40), collapse = ", "),
           call. = FALSE)
    }
    return(rep(NA_real_, nrow(df)))
  }

  as_num <- function(i) suppressWarnings(as.numeric(df[[i]]))
  obs_range <- function(i) {
    x <- as_num(i)
    if (all(is.na(x))) c(NA_real_, NA_real_) else range(x, na.rm = TRUE)
  }

  if (is.null(range)) {
    if (length(candidates) > 1) {
      warning(
        "pick_column(): '", prefix, "' matches ", length(candidates),
        " columns (", paste(names(df)[candidates], collapse = ", "),
        ") and no `range` was given to disambiguate; taking '",
        names(df)[candidates[1]], "'.",
        call. = FALSE
      )
    }
    return(as_num(candidates[1]))
  }

  fits <- vapply(candidates, function(i) {
    r <- obs_range(i)
    all(is.finite(r)) && r[1] >= range[1] && r[2] <= range[2]
  }, logical(1))

  describe <- function(idx) {
    paste(vapply(idx, function(i) {
      r <- obs_range(i)
      paste0("'", names(df)[i], "' [", signif(r[1], 4), ", ",
             signif(r[2], 4), "]")
    }, character(1)), collapse = "; ")
  }

  if (!any(fits)) {
    stop(
      "pick_column(): no column matching '", prefix, "' has values within [",
      range[1], ", ", range[2], "]. Candidates: ", describe(candidates),
      ". The source workbook layout has probably changed, or a raw indicator ",
      "is present where a normalised score was expected.",
      call. = FALSE
    )
  }

  chosen <- candidates[fits]

  if (length(chosen) > 1) {
    exact_fit <- intersect(chosen, exact)
    if (length(exact_fit) == 1) {
      chosen <- exact_fit
    } else {
      stop(
        "pick_column(): '", prefix, "' is ambiguous - ", length(chosen),
        " columns fall within [", range[1], ", ", range[2], "]: ",
        describe(chosen), ". Tighten `range` or rename the source columns.",
        call. = FALSE
      )
    }
  }

  if (length(candidates) > 1) {
    message(
      "pick_column(): '", prefix, "' matched ", length(candidates),
      " columns; selected '", names(df)[chosen],
      "' as the only one within [", range[1], ", ", range[2], "]."
    )
  }

  as_num(chosen)
}
