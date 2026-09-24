#' Normalize values between the 1st and 99th percentiles
#'
#' Scales a numeric vector so that the 1st percentile becomes 0 and the 99th becomes 1.
#'
#' @param x Numeric vector.
#' @return A normalized numeric vector.
#' @export
normalise_quantiles <- function(x) {
  q1 <- quantile(x, 0.01, na.rm = TRUE)
  q99 <- quantile(x, 0.99, na.rm = TRUE)

  scaled <- (x - q1) / (q99 - q1)
  scaled[scaled < 0] <- 0
  scaled[scaled > 1] <- 1

  return(scaled)
}

#' Compute the geometric mean
#'
#' Calculates the geometric mean of a numeric vector, ignoring NAs.
#'
#' @param x Numeric vector.
#' @return Geometric mean as a single numeric value.
#' @export
geometric_mean <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) == 0) return(NA_real_)
  exp(mean(log(x)))
}

#' Invert a 0–10 scale
#'
#' Converts values on a 0–10 scale to their inverse (10 - x).
#'
#' @param x Numeric vector.
#' @return Inverted numeric vector.
#' @export
invert_0_10 <- function(x) {
  ifelse(is.na(x), NA_real_, 10 - x)
}

#' Invert a 0–100 scale
#'
#' Converts values on a 0–100 scale to their inverse (100 - x).
#'
#' @param x Numeric vector.
#' @return Inverted numeric vector.
#' @export
invert_0_100 <- function(x) {
  ifelse(is.na(x), NA_real_, 100 - x)
}

#' Export data to a GeoPackage
#'
#' Joins input data to country geometries and writes to a GeoPackage file.
#'
#' @param data Data frame with country-level data.
#' @param data_key Column name for country codes in `data`.
#' @param cols Columns to include in the export.
#' @param .data_path Output file path.
#' @return Writes a GeoPackage file to disk.
#' @export
export_gpkg <- function(data = NULL,
                        data_key = "ISO_A3",
                        cols = c("overall_risk","severity_adjusted_risk",
                                 data_key),
                        .data_path = "") {
  rnaturalearth::ne_countries(scale = "medium", returnclass = "sf") |>
    dplyr::left_join(data[, cols], by = setNames(data_key, "adm0_a3")) |>
    sf::st_write(.data_path, append = FALSE)
}

#' GBD round used by the pipeline
#'
#' The GBD year appears in two input file names. Keeping it in one place means
#' a new round is a one-line change rather than a search-and-replace, and makes
#' the vintage of the hazard component explicit rather than buried in a path.
#'
#' Per `sources.md`, the extract must be requested from
#' <https://vizhub.healthdata.org/gbd-results/> with: Estimate = Cause of death
#' or injury; Measure = DALYs; Metric = Rate; Cause = "All causes" plus all
#' level-2 causes; Location = all countries and territories; Age = all ages;
#' Sex = both; Year = this value.
#'
#' @return A character scalar.
#' @export
gbd_year <- function() "2021"
