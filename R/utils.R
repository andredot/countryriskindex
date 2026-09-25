
# Here below put your small tiny supporting functions -------------
view_in_excel <- function(.data) {
  if (interactive()) {
    tmp <- fs::file_temp("excel", ext = "csv")
    readr::write_excel_csv(.data, tmp)
    fs::file_show(tmp)
  }
  invisible(.data)
}

extract_fct_names <- function(path) {
  readr::read_lines(path) |>
    stringr::str_extract_all("^.*(?=`? ?<- ?function)") |>
    unlist() |>
    purrr::compact() |>
    stringr::str_remove_all("[\\s`]+")
}

get_input_data_path <- function(x = "") {
  file.path(
    Sys.getenv("PRJ_SHARED_PATH"),
    Sys.getenv("INPUT_DATA_FOLDER"),
    x
  ) |>
    normalizePath()
}

get_output_data_path <- function(x = "") {
  file.path(
    Sys.getenv("PRJ_SHARED_PATH"),
    Sys.getenv("OUTPUT_DATA_FOLDER"),
    x
  ) |>
    normalizePath(mustWork = FALSE)
}

share_objects <- function(obj_list) {
  now <- lubridate::now() |>
    stringr::str_remove_all("\\W+") |>
    stringr::str_sub(1, 12)

  file_name_now <- stringr::str_c(
    names(obj_list), "-", now, ".rds"
  )

  file_name_latest <- stringr::str_c(
    names(obj_list), "-", "latest", ".rds"
  )

  # Those must be RDS
  obj_paths_now <- get_output_data_path(file_name_now) |>
    normalizePath(mustWork = FALSE) |>
    purrr::set_names(names(obj_list))

  obj_paths_latest <- get_output_data_path(file_name_latest) |>
    normalizePath(mustWork = FALSE) |>
    purrr::set_names(names(obj_list))

  obj_list |>
    purrr::walk2(obj_paths_now, readr::write_rds)
  obj_list |>
    purrr::walk2(obj_paths_latest, readr::write_rds)

  obj_paths_latest
}

save_plots_jpeg <- function(plots, path = "plots_jpeg") {
  if (!dir.exists(path)) {
    dir.create(path)
  }

  for (name in names(plots)) {
    filename <- paste0(path, "/", gsub(" ", "_", name), ".jpeg")
    ggplot2::ggsave(
      filename = filename,
      plot = plots[[name]],
      width = 1800 / 216,
      height = 1200 / 216,
      dpi = 216,
      units = "in"
    )
  }

  invisible(NULL)
}

#' Normalise values against a (possibly frozen) reference distribution
#'
#' Scales a numeric vector so that the reference 1st percentile maps to 0 and
#' the reference 99th percentile maps to 1, clipping outside that range.
#'
#' By default the reference percentiles are computed from `x` itself, which
#' makes the resulting score *relative to the current run*: a country's value
#' changes when other countries change, even if its own inputs are identical.
#' For year-on-year comparability, pass frozen bounds via `q01`/`q99` (see
#' [compute_reference_quantiles()] and [get_reference_bounds()]).
#'
#' @param x Numeric vector.
#' @param q01 Optional numeric scalar: frozen lower (1st percentile) bound.
#' @param q99 Optional numeric scalar: frozen upper (99th percentile) bound.
#'
#' @return A numeric vector scaled to `[0, 1]`.
#' @export
#'
#' @examples
#' normalise_quantiles(c(1, 5, 10))
#' normalise_quantiles(c(1, 5, 10), q01 = 0, q99 = 20)
normalise_quantiles <- function(x, q01 = NULL, q99 = NULL) {
  if (is.null(q01)) {
    q01 <- unname(stats::quantile(x, 0.01, na.rm = TRUE))
  }
  if (is.null(q99)) {
    q99 <- unname(stats::quantile(x, 0.99, na.rm = TRUE))
  }

  if (!is.finite(q01) || !is.finite(q99) || q99 <= q01) {
    stop(
      "normalise_quantiles(): invalid reference bounds ",
      "(q01 = ", q01, ", q99 = ", q99, ").",
      call. = FALSE
    )
  }

  scaled <- (x - q01) / (q99 - q01)
  scaled[!is.na(scaled) & scaled < 0] <- 0
  scaled[!is.na(scaled) & scaled > 1] <- 1

  scaled
}

#' Compute a frozen reference quantile table
#'
#' Computes the 1st and 99th percentiles of the named variables in `df`. The
#' resulting table is intended to be written once (for a chosen baseline year)
#' and then reused by every subsequent run, so that scores remain comparable
#' over time. See `inst/extdata/reference_quantiles_2025.csv`.
#'
#' @param df A data frame containing the variables in `vars`.
#' @param vars Character vector of variable names to summarise. Each element may
#'   be a column name of `df`; entries not found are skipped with a warning.
#'
#' @return A data frame with columns `variable`, `q01`, `q99`, `n_obs`.
#' @export
compute_reference_quantiles <- function(df, vars) {
  missing_vars <- setdiff(vars, names(df))
  if (length(missing_vars) > 0) {
    warning(
      "compute_reference_quantiles(): skipping absent variables: ",
      paste(missing_vars, collapse = ", "),
      call. = FALSE
    )
  }
  vars <- intersect(vars, names(df))

  out <- lapply(vars, function(v) {
    x <- df[[v]]
    data.frame(
      variable = v,
      q01 = unname(stats::quantile(x, 0.01, na.rm = TRUE)),
      q99 = unname(stats::quantile(x, 0.99, na.rm = TRUE)),
      n_obs = sum(!is.na(x)),
      stringsAsFactors = FALSE
    )
  })

  do.call(rbind, out)
}

#' Look up frozen reference bounds for one variable
#'
#' Convenience accessor for a reference quantile table. Returns `NULL` when the
#' variable is absent or the table is `NULL`, so that callers fall back to
#' run-relative normalisation.
#'
#' @param reference A data frame as returned by [compute_reference_quantiles()],
#'   or `NULL`.
#' @param var Name of the variable to look up.
#'
#' @return A list with elements `q01` and `q99`, or `NULL`.
#' @export
get_reference_bounds <- function(reference, var) {
  if (is.null(reference) || !is.data.frame(reference)) {
    return(NULL)
  }
  if (!all(c("variable", "q01", "q99") %in% names(reference))) {
    return(NULL)
  }

  row <- reference[reference$variable == var, , drop = FALSE]
  if (nrow(row) == 0) {
    return(NULL)
  }

  list(q01 = as.numeric(row$q01[1]), q99 = as.numeric(row$q99[1]))
}

#' Compute the geometric mean
#'
#' Calculates the geometric mean of a numeric vector, ignoring `NA`s.
#'
#' Two guards are applied that the naive `exp(mean(log(x)))` form lacks:
#' non-positive values are clamped to `eps` (otherwise a single zero collapses
#' the whole aggregate to zero, which happens routinely after quantile
#' normalisation), and the result is `NA` when fewer than `min_n` non-missing
#' values are available (otherwise a country measured on two indicators is
#' silently treated as equivalent to one measured on four).
#'
#' @param x Numeric vector.
#' @param min_n Minimum number of non-missing values required; below this the
#'   function returns `NA_real_`. Defaults to 1 (previous behaviour).
#' @param eps Small positive value used to clamp non-positive inputs.
#'
#' @return Geometric mean as a single numeric value.
#' @export
#'
#' @examples
#' geometric_mean(c(1, 4, 16))
#' geometric_mean(c(1, NA, NA), min_n = 2)
geometric_mean <- function(x, min_n = 1, eps = 1e-4) {
  x <- x[!is.na(x)]
  if (length(x) < min_n || length(x) == 0) {
    return(NA_real_)
  }
  x[x < eps] <- eps
  exp(mean(log(x)))
}

#' Row-wise geometric mean over selected columns
#'
#' Thin wrapper around [geometric_mean()] for data frame columns, avoiding the
#' `apply()` idiom which silently coerces mixed-type frames to character.
#'
#' @param df A data frame.
#' @param cols Character vector of column names.
#' @param min_n Minimum number of non-missing values per row.
#' @param eps Small positive value used to clamp non-positive inputs.
#'
#' @return A numeric vector of length `nrow(df)`.
#' @export
row_geometric_mean <- function(df, cols, min_n = 1, eps = 1e-4) {
  stopifnot(all(cols %in% names(df)))
  mat <- as.matrix(df[, cols, drop = FALSE])
  storage.mode(mat) <- "double"
  apply(mat, 1, geometric_mean, min_n = min_n, eps = eps)
}

#' Shift a probability on the log-odds scale
#'
#' Applies `plogis(qlogis(p) + delta)`. Unlike an additive shift in probability
#' space, this is bounded (never reaches 0 or 1), monotone, and has a constant
#' odds-ratio interpretation: `delta` is a log odds ratio regardless of the
#' baseline. It is the transformation used for every adjustment in this package
#' so that all modifiers are expressed on the same scale.
#'
#' @param p Numeric vector of probabilities in `[0, 1]`.
#' @param delta Numeric vector of log-odds shifts (recycled against `p`).
#' @param eps Clamp applied to `p` before transformation, to keep `qlogis()`
#'   finite.
#'
#' @return A numeric vector of adjusted probabilities.
#' @export
#'
#' @examples
#' logit_shift(0.35, 1.2)
#' logit_shift(c(0.2, 0.5, 0.9), 0.5)
logit_shift <- function(p, delta, eps = 1e-3) {
  delta <- rep_len(delta, length(p))
  delta[is.na(delta)] <- 0

  clamped <- pmin(pmax(p, eps), 1 - eps)
  out <- stats::plogis(stats::qlogis(clamped) + delta)

  # Clamping the input can push a score that was already at the boundary the
  # WRONG way: logit_shift(1, 0.5) would return 0.9995, i.e. a positive shift
  # that lowers the value. Enforce the direction of `delta` so the adjustment is
  # always monotone and delta == 0 is exactly the identity.
  up <- !is.na(p) & delta > 0
  down <- !is.na(p) & delta < 0
  flat <- !is.na(p) & delta == 0
  out[up] <- pmax(out[up], p[up])
  out[down] <- pmin(out[down], p[down])
  out[flat] <- p[flat]

  out
}

#' Invert a 0-10 scale
#'
#' Converts values on a 0-10 scale to their inverse (10 - x).
#'
#' @param x Numeric vector.
#' @return Inverted numeric vector.
#' @export
invert_0_10 <- function(x) {
  ifelse(is.na(x), NA_real_, 10 - x)
}

#' Invert a 0-100 scale
#'
#' Converts values on a 0-100 scale to their inverse (100 - x).
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
                        cols = c("overall_risk", "severity_adjusted_risk",
                                 data_key),
                        .data_path = "") {
  rnaturalearth::ne_countries(scale = "medium", returnclass = "sf") |>
    dplyr::left_join(data[, cols], by = stats::setNames(data_key, "adm0_a3")) |>
    sf::st_write(.data_path, append = FALSE)
}

#' Warn about variables lacking frozen reference bounds
#'
#' Scores for variables absent from the reference table fall back to
#' run-relative normalisation and are therefore *not* comparable across
#' releases. This check makes that visible rather than silent - in particular on
#' the very first run, when the shipped reference file is still empty.
#'
#' @param reference A reference quantile table, or `NULL`.
#' @param vars Character vector of variables expected to be covered.
#'
#' @return `invisible(TRUE)` if all variables are covered, `invisible(FALSE)`
#'   otherwise.
#' @export
check_reference_coverage <- function(reference,
                                     vars = c("hazard_raw",
                                              "vulnerability_raw",
                                              "capacity_raw",
                                              "resources_raw")) {
  covered <- if (is.null(reference) || !is.data.frame(reference) ||
                   !"variable" %in% names(reference)) {
    character(0)
  } else {
    as.character(reference$variable)
  }

  missing_vars <- setdiff(vars, covered)
  if (length(missing_vars) > 0) {
    warning(
      "No frozen reference bounds for: ", paste(missing_vars, collapse = ", "),
      ". These components are normalised against the CURRENT run and are not ",
      "comparable with previous releases. Run ",
      "dev/regenerate_reference_quantiles.R to establish the baseline.",
      call. = FALSE
    )
    return(invisible(FALSE))
  }
  invisible(TRUE)
}


#' Locate and read the frozen reference quantile table
#'
#' Looks for the reference file in the installed package and, failing that, in
#' the source tree (so `devtools::load_all()` works before installation).
#'
#' A missing or empty file is **not** an error: the pipeline degrades to
#' run-relative normalisation with a warning, so a fresh checkout still runs and
#' can generate the baseline. It does mean the resulting scores are not
#' comparable with previous releases, which is what the warning says.
#'
#' Note that this is deliberately not a `targets` `format = "file"` target:
#' after regenerating the reference, run
#' `targets::tar_invalidate(reference_quantiles)` to force a rebuild.
#'
#' @param file Name of the reference file within `inst/extdata`.
#'
#' @return A data frame with columns `variable`, `q01`, `q99`, `n_obs`. Zero
#'   rows when no reference is available.
#' @export
read_reference_quantiles <- function(file = "reference_quantiles_2025.csv") {
  empty <- data.frame(
    variable = character(0), q01 = numeric(0),
    q99 = numeric(0), n_obs = integer(0),
    stringsAsFactors = FALSE
  )

  path <- system.file("extdata", file, package = "countryriskindex")
  if (!nzchar(path) || !file.exists(path)) {
    path <- file.path("inst", "extdata", file)
  }
  if (!file.exists(path)) {
    warning(
      "Reference quantile file '", file, "' not found. Falling back to ",
      "run-relative normalisation: scores will NOT be comparable with ",
      "previous releases. Run dev/regenerate_reference_quantiles.R to ",
      "establish the baseline.",
      call. = FALSE
    )
    return(empty)
  }

  ref <- readr::read_csv(path, show_col_types = FALSE)
  if (nrow(ref) == 0) {
    warning(
      "Reference quantile file '", path, "' is empty (this is expected on the ",
      "first run). Falling back to run-relative normalisation. Run ",
      "dev/regenerate_reference_quantiles.R, then reinstall and re-run.",
      call. = FALSE
    )
    return(empty)
  }

  check_reference_coverage(ref)
  ref
}
