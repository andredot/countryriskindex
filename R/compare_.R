## RUN-TO-RUN COMPARISON --------------------------------------------------
##
## v4.0 compared each release against the legacy CRPI spreadsheets
## (`Country Risk Profile Index X3.12.x.xlsx`). Those imports have been removed:
## the legacy index is no longer a meaningful benchmark, and pinning every
## release to it made the comparison less informative with each revision.
##
## Instead, each run writes a snapshot of its own scores. The next run compares
## against the most recent previous snapshot, so "what changed and why" is
## answered relative to the last published version, whatever that was.

#' Snapshot file name pattern
#'
#' @keywords internal
#' @noRd
snapshot_pattern <- function() "^chri_snapshot_[0-9]{8}_[0-9]{6}\\.csv$"

#' Columns retained in a run snapshot
#'
#' @return A character vector of column names.
#' @export
snapshot_columns <- function() {
  c("ISO_A3", "country",
    "hazard_score", "vulnerability_score", "capacity_score",
    "overall_risk", "severity_index", "severity_adjusted_risk",
    "severity_uplift", "data_completeness", "low_confidence")
}

#' Write a snapshot of the current run
#'
#' Saves the headline scores to a timestamped CSV in `dir`, to be used as the
#' comparison baseline by future runs.
#'
#' @param df A data frame of scored countries.
#' @param dir Directory to write to. Defaults to the project output data path.
#' @param cols Columns to retain. Defaults to [snapshot_columns()].
#' @param timestamp Optional `POSIXct` used to build the file name.
#'
#' @return The path of the file written (invisibly returned as a character
#'   scalar, suitable for a `targets` `format = "file"` target).
#' @export
save_run_snapshot <- function(df,
                              dir = NULL,
                              cols = snapshot_columns(),
                              timestamp = Sys.time()) {
  if (is.null(dir)) {
    dir <- get_output_data_path("snapshots")
  }
  if (!dir.exists(dir)) {
    dir.create(dir, recursive = TRUE)
  }

  cols <- intersect(cols, names(df))
  stamp <- format(timestamp, "%Y%m%d_%H%M%S")
  path <- file.path(dir, paste0("chri_snapshot_", stamp, ".csv"))

  readr::write_csv(df[, cols, drop = FALSE], path)
  path
}

#' Load the most recent previous snapshot
#'
#' Returns the newest snapshot in `dir` excluding `exclude` (normally the file
#' just written by the current run). Returns `NULL` - not an error - when no
#' earlier snapshot exists, so the first run of a new pipeline works without
#' special casing.
#'
#' @param dir Directory holding snapshots.
#' @param exclude Optional path to exclude from the search.
#'
#' @return A data frame, or `NULL` when no previous snapshot is available.
#' @export
load_previous_snapshot <- function(dir = NULL, exclude = NULL) {
  if (is.null(dir)) {
    dir <- get_output_data_path("snapshots")
  }
  if (!dir.exists(dir)) {
    return(NULL)
  }

  files <- list.files(dir, pattern = snapshot_pattern(), full.names = TRUE)
  if (!is.null(exclude)) {
    files <- setdiff(normalizePath(files, mustWork = FALSE),
                     normalizePath(exclude, mustWork = FALSE))
  }
  if (length(files) == 0) {
    return(NULL)
  }

  latest <- files[order(basename(files), decreasing = TRUE)][1]
  out <- readr::read_csv(latest, show_col_types = FALSE)
  attr(out, "snapshot_path") <- latest
  out
}

#' Compare the current run against a previous snapshot
#'
#' @param current A data frame of current scores.
#' @param previous A data frame from [load_previous_snapshot()], or `NULL`.
#' @param value_col The score to compare.
#'
#' @return A data frame with `ISO_A3`, `country`, `previous`, `current`,
#'   `difference` and `status` (`"changed"`, `"new"`, `"dropped"`), or `NULL`
#'   when `previous` is `NULL`.
#' @export
compare_runs <- function(current, previous,
                         value_col = "severity_adjusted_risk") {
  if (is.null(previous)) {
    return(NULL)
  }
  if (!value_col %in% names(current) || !value_col %in% names(previous)) {
    warning("compare_runs(): '", value_col, "' missing from one of the runs.",
            call. = FALSE)
    return(NULL)
  }

  cur <- data.frame(
    ISO_A3 = current[["ISO_A3"]],
    country = current[["country"]],
    current = current[[value_col]],
    stringsAsFactors = FALSE
  )
  prev <- data.frame(
    ISO_A3 = previous[["ISO_A3"]],
    previous = previous[[value_col]],
    stringsAsFactors = FALSE
  )

  out <- merge(cur, prev, by = "ISO_A3", all = TRUE)
  out$difference <- out$current - out$previous
  out$status <- ifelse(
    is.na(out$previous), "new",
    ifelse(is.na(out$current), "dropped", "changed")
  )
  out$value_col <- value_col

  out[order(-abs(out$difference), na.last = TRUE), , drop = FALSE]
}

#' Plot the difference between the current run and the previous snapshot
#'
#' Renders a lollipop chart of the countries that moved most. When there is no
#' previous snapshot (first run), returns an informative placeholder rather than
#' failing, so the report still builds.
#'
#' @param comparison Output of [compare_runs()], possibly `NULL`.
#' @param top_n Number of largest movers to display.
#'
#' @return A ggplot2 object.
#' @export
create_run_comparison_plot <- function(comparison, top_n = 30) {
  if (is.null(comparison) || nrow(comparison) == 0) {
    return(
      ggplot2::ggplot() +
        ggplot2::annotate(
          "text", x = 0, y = 0,
          label = paste(
            "No previous snapshot available.",
            "This run establishes the baseline;",
            "the next run will compare against it.",
            sep = "\n"
          ),
          size = 5, colour = "grey30"
        ) +
        ggplot2::theme_void()
    )
  }

  d <- comparison[comparison$status == "changed" &
                    !is.na(comparison$difference), , drop = FALSE]
  d <- utils::head(d, top_n)
  if (nrow(d) == 0) {
    return(
      ggplot2::ggplot() +
        ggplot2::annotate("text", x = 0, y = 0,
                          label = "No country changed between runs.",
                          size = 5, colour = "grey30") +
        ggplot2::theme_void()
    )
  }

  d$country <- stats::reorder(d$country, d$difference)

  ggplot2::ggplot(d, ggplot2::aes(y = .data$country)) +
    ggplot2::geom_segment(
      ggplot2::aes(x = .data$previous, xend = .data$current,
                   yend = .data$country),
      colour = "grey70", linewidth = 0.8
    ) +
    ggplot2::geom_point(ggplot2::aes(x = .data$previous),
                        colour = "grey40", size = 2.5) +
    ggplot2::geom_point(ggplot2::aes(x = .data$current),
                        colour = "firebrick", size = 2.5) +
    ggplot2::labs(
      title = "Change since the previous run",
      subtitle = paste0(
        "Grey: previous snapshot. Red: current run. ",
        "Metric: ", d$value_col[1]
      ),
      x = "Score", y = NULL
    ) +
    ggplot2::theme_minimal(base_size = 12)
}
