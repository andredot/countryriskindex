## Rebase the frozen normalisation reference
##
## The CHRI normalises its components against the 1st and 99th percentiles of a
## FROZEN reference distribution rather than the current run. This is what keeps
## scores comparable between releases: without it, a country's score changes
## whenever *other* countries change, and year-on-year movement cannot be
## attributed to anything real.
##
## Consequently this script must NOT be run as part of the normal pipeline.
## Run it only when deliberately rebasing the index, and when you do:
##   1. record the reason and the date in NEWS.md;
##   2. bump the index version in reports/report.qmd;
##   3. state clearly that scores before and after the rebase are not comparable.
##
## Usage (from the project root, with the pipeline already run):
##   source("dev/regenerate_reference_quantiles.R")

library(targets)
devtools::load_all()

# Build the scores WITHOUT a reference, so the quantiles reflect the raw data
merged <- tar_read(merged_indicators)

unreferenced <- merged |>
  add_data_completeness() |>
  add_hazard_score(reference = NULL) |>
  add_vulnerability_score(reference = NULL) |>
  add_capacity_score(reference = NULL)

reference_vars <- c(
  "hazard_raw",        # All-causes DALY rate
  "vulnerability_raw", # inverted geometric mean of the vulnerability components
  "capacity_raw",      # inverted geometric mean of the capacity components
  "resources_raw"      # log1p(doctor density)
)

reference <- compute_reference_quantiles(unreferenced, reference_vars)
print(reference)

out_path <- file.path("inst", "extdata", "reference_quantiles_2025.csv")
readr::write_csv(reference, out_path)

message("Reference quantiles written to ", out_path)
message("Reinstall the package so the new file is picked up by system.file().")
