# Data sources

Every source listed here is consumed by the analytical pipeline. Anything not
listed is not fetched: version 4.10 removed six WHO datasets and the UN IGME
under-5 mortality extract, all of which were downloaded and parsed but never
read by any downstream function.

## WHO

Only two indicators reach the index, both in the **capacity** component.

| Indicator | Code | Used by | Link |
|---|---|---|---|
| UHC service coverage index | `9A706FD` | capacity → financing | <https://data.who.int/indicators/i/3805B1E/9A706FD> |
| Density of doctors | `217795A` | capacity → resources | <https://data.who.int/indicators/i/CCCEBB2/217795A> |

Save as `WHO/9A706FD_ALL_LATEST.csv` and `WHO/217795A_ALL_LATEST.csv`, plus the
country key table `WHO/WHO_loc_keys.xlsx`.

WHO publishes "most recent year available" per country, which differs
systematically between well- and poorly-measured countries. `preprocess_who_data()`
therefore retains `year_uhc` and `year_doctors` so the heterogeneity is visible.

## GBD

Request from <https://vizhub.healthdata.org/gbd-results/>:

| Parameter | Value |
|---|---|
| GBD Estimate | Cause of death or injury |
| Measure | DALYs |
| Metric | Rate |
| Cause | "All causes" **plus all level-2 causes** |
| Location | All countries and territories |
| Age | All ages |
| Sex | Both |
| Year | see `gbd_year()` |

Save as `GBD/IHME-GBD_<year>_DALY.csv` with the key table
`GBD/IHME_GBD_<year>_location_keys.csv`. Set the round in `gbd_year()`
(`R/small_utils.R`); it is referenced from both paths.

`preprocess_gbd_rates_by_cause()` pivots on `cause_name` and keeps whatever
causes the file contains — no cause IDs are hard-coded. Only `All causes` feeds
the hazard score; the level-2 causes drive the radar decomposition and the
correlation matrix. An extract lacking `All causes` is a hard error.

## HAQ Index

No regular update cycle; refresh only when IHME publishes a new paper. The
current values derive from GBD 2019, which is the oldest input in the index and
is recorded as a limitation in the report.

## EU INFORM

| Dataset | Link |
|---|---|
| INFORM Risk | <https://drmkc.jrc.ec.europa.eu/inform-index/INFORM-Risk/Results-and-data> |
| INFORM Severity | <https://drmkc.jrc.ec.europa.eu/inform-index/INFORM-Severity/Results-and-data> |

Save as `EU/INFORM_Risk_<version>.xlsx` and `EU/INFORM_Severity_<month>.xlsx`,
and update the paths in `_targets.R`.

INFORM Risk supplies the vulnerability indicators and, from the Lack of Coping
Capacity sheet, the infrastructure and health-expenditure indicators. Note that
`preprocess_inform()` reads the **0–10 component scores**, not the raw
underlying indicators; `pick_column()` asserts that range and will fail at
import if the workbook layout changes.

INFORM Severity is published **monthly** and is the only genuinely time-varying
input. It drives the crisis modifier via the driver taxonomy in
`crisis_modifier_matrix.xlsx`. Countries hosting several crises are collapsed to
one row (maximum across severity dimensions, union of drivers).

## Survey

Microsoft Forms export, saved as `survey/impact_survey_v1.xlsx`, plus the
worksite returns in `Healthcare proximity Assessment.xlsx`. These feed the
site-level proximity module only, which is unchanged in 4.10.

## CRPI (retired)

Retained for retro-compatibility only and no longer imported. Since 4.10 each
run snapshots its own scores and compares against the previous run, so no
historical spreadsheet needs updating.

## Not used

Removed in 4.10 after confirming zero downstream consumers:

- WHO household health expenditure (`RELAY_MAY2023_WIDE`)
- WHO antibiotic consumption (`19E688D`)
- WHO government health expenditure (`B9C6C79`)
- WHO ODA to health (`BBF3A64`)
- WHO access to essential medicines (`D2A45A5`)
- WHO unsafe WASH deaths (`ED50112`)
- UN IGME under-5 mortality rate
- INFORM headline aggregates (INFORM Risk, Hazard & Exposure, Vulnerability,
  Lack of Coping Capacity) and Lack of Reliability
