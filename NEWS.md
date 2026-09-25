# countryriskindex 0.2.0 (index version 4.10)

## Score decomposition (new)

Every published score can now be traced back to the inputs that produced it,
exactly rather than approximately. New module `R/explain_.R`.

* `build_decomposition()` is the single pipeline target (`risk_decomposition`).
  It is the expensive step, so it runs once and the report and Shiny app read
  its output rather than recomputing.
* **Level 1 is exact by construction.** The index is a geometric mean, which is
  additive in logs, so `decompose_components()` splits
  `log(risk) - log(reference)` across hazard, vulnerability and capacity with no
  approximation and no weighting choice to dispute.
* **Level 2 uses exact Shapley values.** Extending the log decomposition below
  the component level does not work: `invert_0_10()` and `normalise_quantiles()`
  are affine, and on realistic inputs the naive extension returns contributions
  of the *wrong sign*. `shapley_contributions()` enumerates all 16 coalitions
  per pillar, which is exact and cheap; it refuses more than 12 inputs rather
  than attempting a flat decomposition over every indicator.
* **The levels reconcile without a correction factor**, because a pillar's
  Shapley values sum to `log(pillar_c) - log(pillar_ref)` and are then scaled by
  1/3. Verified by test to 1e-9.
* `decompose_severity()` splits the crisis uplift by component on the same
  footing, reporting the log-score effect alongside the `delta_*` log-odds shift
  that produced it.
* `build_reference_profile()` derives each pillar's reference from the reference
  *sub-components* rather than averaging observed pillar scores. Averaging them
  gives a baseline unreachable from its own inputs, and the two levels then fail
  to reconcile.
* `resolve_pillar_bounds()` fixes the normalisation bounds across every coalition
  evaluation. Without this the function being decomposed changes between
  coalitions and the Shapley values are meaningless; with a single row, quantile
  normalisation also degenerates to `q01 == q99`.
* `summarise_decomposition()` reports the dominant driver, the mitigating
  factor, and the crisis contribution per country.
* `attach_confidence()` tags each contribution with the completeness of its
  component, so a large contribution built on missing inputs is visible.
* `create_decomposition_waterfall()` and `create_contribution_bars()` render it.
  The waterfall's running product reproduces the published score exactly.
* `aggregate_vulnerability()` / `aggregate_capacity()` factored out of
  `add_*_score()` so scoring and decomposition share one definition and cannot
  drift. Vulnerability sub-components are now stored on the data frame with a
  `vc_` prefix.

### Fixed

* `logit_shift()` could move a score **against** the sign of its shift. A
  component clipped at 1 came back as 0.9995 under a positive adjustment,
  producing a small negative severity contribution. It is now
  direction-preserving, and `delta == 0` is exactly the identity.

## Shiny explorer repaired

* The lollipop tab is replaced by a **Score Decomposition** tab (waterfall,
  contribution bars, summary table) with a component/indicator toggle and a
  switch for the crisis modifier. It reads the precomputed target.
* `collapse_severity_by_country()` dropped `CRISIS`, a character column, which
  the radar tab pulls to label the correction. It is now retained, with multiple
  crises joined by semicolons.
* The radar tab's `is.na(drivers_value) || length(drivers_value) == 0` errors
  under R >= 4.3 on zero-length or length > 1 operands - and a country with
  several crises returns more than one. Rewritten, and guarded for a missing
  `CRISIS` column.
* `ggradar()` was called with two axis labels in one branch and five in the
  other; it expects three.
* The histogram passed a zero-length `count` into `geom_segment()` when the
  selected country fell outside the binned range, silently breaking the layer.
  Also guards non-finite scores.
* Country choices are sorted and `NA`-free.


## Input sources reduced to those actually used

The pipeline downloaded and parsed a number of inputs that no downstream
function ever read. `sources.md` is now the authoritative list, and everything
in it is consumed.

* **WHO: eight datasets to two.** Only `9A706FD` (UHC service coverage) and
  `217795A` (density of doctors) reach the index, both via the capacity
  component. Household health expenditure, antibiotic consumption, government
  health expenditure, ODA to health, access to essential medicines and
  unsafe-WASH deaths are no longer fetched. `preprocess_who_data()` now takes
  three arguments instead of nine.
* `preprocess_who_data()` additionally returns `year_uhc` and `year_doctors`.
  WHO's "most recent year available" differs systematically between well- and
  poorly-measured countries, and that vintage should be visible.
* **UN IGME under-5 mortality removed.** `preprocess_u5mr()`,
  `createmap_u5mr_avg()`, `createmap_u5mr_did()` and `createimg_u5mr_bars()`
  are gone: the source is not in `sources.md` and had no pipeline targets.
* **INFORM headline aggregates removed.** `preprocess_inform()` no longer
  extracts INFORM Risk, Hazard & Exposure, Vulnerability, Lack of Coping
  Capacity or Lack of Reliability. `createmap_cap_avg()` is removed with them.
* `createimg_cap_bars()` coloured `capacity_score` by the raw INFORM coping
  capacity aggregate, and `createimg_risk_hist()` coloured `overall_risk` by
  raw INFORM risk. Both now use the computed score, which is what is being
  plotted.

## GBD extract: all level-2 causes, no hard-coded IDs

* `preprocess_gbd_rates_by_cause()` previously selected ten causes by
  hard-coded `cause_id` and built an "Other NCDs" aggregate from ten more.
  Both were fragile across GBD rounds and arbitrary in composition. It now
  pivots on `cause_name` and keeps whatever causes the extract contains, per
  the "All causes + all level-2 causes" request in `sources.md`.
* A missing "All causes" column is now a hard error naming the causes found,
  rather than a downstream `NA`.
* Duplicated country-cause rows (from leaving more than one year, sex or age
  group in the download) are averaged with a warning instead of silently
  producing list columns.
* New `gbd_cause_columns()` derives the cause set from the data. The radar
  chart, correlation matrix and Shiny explorer use it instead of hard-coded
  lists, so they adapt when GBD renames a cause.
* New `default_cause_groups()` maps level-2 causes onto the radar channels;
  causes absent from the extract are skipped with a warning.
* `create_correlation_matrix_with_groups()` gains `max_causes` (default 12) and
  keeps the causes with the largest cross-country spread, since a heatmap with
  20+ cause rows is unreadable.
* New `gbd_year()` holds the GBD round in one place; both input paths derive
  from it. Currently `"2023"`.


Corrective release. The conceptual framework is unchanged; five behaviours in
the implementation that were distorting results have been fixed, and the
diagnostics needed to detect their recurrence have been added.

## Breaking changes

* `add_severity()` no longer uses the convex-pull formula. Scores from this
  release are **not** comparable with 4.00 without re-running the pipeline. The
  old behaviour is available via `add_severity(method = "convex")` for
  comparison only; it emits a warning.
* `add_hazard_score()` now uses quantile normalisation rather than division by
  the observed maximum. The v4.00 behaviour is available via `method = "max"`.
* `add_capacity_score()` normalises `doctor_density` before aggregation.
  Capacity scores will change for every country.
* `geometric_mean()` gains `min_n`, and `add_overall_risk()` defaults to
  `min_n = 3`. Countries missing an entire component now return `NA` instead of
  a score computed from the remaining components.
* The legacy CRPI imports have been removed: `import_crpi_excel()`,
  `preprocess_crpi()` and `create_lollipop_plot_comp()` are gone, along with
  the `crpi*` and `fig_lolliplot1/2/4/5` targets.

## Crisis severity

* **The crisis modifier no longer overrides the structural model.** v4.00
  applied `risk + ((S - 1) / 4) * (1 - risk)`, a convex pull towards 1.0: at
  severity 5 it returned exactly 1.0 for every country, and at severity 3 it
  imposed a floor of 0.5. From a baseline of 0.35, moving from severity 1 to 2
  added +0.16, while *doubling* measured vulnerability added only +0.09 — half a
  point of severity outweighed doubling vulnerability.
* Severity is now routed onto the component it acts on
  (`add_severity_components()`): the impact/conditions dimensions to hazard,
  society and safety to vulnerability, operating environment to capacity. Which
  channels activate is determined by the country's INFORM Severity drivers via
  the existing crisis modifier matrix, which was previously wired only to the
  radar chart.
* Adjustment is applied on the log-odds scale (`logit_shift()`): bounded,
  monotone, and interpretable as a log odds ratio. Because the components are
  then combined with a geometric mean, a single-channel shift is damped to
  roughly one third in log space.
* Driver counts convert to a saturating weight `n / (n + kappa)`, so long driver
  lists cannot accumulate unbounded uplift. A `w_floor` prevents a gap in the
  driver matrix from silently switching the modifier off.
* An absent severity record and a severity of 1 now both give zero intensity,
  removing the v4.00 discontinuity where merely entering the INFORM Severity
  list was worth roughly +0.16.
* Net effect: a minor single-driver crisis moves a mid-range country by about
  +0.018 (was +0.163, a ninefold reduction); a severe multi-driver crisis still
  produces a visible +0.17.
* `severity_calibration_table()` and `create_severity_calibration_plot()` make
  the coefficients auditable. `summarise_severity_effect()` reports per-country
  what the modifier did and what v4.00 would have produced.

## Normalisation

* `normalise_quantiles()` accepts frozen `q01`/`q99` bounds. All three
  components read them from `inst/extdata/reference_quantiles_2025.csv`, so a
  country's score no longer moves because *other* countries moved.
  Regenerate deliberately with `dev/regenerate_reference_quantiles.R`;
  `check_reference_coverage()` warns when bounds are absent.
* `doctor_density` is normalised to 0-100 on a log axis. Previously it entered
  the capacity geometric mean as a raw rate spanning roughly 0.1 to 85 while the
  other three components spanned 0-100, carrying several times their combined
  influence in log space.
* `geometric_mean()` clamps non-positive inputs, so a single zero from quantile
  normalisation no longer collapses the whole aggregate to zero.

## Data quality

* `add_data_completeness()` counts missing indicators per component, publishes
  `data_completeness`, and sets `low_confidence` when either the total count or
  the share missing within one component exceeds a threshold. Surfaced via
  `createmap_completeness()` and a table in the report.
* `preprocess_severity()` now calls `collapse_severity_by_country()`, reducing
  multiple crises per country to one row (maximum for severity dimensions, union
  for driver flags). The severity join in `merge_health_datasets()` asserts
  `relationship = "one-to-one"` so silent row duplication fails loudly.
* `pick_column()` replaces positional column selection such as
  `Road density...16`. The INFORM Lack of Coping Capacity sheet carries road
  density twice - raw (roughly 1 to 850) and as the normalised INFORM 0-10
  score - so the function selects the candidate whose observed values satisfy
  the expected range rather than the first or last match. That is stable across
  workbook revisions in a way a column position is not. When no candidate fits,
  or several do, it errors and reports each candidate's observed range instead
  of guessing.

## Validation

* New PCA module (`run_pca_validation()`, `summarise_dimensionality()`,
  `create_pca_scree()`, `create_pca_loadings()`, `create_pca_biplot()`). The
  correlation matrix shows whether indicators capture similar or different
  things; the PCA shows how many independent dimensions the index actually has.
* The correlation matrix now skips absent variables with a warning instead of
  failing the report.
* The report no longer claims that the hazard score's correlation with the
  all-cause DALY rate validates the aggregation. The hazard score is a monotone
  transformation of that rate, so the correlation is 1 by construction.

## Run-to-run comparison

* Each run writes a snapshot (`save_run_snapshot()`) and compares against the
  most recent previous one (`load_previous_snapshot()`, `compare_runs()`,
  `create_run_comparison_plot()`). The first run establishes the baseline and
  reports that rather than failing.

## Fixes

* `update_score()` honoured `times` incorrectly: it decremented whenever
  `times > 1`, so `times = 1` and `times = 2` behaved identically and 3 or more
  collapsed to two applications. It now applies exactly `times` times, capped at
  `max_times`, and is implemented in base R.
* The report's severity section described multiplying risk by a factor of up to
  2; the code performed a convex combination. The two agreed only at exactly
  `risk = 0.5`. Documentation and implementation now match.
* Report execution suppresses knitr warnings and messages, which were being
  rendered into the published output.
* The author contributions placeholder has been replaced with a draft statement
  (verify before publication).

## Known limitations, unchanged in this release

* Crisis severity is still applied at national level, so a geographically
  limited crisis affects the whole country. Sub-national scoping is deferred.
* The site-level proximity module is unchanged. Note that its odds-additive
  adjustment has a *diminishing* effect as baseline risk rises, which is the
  opposite of what the report describes; this is scheduled with the sub-national
  work.
* No uncertainty or sensitivity analysis, and no external criterion validation.
* Input vintages remain heterogeneous (HAQI from GBD 2019; WHO indicators use
  the most recent year available per country).
