# countryriskindex 0.2.0 (index version 4.10)

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
  `Road density...16`. It matches by prefix, requires an unambiguous match, and
  asserts the expected value range, so a workbook layout change fails at import
  rather than quietly at interpretation.

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
