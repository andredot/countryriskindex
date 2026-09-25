library(targets)
library(tarchetypes)
library(shiny)
library(crew)  # parallel computing

controller <- crew::crew_controller_local(
  name = "countryriskindex_controller",
  workers = 1
)

# Set target-specific options such as packages.
tar_option_set(
  # error handling
  error = "abridge", # "continue" (do other), "null" (NULL if error)
  workspace_on_error = TRUE,
  # fast data formats
  format = "qs",
  # parallel computing
  storage = "worker",
  retrieval = "worker",
  controller = controller
)

# Define custom functions and other global objects.
# This is where you write source(\"R/functions.R\")
# if you keep your functions in external scripts.
tar_source()


# End this file with a list of target objects.
list(

  # Import your file from custom (shared) location, and preprocess them
  # Utilities file
  tar_target(cm_data, risk_input_path("crisis_modifier") |>
               import_cm_matrix()),

  # GBD section
  # GBD extract: DALYs, Rate, All ages, Both sexes, "All causes" + all level-2
  # causes. See sources.md for the exact vizhub request. Bump gbd_year() in
  # R/small_utils.R when a new round is downloaded. Input file names live in
  # risk_input_files() (R/calculator_.R), shared with run_calculator_app().
  tar_target(gbd_data, risk_input_path("gbd_daly") |>
               import_data()),
  tar_target(haqi_data,
             risk_input_path("haqi") |>
               import_data()),
  tar_target(location_keys, risk_input_path("gbd_location_keys") |>
               import_data()),
  tar_target(who_location_keys, risk_input_path("who_location_keys") |>
               import_excel()),

  tar_target(gbd_rates, preprocess_gbd_rates_by_cause(gbd_data, location_keys)),
  tar_target(haq_index, preprocess_haq_index(haqi_data, location_keys)),
  tar_target(fig_daly_bar, createimg_daly_bars(gbd_rates)),
  tar_target(map_daly_avg, createmap_DALY_avg(gbd_rates)),
  tar_target(map_daly_scores, createmap_DALY_score(gbd_rates)),


  # WHO section
  ## indicators
  tar_target(x9a706fd_all_latest, risk_input_path("who_uhc") |>
               import_data()),
  tar_target(x217795a_all_latest, risk_input_path("who_doctors") |>
               import_data()),
  # Only two WHO indicators are consumed by the index (see sources.md):
  # 9A706FD (UHC service coverage) and 217795A (density of doctors).
  tar_target(who_indicators, preprocess_who_data(x9a706fd_all_latest,
                                                 x217795a_all_latest,
                                                 who_location_keys)),
  # EU INFORM section
  tar_target(inform_risk_path, risk_input_path("inform_risk")),
  tar_target(inform_risk_data, import_inform_excel(inform_risk_path)),
  tar_target(inform_lcc_data, import_inform_excel(inform_risk_path, sheet = 5)),
  tar_target(inform_cap, preprocess_inform(inform_risk_data, inform_lcc_data)),

  tar_target(inform_severity_path, risk_input_path("inform_severity")),
  tar_target(inform_severity_data, import_severity_excel(inform_severity_path)),
  tar_target(inform_severity, preprocess_severity(inform_severity_data)),
  tar_target(map_sev_score, createmap_severity_score(risk_score)),
  tar_target(map_adj_risk_score, createmap_adjusted_risk_score(risk_score)),

  tar_target(fig_cap_bar, createimg_cap_bars(risk_score)),
  tar_target(fig_vul_bar, createimg_vul_bars(risk_score)),
  tar_target(map_cap_score, createmap_cap_score(risk_score)),
  tar_target(map_vul_score, createmap_vul_score(risk_score)),

  # Risk making
  tar_target(merged_indicators, merge_health_datasets(gbd_rates,
                                                      haq_index,
                                                      inform_cap,
                                                      who_indicators,
                                                      inform_severity)),
  # Frozen 2025 reference distribution. Normalisation bounds are read from this
  # file rather than recomputed each run, so a country's score does not move
  # merely because other countries moved. Regenerate ONLY when deliberately
  # rebasing the index: see dev/regenerate_reference_quantiles.R.
  # After regenerating the reference, run
  #   targets::tar_invalidate(reference_quantiles)
  # to force the downstream scores to rebuild.
  tar_target(reference_quantiles, read_reference_quantiles()),

  # score_risk() is shared with the calculator app (run_calculator_app()).
  tar_target(risk_score, score_risk(merged_indicators, cm_data,
                                    reference_quantiles)),

  # Severity diagnostics: what the crisis modifier actually did, and why
  tar_target(severity_calibration, severity_calibration_table()),
  tar_target(fig_severity_calibration,
             create_severity_calibration_plot(severity_calibration)),
  tar_target(fig_severity_uplift, createimg_severity_uplift(risk_score)),
  tar_target(severity_effect_table, summarise_severity_effect(risk_score)),

  # Data completeness
  tar_target(map_completeness, createmap_completeness(risk_score)),

  # Score decomposition. This is the computationally expensive part of the
  # explanation (exact Shapley values per pillar, per country), so it is
  # computed ONCE here and read by the report and the Shiny app rather than
  # recalculated on the fly.
  tar_target(risk_decomposition,
             build_decomposition(risk_score,
                                 reference_quantiles = reference_quantiles)),
  tar_target(decomposition_summary, risk_decomposition$summary),
  tar_target(fig_waterfall_top,
             create_decomposition_waterfall(
               risk_decomposition,
               country = risk_decomposition$summary$country[1],
               level = "indicator")),
  tar_target(fig_contributions_top,
             create_contribution_bars(
               risk_decomposition,
               country = risk_decomposition$summary$country[1])),

  # Validation: correlation (are we measuring similar or different things?)
  # and PCA (how many independent things are we actually measuring?)
  tar_target(pca_validation, run_pca_validation(risk_score)),
  tar_target(pca_dimensionality, summarise_dimensionality(pca_validation)),
  tar_target(fig_pca_scree, create_pca_scree(pca_validation)),
  tar_target(fig_pca_loadings, create_pca_loadings(pca_validation)),
  tar_target(fig_pca_biplot, create_pca_biplot(pca_validation)),

  # Run-to-run comparison, replacing the retired CRPI benchmark
  tar_target(previous_snapshot, load_previous_snapshot()),
  tar_target(run_comparison, compare_runs(risk_score, previous_snapshot)),
  tar_target(fig_run_comparison, create_run_comparison_plot(run_comparison)),
  tar_target(current_snapshot, save_run_snapshot(risk_score),
             format = "file"),
  tar_target(radar_data, extract_radar_data(risk_score)),
  tar_target(corrected_radar_data, update_risk_with_cm(cm_data, risk_score, radar_data)),
  tar_target(map_risk_score, createmap_risk_score(risk_score)),
  tar_target(fig_3d_risk, create_3d_plot(db = risk_score,
                                         x_var = "hazard_score",
                                         y_var = "vulnerability_score",
                                         z_var = "capacity_score",
                                         color_var = "overall_risk",
                                         label_var = "country",
                                         labels = c("Hazard", "Vulnerability", "Capacity")
                                         )),
  tar_target(fig_risk_bars, createimg_risk_bars(risk_score)),
  tar_target(fig_risk_hist, createimg_risk_hist(risk_score)),
  tar_target(fig_risk_shift, create_lollipop_plot_shift(risk_score)),
  tar_target(corplot, create_correlation_matrix_with_groups(risk_score)),

  # Real-time localisation information
  tar_target(raw_survey, get_input_data_path("/survey/impact_survey_v1.xlsx") |>
               import_cm_matrix()),
  tar_target(survey, preprocess_survey(raw_survey)),
  tar_target(long, pivot_disease_data(survey)),

  tar_target(proximity_assessment_raw, get_input_data_path("/Healthcare proximity Assessment.xlsx") |>
               import_cm_matrix()),
  tar_target(proximity_assessment, preprocess_proximity_assessment(proximity_assessment_raw, merged_indicators)),
  tar_target(proximity_adjustments, add_proximity_score(long, proximity_assessment)),
  tar_target(worksite_risk, add_localised_risk(risk_score, proximity_adjustments)),

  # QUARTO DOCUMENTS and REPORT
  tar_quarto(report, here::here("reports/report.qmd")),
  # tar_quarto(vul_cap_components, here::here("reports/vulcomb.qmd")),
  # tar_quarto(disease_clustering, here::here("reports/disease_clustering.qmd")),

  # SHINY APP
  # NOTE: this target builds the app object; it does not launch it. To open the
  # explorer interactively, from a fresh session:
  #   targets::tar_read(shiny_explorer)
  # The object carries its own copy of the helper functions (see
  # self_contained() in R/run_app.R), so tar_source() is not needed first.
  tar_target(shiny_explorer,
             run_app(risk_score, radar_data, corrected_radar_data,
                     risk_decomposition),
             cue = tar_cue(mode = "always")),

  # Decide what to share with other, and do it in a standard RDS format
  tar_target(
    objectToShare,
    list(
      RISK = risk_score,
      MSSP = worksite_risk
    )
  ),
  tar_target(
    shareOutput,
    share_objects(objectToShare),
    format = "file",
    pattern = map(objectToShare)
  ),


  tar_target(JustDontCareLastComma, NULL)
)
