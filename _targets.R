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
  tar_target(cm_data, get_input_data_path("crisis_modifier_matrix.xlsx") |>
               import_cm_matrix()),

  # GBD section
  tar_target(gbd_data, get_input_data_path("GBD/IHME-GBD_2021_DALY.csv") |>
               import_data()),
  tar_target(haqi_data,
             get_input_data_path("GBD/HAQI/IHME_GBD_2019_HAQ_1990_2019_DATA_Y2022M012D21.csv") |>
               import_data()),
  tar_target(location_keys, get_input_data_path("GBD/IHME_GBD_2021_location_keys.csv") |>
               import_data()),
  tar_target(who_location_keys, get_input_data_path("WHO/WHO_loc_keys.xlsx") |>
               import_excel()),

  tar_target(gbd_rates, preprocess_gbd_rates_by_cause(gbd_data, location_keys)),
  tar_target(haq_index, preprocess_haq_index(haqi_data, location_keys)),
  tar_target(fig_daly_bar, createimg_daly_bars(gbd_rates)),
  tar_target(map_daly_avg, createmap_DALY_avg(gbd_rates)),
  tar_target(map_daly_scores, createmap_DALY_score(gbd_rates)),


  # WHO section
  ## indicators
  tar_target(relay_may2023_wide, get_input_data_path("WHO/RELAY_MAY2023_WIDE.csv") |>
               import_data()),
  tar_target(x9a706fd_all_latest, get_input_data_path("WHO/9A706FD_ALL_LATEST.csv") |>
               import_data()),
  tar_target(x19e688d_all_latest, get_input_data_path("WHO/19E688D_ALL_LATEST.csv") |>
               import_data()),
  tar_target(x217795a_all_latest, get_input_data_path("WHO/217795A_ALL_LATEST.csv") |>
               import_data()),
  tar_target(b9c6c79_all_latest, get_input_data_path("WHO/B9C6C79_ALL_LATEST.csv") |>
               import_data()),
  tar_target(bbf3a64_all_latest, get_input_data_path("WHO/BBF3A64_ALL_LATEST.csv") |>
               import_data()),
  tar_target(d2a45a5_all_latest, get_input_data_path("WHO/D2A45A5_ALL_LATEST.csv") |>
               import_data()),
  tar_target(ed50112_all_latest, get_input_data_path("WHO/ED50112_ALL_LATEST.csv") |>
               import_data()),
  tar_target(who_indicators, preprocess_who_data( relay_may2023_wide,
                                                  x9a706fd_all_latest,
                                                  x19e688d_all_latest,
                                                  x217795a_all_latest,
                                                  b9c6c79_all_latest,
                                                  bbf3a64_all_latest,
                                                  d2a45a5_all_latest,
                                                  ed50112_all_latest,
                                                  who_location_keys)),
  # EU INFORM section
  tar_target(inform_risk_path, get_input_data_path("EU/INFORM_Risk_2025_v070.xlsx")),
  tar_target(inform_risk_data, import_inform_excel(inform_risk_path)),
  tar_target(inform_lcc_data, import_inform_excel(inform_risk_path, sheet = 5)),
  tar_target(inform_cap, preprocess_inform(inform_risk_data, inform_lcc_data)),

  tar_target(inform_severity_path, get_input_data_path("EU/INFORM_Severity_March_2025.xlsx")),
  tar_target(inform_severity_data, import_severity_excel(inform_severity_path)),
  tar_target(inform_severity, preprocess_severity(inform_severity_data)),
  tar_target(map_sev_score, createmap_severity_score(risk_score)),
  tar_target(map_adj_risk_score, createmap_adjusted_risk_score(risk_score)),

  tar_target(fig_cap_bar, createimg_cap_bars(risk_score)),
  tar_target(fig_vul_bar, createimg_vul_bars(risk_score)),
  tar_target(map_cap_score, createmap_cap_score(risk_score)),
  tar_target(map_vul_score, createmap_vul_score(risk_score)),

  # Former risk score
  tar_target(crpi1, get_input_data_path("CRPI/Country Risk Profile Index X3.12.1.xlsx") |>
               import_crpi_excel()),
  tar_target(crpi2, get_input_data_path("CRPI/Country Risk Profile Index X3.12.2.xlsx") |>
               import_crpi_excel()),
  tar_target(crpi, preprocess_crpi(crpi1, crpi2)),
  tar_target(fig_lolliplot1, create_lollipop_plot_comp(risk_score, crpi)),
  tar_target(fig_lolliplot2, create_lollipop_plot_comp(risk_score, crpi,
                                                   sort_by_difference = TRUE)),
  tar_target(fig_lolliplot3, create_lollipop_plot_shift(risk_score)),
  tar_target(fig_lolliplot4, create_lollipop_plot_comp(risk_score, crpi,
                                                       x_col = "severity_adjusted_risk",
                                                       y_col = "Index_adjusted",
                                                       color_x = "deepskyblue",
                                                       color_y = "darkblue")),
  tar_target(fig_lolliplot5, create_lollipop_plot_comp(risk_score, crpi,
                                                       sort_by_difference = TRUE,
                                                       x_col = "severity_adjusted_risk",
                                                       y_col = "Index_adjusted",
                                                       color_x = "deepskyblue",
                                                       color_y = "darkblue")),



  # Risk making
  tar_target(merged_indicators, merge_health_datasets(gbd_rates,
                                                      haq_index,
                                                      inform_cap,
                                                      who_indicators,
                                                      inform_severity)),
  tar_target(risk_score, merged_indicators |>
               add_hazard_score() |>
               add_vulnerability_score() |>
               add_capacity_score() |>
               add_overall_risk() |>
               add_severity()
             ),
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
  tar_quarto(vul_cap_components, here::here("reports/vulcomb.qmd")),
  tar_quarto(disease_clustering, here::here("reports/disease_clustering.qmd")),

  # SHINY APP
  tar_target(shiny_explorer,
             run_app(risk_score, radar_data, corrected_radar_data),
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
