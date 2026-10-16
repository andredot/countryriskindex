# Live scoring app ---------------------------------------------------------
#
# Upload the new GBD DALY extract and the INFORM Risk and Severity workbooks;
# the WHO indicators are downloaded automatically and the reference tables
# (location keys, crisis modifier matrix, HAQI) are read from the shared
# folder. Scores, decomposition and effective weights are recomputed on the
# fly with the same steps as the pipeline, and compared with the current
# pipeline run.
#
# Needs the 'writexl' package for the Excel download.

targets::tar_source()
baseline <- tryCatch(targets::tar_read(risk_score), error = function(e) NULL)

# launch.browser = TRUE opens the system browser: downloads do not work
# reliably in the RStudio Viewer pane.
shiny::runApp(run_scoring_app(baseline = baseline), launch.browser = TRUE)

# To use the local WHO copies instead of downloading them:
# shiny::runApp(run_scoring_app(baseline = baseline, download_who = FALSE),
#               launch.browser = TRUE)
#
# To refresh the WHO copies used by the pipeline itself:
# download_who_indicators(dirname(risk_input_path("who_uhc")))
