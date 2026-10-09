# Live scoring app ---------------------------------------------------------
#
# Upload the input files (GBD, WHO, INFORM Risk and Severity, crisis modifier
# matrix) and the scores, decomposition and effective weights are recomputed
# on the fly, with the same steps as the pipeline. The current pipeline run
# is used as the baseline the new scores are compared against.
#
# Needs the 'writexl' package for the Excel download.

targets::tar_source()
baseline <- tryCatch(targets::tar_read(risk_score), error = function(e) NULL)

shiny::runApp(run_scoring_app(baseline = baseline))

# To prefill empty inputs with the files in the shared input folder:
# shiny::runApp(run_scoring_app(baseline = baseline,
#                               defaults = default_risk_input_paths()))
