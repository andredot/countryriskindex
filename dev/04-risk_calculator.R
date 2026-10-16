# Risk score calculator ----------------------------------------------------
#
# Pick the input files (GBD, WHO, INFORM Risk and Severity, crisis modifier
# matrix), compute the risk score with the same steps as the `risk_score`
# target, and download it as an Excel workbook. Inputs left empty use the
# files in the shared input folder (see risk_input_files()).
#
# Needs the 'writexl' package: renv::install("writexl"); renv::snapshot()

targets::tar_source()
# launch.browser = TRUE opens the system browser: downloads do not work
# reliably in the RStudio Viewer pane.
shiny::runApp(run_calculator_app(), launch.browser = TRUE)
