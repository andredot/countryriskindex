# Deploy the live scoring app to shinyapps.io --------------------------------
#
# The app is deployed as a self-contained folder (deploy/scoring_app/), so the
# server does not need the countryriskindex package: app.R sources the R files
# the app uses, and the static inputs (location keys, HAQI, crisis modifier
# matrix, reference quantiles) and a baseline snapshot travel with it. Users of
# the deployed app upload only the GBD and INFORM files; the WHO indicators are
# downloaded when the app starts.
#
# Do not deploy the project root: without an app.R there, the server renders
# README.Rmd as an interactive document, which needs the package installed.

targets::tar_source()

baseline <- tryCatch(targets::tar_read(risk_score), error = function(e) NULL)
bundle <- build_scoring_bundle("deploy/scoring_app", baseline = baseline)

# Optional: try the bundle locally first, exactly as the server will run it.
# shiny::runApp(bundle, launch.browser = TRUE)

rsconnect::deployApp(
  appDir = bundle,
  appName = "countryriskindex",  # keeps the existing URL
  forceUpdate = TRUE
)
