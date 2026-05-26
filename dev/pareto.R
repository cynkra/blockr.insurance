# Reinsurance — Tower → piecewise Pareto → simulation (dev wrapper)
#
# Loads the workspace source trees via pkgload, then runs the deployable
# app at inst/examples/pareto/app.R. The board logic lives in app.R — see
# that file for the actual block wiring.
#
# Run from workspace root:
#   Rscript blockr.insurance/dev/pareto.R
#
# To run *outside* the dev container (laptop, Connect, shinyapps.io):
# install the deps once and serve app.R directly. See the install + run
# instructions at the top of inst/examples/pareto/app.R.

for (p in c("blockr.core", "blockr.dock", "blockr.dplyr",
            "blockr.input", "blockr.extra", "blockr.ggplot",
            "blockr.insurance")) {
  pkgload::load_all(p, quiet = TRUE)
}

# Source app.R directly (not shiny::runApp(dir)) so the working directory
# stays /workspace and the relative `pkgload::load_all("blockr.core")`
# calls inside app.R resolve. serve(board) at the bottom of app.R
# returns a shinyApp object; we capture it and hand it to runApp.
src <- source("blockr.insurance/inst/examples/pareto/app.R")
shiny::runApp(
  src$value,
  port = 3838,
  host = "0.0.0.0",
  launch.browser = FALSE
)
