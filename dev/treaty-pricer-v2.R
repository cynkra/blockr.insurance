# Treaty pricer v2 — dev wrapper.
#
# Loads workspace source trees via pkgload, then sources the deployable app at
# inst/examples/treaty-pricer-v2/app.R. Board logic lives in app.R.
#
# Run from workspace root:
#   Rscript blockr.insurance/dev/treaty-pricer-v2.R
#
# Then open http://127.0.0.1:3838/

for (p in c("blockr.core", "blockr.dock", "blockr.dplyr",
            "blockr.input", "blockr.extra", "blockr.bi",
            "blockr.insurance")) {
  pkgload::load_all(p, quiet = TRUE)
}

src <- source("blockr.insurance/inst/examples/treaty-pricer-v2/app.R")
shiny::runApp(
  src$value,
  port = 3838,
  host = "0.0.0.0",
  launch.browser = FALSE
)
