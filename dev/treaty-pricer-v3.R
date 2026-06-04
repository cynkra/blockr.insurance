# Treaty pricer v3 — dev wrapper.
#
# Run from workspace root:
#   Rscript blockr.insurance/dev/treaty-pricer-v3.R
# Then open http://127.0.0.1:3838/

for (p in c("blockr.ui", "blockr.core", "blockr.dock", "blockr.dplyr",
            "blockr.input", "blockr.extra", "blockr.bi",
            "blockr.insurance")) {
  pkgload::load_all(p, quiet = TRUE)
}

src <- source("blockr.insurance/inst/examples/treaty-pricer-v3/app.R")
shiny::runApp(
  src$value,
  port = 3838,
  host = "0.0.0.0",
  launch.browser = FALSE
)
