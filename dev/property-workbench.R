# Run the Property Workbench board against LOCAL source checkouts (your latest
# uncommitted changes to any blockr package). This is the pkgload::load_all()
# counterpart of the shipped, library()-based inst/examples/property-workbench.R:
# it just flips the loader and sources it, so the two can never drift.
#
# Run from an R session at the workspace root:
#   source("blockr.insurance/dev/property-workbench.R")
#
# (End users without the source checkouts run the shipped copy instead:
#   source(system.file("examples/property-workbench.R", package = "blockr.insurance")))

options(shiny.port = 3838, shiny.host = "0.0.0.0")

dev_local <- TRUE
source("blockr.insurance/inst/examples/property-workbench.R")
