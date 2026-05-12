# Local reproduction of the deployed UWR app.
# Mirrors blockr.deploy/shinyproxy-hetzner/apps/uwr/app.R.
#
# Run from this directory: setwd("/Users/christophsax/git/blockr/blockr.insurance/dev")
# then source("uwr-repro.R")

library(blockr.core)
library(blockr.io)
library(blockr.dplyr)
library(blockr.dm)
library(blockr.dock)
library(blockr.dag)
library(blockr.bi)
library(blockr.input)
library(blockr.insurance)
library(blockr.session)

pins_dir <- tempfile("blockr-pins-")
dir.create(pins_dir, recursive = TRUE)

options(
  blockr.lazy_eval = FALSE,
  blockr.dock_is_locked = FALSE,
  blockr.html_table_preview = TRUE,
  blockr.session_url_params = TRUE,
  blockr.session_mgmt_backend = pins::board_folder(pins_dir),
  shiny.port = 4399
)

dashboard_json <- jsonlite::fromJSON(
  "UWR.json",
  simplifyDataFrame = FALSE,
  simplifyMatrix = FALSE
)

# Rebase absolute paths to the bundled CSVs in inst/extdata.
extdata <- system.file("extdata", package = "blockr.insurance")
dashboard_json$payload$blocks$payload$employees_read$payload$path <-
  file.path(extdata, "employees.csv")
dashboard_json$payload$blocks$payload$claims_read$payload$path <-
  file.path(extdata, "life_claims.csv")

board <- blockr.core::blockr_deser(dashboard_json)

app <- serve(board, plugins = custom_plugins(manage_project()))
shiny::runApp(app, port = 4399, host = "127.0.0.1", launch.browser = FALSE)
