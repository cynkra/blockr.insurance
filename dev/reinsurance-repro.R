# Local repro of the reinsurance app.
library(blockr.core)
library(blockr.io)
library(blockr.dplyr)
library(blockr.dm)
library(blockr.dock)
library(blockr.dag)
library(blockr.bi)
library(blockr.extra)
library(blockr.input)
library(blockr.insurance)
library(blockr.session)

options(
  blockr.lazy_eval = FALSE,
  blockr.dock_is_locked = FALSE,
  blockr.html_table_preview = TRUE,
  shiny.port = 4400
)

json_path <- system.file("examples", "reinsurance.json",
                         package = "blockr.insurance")
if (!nzchar(json_path)) {
  json_path <- "/Users/christophsax/git/blockr/blockr.insurance/inst/examples/reinsurance.json"
}

dashboard_json <- jsonlite::fromJSON(
  json_path,
  simplifyDataFrame = FALSE,
  simplifyMatrix = FALSE
)

blocks <- dashboard_json$payload$blocks$payload
for (bid in names(blocks)) {
  ctor <- blocks[[bid]]$constructor$constructor[[1]]
  if (identical(ctor, "new_static_block")) {
    rows <- blocks[[bid]]$payload$data
    df <- jsonlite::fromJSON(
      jsonlite::toJSON(rows, auto_unbox = TRUE, na = "null"),
      simplifyDataFrame = TRUE
    )
    blocks[[bid]]$payload$data <- df
  } else if (identical(ctor, "new_crossfilter_block")) {
    ad <- blocks[[bid]]$payload$active_dims
    if (is.list(ad)) {
      for (tn in names(ad)) {
        if (is.list(ad[[tn]]) && length(ad[[tn]]) == 0L) ad[[tn]] <- character()
      }
      blocks[[bid]]$payload$active_dims <- ad
    }
  }
}
dashboard_json$payload$blocks$payload <- blocks

board <- blockr.core::blockr_deser(dashboard_json)
app <- serve(board, plugins = custom_plugins(manage_project()))
shiny::runApp(app, port = 4400, host = "127.0.0.1", launch.browser = FALSE)
