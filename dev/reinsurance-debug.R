# Debug helper: trace build_crossfilter_lookups inputs.
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
  shiny.port = 4400
)

# Trace
trace(
  blockr.dm:::build_crossfilter_lookups,
  tracer = quote({
    cat("\n=== build_crossfilter_lookups CALL ===\n")
    cat("tables names:", paste(names(tables), collapse = ", "), "\n")
    for (tn in names(tables)) {
      df <- tables[[tn]]
      cat("  ", tn, ": class=", paste(class(df), collapse=","), " nrow=", nrow(df), " ncol=", ncol(df), "\n", sep = "")
      for (cn in names(df)) {
        col <- df[[cn]]
        flag <- if (is.list(col) && !is.data.frame(col)) " <<LIST>>" else ""
        cat("    ", cn, ": ", class(col)[1], flag, "\n", sep = "")
      }
    }
    cat("active_dims:\n"); print(active_dims)
    cat("pks:\n"); print(pks)
    cat("fks:\n"); print(fks)
  }),
  print = FALSE
)

dashboard_json <- jsonlite::fromJSON(
  "/Users/christophsax/git/blockr/blockr.insurance/inst/examples/reinsurance.json",
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
  }
}
dashboard_json$payload$blocks$payload <- blocks

board <- blockr.core::blockr_deser(dashboard_json)
app <- serve(board, plugins = custom_plugins(manage_project()))
shiny::runApp(app, port = 4400, host = "127.0.0.1", launch.browser = FALSE)
