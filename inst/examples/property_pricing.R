# Property pricing — minimal blockr workflow.
#
# Run from an R session after installing blockr.insurance:
#
#   library(blockr.insurance)
#   source(system.file("examples", "property_pricing.R",
#                      package = "blockr.insurance"))
#
# Demonstrates the rating-engine pattern: a pure R function
# `engine(inputs, params) -> outputs` (here `engine_property()`) wrapped in
# a generic `new_rating_engine_block()`. The engine takes a list of input
# tables and an optional list of parameter tables, both supplied as `dm`
# objects from upstream blocks, and returns a list of result tables exposed
# downstream as a `dm`.

options(
  # Dock's hidden-output detection misreports block visibility, so
  # `lazy_eval = TRUE` (the default) leaves blocks suspended and tables
  # never render. Force eager evaluation.
  blockr.lazy_eval = FALSE,
  blockr.dock_is_locked = FALSE
)

library(blockr.core)
library(blockr.dock)
library(blockr.dm)
library(blockr.insurance)

board <- new_dock_board(
  blocks = c(

    # === INPUTS ===
    locations = new_dataset_block("property_locations", "blockr.insurance"),
    claims    = new_dataset_block("property_claims",    "blockr.insurance"),
    inputs    = new_dm_block(infer_keys = FALSE),

    # === RATING ENGINE ===
    pricing = new_rating_engine_block(
      engine  = "engine_property",
      package = "blockr.insurance"
    ),

    # === RESULTS ===
    base_premium       = new_dm_pull_block(table = "base_premium"),
    exposure_premium   = new_dm_pull_block(table = "exposure_premium"),
    experience_premium = new_dm_pull_block(table = "experience_premium"),
    model_price        = new_dm_pull_block(table = "model_price")
  ),

  links = links(
    from = c(
      "locations", "claims",
      "inputs",
      "pricing", "pricing", "pricing", "pricing"
    ),
    to = c(
      "inputs", "inputs",
      "pricing",
      "base_premium", "exposure_premium",
      "experience_premium", "model_price"
    ),
    input = c(
      "locations", "claims",
      "inputs",
      "data", "data", "data", "data"
    )
  ),

  layout = dock_layouts(
    Inputs = dock_view("locations", "claims", "inputs", active = TRUE),
    Pricing = dock_view(
      "inputs", "pricing",
      "base_premium", "exposure_premium",
      "experience_premium", "model_price"
    )
  )
)

serve(board)
