# Pricing Workflow with BI Dashboard - Scenario Comparison
#
# Two parallel scenarios with different loading factors:
#
# 1. Read data -> Run pricing -> Extract results
# 2. Visual filter (applies to both scenarios)
# 3. Scenario 1: Loading factor = 500
# 4. Scenario 2: Loading factor = 1000
#
# Compare KPIs and waterfall charts side by side!

# pkgload::load_all("../blockr.bi")
# pkgload::load_all(".")

library(blockr)
library(blockr.dag)
library(blockr.dm)
library(blockr.bi)
library(blockr.extra)
library(blockr.insurance)

# Get example data path from homer.gre package
example_path <- system.file("extdata/example", package = "homer.gre")

# Scenario comparison workflow
run_app(
  blocks = c(
    # =========================================================================
    # DATA PIPELINE
    # =========================================================================

    data_dm = new_dm_read_block(
      path = example_path,
      source = "path",
      infer_keys = FALSE
    ),

    pricing = new_pricing_module_block(),

    result_table = new_dm_pull_block(table = "MODEL_PRICE"),

    # =========================================================================
    # FILTER (applies to both scenarios)
    # =========================================================================

    filter = new_visual_filter_block(
      dimensions = c("Country"),
      measure = "Model_Price"
    ),

    # =========================================================================
    # SCENARIO 1: Loading factor = 500
    # =========================================================================

    calc_1 = new_function_block(
      fn = function(data, loading_factor = 500) {
        data |>
          dplyr::mutate(
            Final_Price = Model_Price * loading_factor
          )
      }
    ),

    kpis_1 = new_kpi_block(
      measures = c("Ground_Up_Premium", "Model_Price", "Final_Price"),
      digits = "0"
    ),

    walk_1 = new_waterfall_block(
      measures = c("Ground_Up_Premium", "Model_Price", "Final_Price")
    ),

    # =========================================================================
    # SCENARIO 2: Loading factor = 1000
    # =========================================================================

    calc_2 = new_function_block(
      fn = function(data, loading_factor = 1000) {
        data |>
          dplyr::mutate(
            Final_Price = Model_Price * loading_factor
          )
      }
    ),

    kpis_2 = new_kpi_block(
      measures = c("Ground_Up_Premium", "Model_Price", "Final_Price"),
      digits = "0"
    ),

    walk_2 = new_waterfall_block(
      measures = c("Ground_Up_Premium", "Model_Price", "Final_Price")
    )
  ),
  links = c(
    # Data pipeline
    new_link("data_dm", "pricing", "data"),
    new_link("pricing", "result_table", "data"),

    # Filter receives raw results
    new_link("result_table", "filter", "data"),

    # Scenario 1: filter -> calc_1 -> kpis_1/walk_1
    new_link("filter", "calc_1", "data"),
    new_link("calc_1", "kpis_1", "data"),
    new_link("calc_1", "walk_1", "data"),

    # Scenario 2: filter -> calc_2 -> kpis_2/walk_2
    new_link("filter", "calc_2", "data"),
    new_link("calc_2", "kpis_2", "data"),
    new_link("calc_2", "walk_2", "data")
  ),
  extensions = list(new_dag_extension())
)

# =============================================================================
# ADVANCED: Custom Parameters
# =============================================================================
#
# Pass custom parameter tables to override package defaults:
#
# param_path <- system.file("extdata/param", package = "homer.gre")
#
# blocks = c(
#   data_dm = new_dm_read_block(path = example_path, ...),
#   param_dm = new_dm_read_block(path = param_path, ...),
#   pricing = new_pricing_module_block(),
#   ...
# )
# links = c(
#   new_link("data_dm", "pricing", "data"),
#   new_link("param_dm", "pricing", "param"),  # <-- param input
#   ...
# )

# =============================================================================
# ADVANCED: Individual Modules
# =============================================================================
#
# Run individual pricing steps instead of full mod_price:
#
# pricing = new_pricing_module_block(module = "homer.gre::mod_base_premium")
# result = new_dm_pull_block(table = "BASE_PREMIUM")
#
# Available modules: mod_base_premium, mod_exposure_premium,
# mod_experience_premium, mod_blending, mod_model_price
