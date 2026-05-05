# Insurance Workbench — Homer Actuarial UI shape, blockr-side replica
#
# Run from workspace root:
#   Rscript blockr.insurance/inst/examples/insurance-workbench.R
#
# Spec: blockr.design/open/insurance-workbench/
# Story: same property book, two engine versions (v1 + v2 with CAT loading),
#        toggleable live, drill-into-policy via scatter click, side-by-side
#        compare at portfolio grain.

options(
  blockr.dock_is_locked    = FALSE,
  blockr.eval_parent_env   = asNamespace("stats"),
  blockr.html_table_preview = TRUE,
  # Dock's hidden-output detection misreports block visibility (per
  # blockr.insurance/inst/examples/property_pricing.R) — without this,
  # blocks stay suspended and panels never render.
  blockr.lazy_eval         = FALSE
  # shiny.port               = 3838L,
  # shiny.host               = "0.0.0.0"
)

pkgload::load_all("blockr.core")
pkgload::load_all("blockr.dock")
pkgload::load_all("blockr.dm")
pkgload::load_all("blockr.dplyr")
pkgload::load_all("blockr.bi")
pkgload::load_all("blockr.extra")
pkgload::load_all("blockr.insurance")

portfolio_dir <- blockr.insurance::default_portfolio_dir()

board <- new_dock_board(
  blocks = c(

    # === SETUP — read the portfolio inputs as a single long-format dm ===
    portfolio_inputs = new_portfolio_inputs_block(
      dir = portfolio_dir,
      block_name = "Portfolio inputs (locations + claims, with policy_id)"
    ),

    # === PORTFOLIO PRICE — runs whichever engine is selected via dropdown ===
    portfolio_price = new_price_block(
      engine     = "engine_property",
      package    = "blockr.insurance",
      block_name = "Portfolio price (engine selector)"
    ),
    portfolio_premium = new_dm_pull_block(
      table = "premium",
      block_name = "Portfolio premium table"
    ),

    # === PORTFOLIO WORKSPACE — KPI + drilldowns ===
    portfolio_kpi = new_kpi_block(
      measures = c("model_price", "exposure_premium"),
      agg_fun  = "sum",
      titles   = c(model_price = "Total Model Price",
                   exposure_premium = "Total Exposure Premium"),
      block_name = "Portfolio KPIs"
    ),
    portfolio_drill_country = new_drilldown_chart_block(
      chart_type = "bar",
      group_by   = "country",
      metric     = "model_price",
      agg_fn     = "sum",
      block_name = "Model price by country"
    ),
    portfolio_drill_peril = new_drilldown_chart_block(
      chart_type = "bar",
      group_by   = "peril",
      metric     = "model_price",
      agg_fn     = "sum",
      block_name = "Model price by peril"
    ),

    # === POLICY WORKSPACE — scatter (filters via click) → waterfall ===
    # The scatter is itself a filter block: its expr filters the upstream
    # premium by the clicked categorical value (`policy_id`). The waterfall
    # downstream sees only the selected policy's rows.
    policy_scatter = new_drilldown_chart_block(
      chart_type = "scatter",
      x_col      = "exposure_premium",
      y_col      = "model_price",
      series_by  = "policy_id",
      block_name = "Policy scatter (click a dot to drill)"
    ),
    policy_waterfall = new_waterfall_block(
      measures = c("base_premium", "exposure_premium",
                   "experience_premium", "risk_premium", "model_price"),
      block_name = "Policy waterfall (selected policy)"
    ),

    # === COMPARE — PORTFOLIO ===
    portfolio_price_v2 = new_price_block(
      engine     = "engine_property_v2",
      package    = "blockr.insurance",
      block_name = "Portfolio price - v2 (forced)"
    ),
    portfolio_premium_v2 = new_dm_pull_block(
      table = "premium",
      block_name = "Portfolio premium - v2"
    ),
    compare_portfolio = new_compare_block(
      key_cols     = c("policy_id", "location_id", "country", "peril"),
      measure_cols = c("base_premium", "exposure_premium",
                       "risk_premium", "model_price"),
      metric       = "diff",
      block_name   = "Compare v1 vs v2 (portfolio)"
    ),
    compare_portfolio_drill = new_drilldown_chart_block(
      chart_type = "bar",
      group_by   = "country",
      metric     = "model_price",
      agg_fn     = "sum",
      block_name = "Diff in model price by country"
    ),
    compare_portfolio_waterfall = new_waterfall_block(
      measures = c("base_premium", "exposure_premium",
                   "risk_premium", "model_price"),
      block_name = "Diff waterfall (portfolio)"
    )
  ),

  links = links(
    from = c(
      # Portfolio v1 chain
      "portfolio_inputs", "portfolio_price",
      # Portfolio drill / KPI off the v1 premium
      "portfolio_premium", "portfolio_premium", "portfolio_premium",
      # Policy: scatter filters portfolio_premium → waterfall downstream
      "portfolio_premium", "policy_scatter",
      # Portfolio v2 chain
      "portfolio_inputs", "portfolio_price_v2",
      # Compare
      "portfolio_premium", "portfolio_premium_v2",
      "compare_portfolio", "compare_portfolio"
    ),
    to = c(
      "portfolio_price",       "portfolio_premium",
      "portfolio_kpi",         "portfolio_drill_country", "portfolio_drill_peril",
      "policy_scatter",        "policy_waterfall",
      "portfolio_price_v2",    "portfolio_premium_v2",
      "compare_portfolio",     "compare_portfolio",
      "compare_portfolio_drill", "compare_portfolio_waterfall"
    ),
    input = c(
      "inputs",  "data",
      "data",    "data",                "data",
      "data",    "data",
      "inputs",  "data",
      "x",       "y",
      "data",    "data"
    )
  ),

  layout = dock_layouts(
    Setup = dock_view(
      "portfolio_inputs", "portfolio_price",
      "portfolio_price_v2",
      "portfolio_premium", "portfolio_premium_v2"
    ),
    Portfolio = dock_view(
      "portfolio_kpi",
      "portfolio_drill_country", "portfolio_drill_peril",
      active = TRUE
    ),
    Policy = dock_view(
      "policy_scatter", "policy_waterfall"
    ),
    `Compare-Portfolio` = dock_view(
      "compare_portfolio_drill", "compare_portfolio_waterfall"
    )
  )
)

serve(board)
