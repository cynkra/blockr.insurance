# Property Workbench — Base vs Challenger portfolio simulations
#
# Run from workspace root:
#   Rscript blockr.insurance/inst/examples/property-workbench.R
#
# Spec: blockr.design/open/insurance-workbench/
# Story: two parallel portfolio simulations — Base and Challenger — each
#        combining a choice of pricing engine with its own editable parameter
#        set. Out of the box Base = (engine_property, default rates) and
#        Challenger = (engine_property_v2, default rates), so the Compare
#        tab reads as the CAT-loading effect. Edit either side's base_rate
#        grid, or swap engines via the price block dropdown, and the Compare
#        waterfall reflects the combined diff. Reset Challenger to match
#        Base (same engine, no rate edits) and the diff goes to zero.

options(
  blockr.dock_is_locked    = FALSE,
  blockr.eval_parent_env   = asNamespace("stats"),
  blockr.html_table_preview = TRUE,
  # Dock's hidden-output detection misreports block visibility — without
  # this, blocks stay suspended and panels never render.
  blockr.lazy_eval         = FALSE
)

pkgload::load_all("blockr.core")
pkgload::load_all("blockr.dock")
pkgload::load_all("blockr.dm")
pkgload::load_all("blockr.dplyr")
pkgload::load_all("blockr.bi")
pkgload::load_all("blockr.input")
pkgload::load_all("blockr.extra")
pkgload::load_all("blockr.session")
pkgload::load_all("blockr.insurance")

portfolio_dir <- blockr.insurance::default_portfolio_dir()

board <- new_dock_board(
  blocks = c(

    # === SETUP — portfolio inputs shared by both runs ===
    portfolio_inputs = new_portfolio_inputs_block(
      dir = portfolio_dir,
      block_name = "Portfolio inputs (locations + claims, with policy_id)"
    ),

    # === SHARED PARAM DEFAULTS — both runs seed their grids from these ===
    # property_params ships as a list of 4 tables. Static blocks hold the
    # defaults; Base and Challenger grids edit base_rate independently.
    # Cleaner shape (one dm-aware CRUD block) is parked in
    # blockr.design/open/dm-crud-input/1-motivation.md.
    country_factor_src = new_static_block(
      data       = blockr.insurance::property_params$country_factor,
      block_name = "country_factor (default)"
    ),
    base_rate_src = new_static_block(
      data       = blockr.insurance::property_params$base_rate,
      block_name = "base_rate (default)"
    ),
    expenses_src = new_static_block(
      data       = blockr.insurance::property_params$expenses,
      block_name = "expenses (default)"
    ),
    cat_factor_src = new_static_block(
      data       = blockr.insurance::property_params$cat_factor,
      block_name = "cat_factor (default, used by v2)"
    ),

    # === BASE RUN — engine + params + price + premium ===
    base_grid = new_grid_block(
      state      = list(key_col = "country"),
      block_name = "Base — base_rate (editable)"
    ),
    base_params = new_dm_block(
      infer_keys = FALSE,
      block_name = "Base — params dm"
    ),
    base_price = new_price_block(
      engine     = "engine_property",
      package    = "blockr.insurance",
      block_name = "Base — price (engine selector)"
    ),
    base_premium = new_dm_pull_block(
      table = "premium",
      block_name = "Base — premium table"
    ),

    # === ENGINE-SELECTION PREVIEW — policy picker + waterfall on Base ===
    # Lets the user see the price build-up update live as they tweak rates or
    # swap engines, without leaving the config tab. The policy picker is a
    # single-dim crossfilter (chip per policy_id) for fast drill-in.
    preview_xfilter = new_crossfilter_block(
      active_dims = list(.tbl = "policy_id"),
      measure     = ".tbl.model_price",
      agg_func    = "sum",
      block_name  = "Policy picker"
    ),
    preview_waterfall = new_waterfall_block(
      measures = c("base_premium", "exposure_premium",
                   "experience_premium", "risk_premium", "model_price"),
      block_name = "Price build-up (Base)"
    ),

    # === CHALLENGER RUN — symmetric, defaults to v2 engine ===
    chal_grid = new_grid_block(
      state      = list(key_col = "country"),
      block_name = "Challenger — base_rate (editable)"
    ),
    chal_params = new_dm_block(
      infer_keys = FALSE,
      block_name = "Challenger — params dm"
    ),
    chal_price = new_price_block(
      engine     = "engine_property_v2",
      package    = "blockr.insurance",
      block_name = "Challenger — price (engine selector)"
    ),
    chal_premium = new_dm_pull_block(
      table = "premium",
      block_name = "Challenger — premium table"
    ),

    # === PORTFOLIO WORKSPACE — KPI + drilldowns off the Base run ===
    portfolio_kpi = new_tile_block(
      showcase = "number",
      state = list(
        aesthetics = list(value = c("model_price", "exposure_premium")),
        stats = list(value = "sum"),
        formats = list(measure_labels = c(
          model_price      = "Total Model Price (Base)",
          exposure_premium = "Total Exposure Premium (Base)"
        ))
      ),
      block_name = "Portfolio KPIs (Base)"
    ),
    portfolio_drill_country = new_drilldown_chart_block(
      chart_type = "bar",
      group_by   = "country",
      metric     = "model_price",
      agg_fn     = "sum",
      block_name = "Model price by country (Base)"
    ),
    portfolio_drill_peril = new_drilldown_chart_block(
      chart_type = "bar",
      group_by   = "peril",
      metric     = "model_price",
      agg_fn     = "sum",
      block_name = "Model price by peril (Base)"
    ),

    # === ANALYSIS CROSSFILTER — single filter source for the Analysis tab ===
    # All Analysis-tab blocks (KPI, drilldowns, scatter, waterfall) read from
    # this. Drill to one policy = pick its policy_id chip here.
    analysis_xfilter = new_crossfilter_block(
      active_dims = list(.tbl = c("country", "peril", "policy_id")),
      measure     = ".tbl.model_price",
      agg_func    = "sum",
      block_name  = "Analysis filter (country / peril / policy)"
    ),

    # === POLICY VIEW — scatter (visual) + waterfall (price build-up) ===
    # Scatter is purely visual now; filter state lives in analysis_xfilter.
    policy_scatter = new_drilldown_chart_block(
      chart_type = "scatter",
      x_col      = "tiv",
      y_col      = "model_price",
      series_by  = "policy_id",
      block_name = "Policy scatter — Base"
    ),
    policy_waterfall = new_waterfall_block(
      measures = c("base_premium", "exposure_premium",
                   "experience_premium", "risk_premium", "model_price"),
      block_name = "Policy waterfall — Base"
    ),

    # === COMPARE — Challenger vs Base, portfolio grain ===
    # x = Challenger, y = Base so compare_frames computes Challenger - Base
    # (positive when Challenger increases the price — natural narrative).
    compare_portfolio = new_compare_block(
      key_cols     = c("policy_id", "location_id", "country", "peril"),
      measure_cols = c("base_premium", "exposure_premium",
                       "risk_premium", "model_price"),
      metric       = "diff",
      block_name   = "Compare Challenger vs Base (portfolio)"
    ),
    compare_portfolio_xfilter = new_crossfilter_block(
      active_dims = list(.tbl = c("country", "peril", "policy_id")),
      measure     = ".tbl.model_price",
      agg_func    = "sum",
      block_name  = "Diff filter (country / peril / policy)"
    ),
    compare_portfolio_waterfall = new_waterfall_block(
      measures = c("base_premium", "exposure_premium",
                   "risk_premium", "model_price"),
      block_name = "Diff waterfall (Challenger - Base)"
    )
  ),

  links = links(
    from = c(
      # Shared defaults seed each side's grid.
      "base_rate_src", "base_rate_src",
      # Base run: params dm = country_factor + edited base_rate + expenses +
      # cat_factor; then price (with portfolio inputs) → premium.
      "country_factor_src", "base_grid", "expenses_src", "cat_factor_src",
      "portfolio_inputs", "base_params",
      "base_price",
      # Challenger run: same shape, mirrored.
      "country_factor_src", "chal_grid", "expenses_src", "cat_factor_src",
      "portfolio_inputs", "chal_params",
      "chal_price",
      # Analysis tab: crossfilter sits between Base premium and every chart.
      "base_premium",
      "analysis_xfilter", "analysis_xfilter", "analysis_xfilter",
      "analysis_xfilter", "analysis_xfilter",
      # Engine-selection preview: base_premium → small crossfilter → waterfall.
      "base_premium", "preview_xfilter",
      # Compare: x = Challenger, y = Base → diff → crossfilter → waterfall.
      "chal_premium", "base_premium",
      "compare_portfolio", "compare_portfolio_xfilter"
    ),
    to = c(
      "base_grid", "chal_grid",
      "base_params", "base_params", "base_params", "base_params",
      "base_price",  "base_price",
      "base_premium",
      "chal_params", "chal_params", "chal_params", "chal_params",
      "chal_price",  "chal_price",
      "chal_premium",
      "analysis_xfilter",
      "portfolio_kpi", "portfolio_drill_country", "portfolio_drill_peril",
      "policy_scatter", "policy_waterfall",
      "preview_xfilter", "preview_waterfall",
      "compare_portfolio", "compare_portfolio",
      "compare_portfolio_xfilter", "compare_portfolio_waterfall"
    ),
    input = c(
      "data", "data",
      "country_factor", "base_rate", "expenses", "cat_factor",
      "inputs",  "params",
      "data",
      "country_factor", "base_rate", "expenses", "cat_factor",
      "inputs",  "params",
      "data",
      "data",
      "data", "data", "data", "data", "data",
      "data", "data",
      "x", "y",
      "data", "data"
    )
  ),

  # Tabs follow a left-to-right reveal:
  #   1. Engine selection — configure the engine + rates (no "Challenger"
  #      vocabulary yet; the audience just sees "the engine").
  #   2. Analysis — KPI + drilldowns over the whole book, plus scatter+waterfall
  #      for single-policy drill-in (scatter click filters the waterfall).
  #   3. Engine (Challenger) — introduce a second configuration to compare
  #      against. Symmetric to tab 1.
  #   4. Compare Portfolio — Challenger - Base diff at portfolio grain.
  layout = dock_layouts(
    `Engine selection` = dock_view(
      "portfolio_inputs",
      "base_grid", "base_price",
      "preview_xfilter", "preview_waterfall",
      "base_premium",
      active = TRUE
    ),
    Analysis = dock_view(
      "analysis_xfilter",
      "portfolio_kpi",
      "portfolio_drill_country", "portfolio_drill_peril",
      "policy_scatter", "policy_waterfall"
    ),
    `Engine (Challenger)` = dock_view(
      "chal_grid", "chal_price", "chal_premium"
    ),
    `Compare Portfolio` = dock_view(
      "compare_portfolio_xfilter", "compare_portfolio_waterfall"
    )
  )
)

serve(board, plugins = custom_plugins(c(manage_project())))
