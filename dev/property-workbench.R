# Property Workbench — Base vs Challenger portfolio simulations
#
# Run from an R session at the workspace root:
#   source("blockr.insurance/dev/property-workbench.R")
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
  blockr.lazy_eval         = FALSE,
  blockr.ai_model          = "gpt-4o-mini"
)

pkgload::load_all("blockr.core")
pkgload::load_all("blockr.dock")
pkgload::load_all("blockr.dm")
pkgload::load_all("blockr.dplyr")
pkgload::load_all("blockr.viz")
pkgload::load_all("blockr.input")
pkgload::load_all("blockr.extra")
pkgload::load_all("blockr.session")
pkgload::load_all("blockr.ai")
pkgload::load_all("blockr.code")
pkgload::load_all("blockr.insurance")

portfolio_dir <- blockr.insurance::default_portfolio_dir()

# Single source of truth for waterfall measures — used by Price build-up,
# Policy waterfall, the Compare block, and the Diff waterfall so every
# price-flow visualisation lines up on the same axis.
waterfall_measures <- c("base_premium", "exposure_premium",
                        "experience_premium", "risk_premium", "model_price")

board <- new_dock_board(
  blocks = c(

    # === SETUP — portfolio inputs shared by both runs ===
    portfolio_inputs = new_portfolio_inputs_block(
      dir = portfolio_dir,
      block_name = "Inputs"
    ),

    # Upstream scope picker. Filters the input dm on policies.policy_id,
    # cascading to locations + claims via FK. Default state is multi-mode
    # with empty values → passthrough (whole book). Switch to single-mode
    # via the gear popover to pick one policy.
    policy_picker = new_value_filter_block(
      state = list(columns = list(
        list(name = "policy_id", table = "policies",
             mode = "multi", values = character())
      )),
      block_name = "Scope"
    ),

    # === SHARED PARAM DEFAULTS — both runs seed their grids from these ===
    # The four sub-tables of property_params are exposed as standalone
    # datasets in blockr.insurance so we can load each with a dataset block.
    # Dataset blocks store the dataset NAME, not the data, so saved
    # workflows round-trip cleanly through JSON save/load. Static blocks
    # don't (jsonlite re-parses the embedded data as a list-of-records, not
    # a data frame, and downstream blocks then crash).
    # Cleaner long-term shape (one dm-aware CRUD block) is parked in
    # blockr.design/open/dm-crud-input/1-motivation.md.
    country_factor_src = new_dataset_block(
      dataset    = "property_param_country_factor",
      package    = "blockr.insurance",
      block_name = "country_factor"
    ),
    base_rate_src = new_dataset_block(
      dataset    = "property_param_base_rate",
      package    = "blockr.insurance",
      block_name = "base_rate"
    ),
    expenses_src = new_dataset_block(
      dataset    = "property_param_expenses",
      package    = "blockr.insurance",
      block_name = "expenses"
    ),
    cat_factor_src = new_dataset_block(
      dataset    = "property_param_cat_factor",
      package    = "blockr.insurance",
      block_name = "cat_factor"
    ),

    # === BASE RUN — engine + params + price + premium ===
    base_grid = new_grid_edit_block(
      state      = list(key_col = "country"),
      block_name = "Base rates"
    ),
    base_params = new_dm_block(
      infer_keys = FALSE,
      block_name = "Base params"
    ),
    base_price = new_price_block(
      engine     = "engine_property",
      package    = "blockr.insurance",
      block_name = "Base engine"
    ),
    base_premium = new_dm_pull_block(
      table = "premium",
      block_name = "Base premium"
    ),


    # === CHALLENGER RUN — symmetric, defaults to v2 engine ===
    chal_grid = new_grid_edit_block(
      state      = list(key_col = "country"),
      block_name = "Challenger rates"
    ),
    chal_params = new_dm_block(
      infer_keys = FALSE,
      block_name = "Challenger params"
    ),
    chal_price = new_price_block(
      engine     = "engine_property_v2",
      package    = "blockr.insurance",
      block_name = "Challenger engine"
    ),
    chal_premium = new_dm_pull_block(
      table = "premium",
      block_name = "Challenger premium"
    ),

    # === PORTFOLIO WORKSPACE — KPI + drilldowns off the Base run ===
    # The tile block is a pure renderer (it no longer aggregates), so the
    # portfolio totals are summed upstream into a one-row frame whose column
    # names are the card labels; the tile then renders them as KPI cards.
    portfolio_kpi_sum = new_summarize_block(
      summaries = list(
        list(type = "expr", name = "Total Model Price (Base)",
             expr = "sum(model_price, na.rm = TRUE)"),
        list(type = "expr", name = "Total Exposure Premium (Base)",
             expr = "sum(exposure_premium, na.rm = TRUE)")
      ),
      by = character(),
      block_name = "Portfolio totals"
    ),
    portfolio_kpi = new_tile_block(
      value = c("Total Model Price (Base)", "Total Exposure Premium (Base)"),
      format = "compact",
      block_name = "KPIs"
    ),
    portfolio_drill_country = new_chart_block(
      chart_type = "bar",
      group   = "country",
      metric     = "model_price",
      agg_fn     = "sum",
      block_name = "By country"
    ),
    portfolio_drill_peril = new_chart_block(
      chart_type = "bar",
      group   = "peril",
      metric     = "model_price",
      agg_fn     = "sum",
      block_name = "By peril"
    ),

    # === ANALYSIS CROSSFILTER — single filter source for the Analysis tab ===
    # All Analysis-tab blocks (KPI, drilldowns, scatter, waterfall) read from
    # this. Drill to one policy = pick its policy_id chip here.
    analysis_xfilter = new_crossfilter_block(
      active_dims = list(.tbl = c("country", "peril", "policy_id")),
      measure     = ".tbl.model_price",
      agg_func    = "sum",
      block_name  = "Filter"
    ),

    # === POLICY VIEW — scatter (visual) + waterfall (price build-up) ===
    # Scatter is purely visual now; filter state lives in analysis_xfilter.
    policy_scatter = new_chart_block(
      chart_type = "scatter",
      x      = "tiv",
      y      = "model_price",
      series  = "policy_id",
      block_name = "Policy scatter"
    ),
    # Single waterfall reused on Engine selection (configuration feedback)
    # and Analysis (chip-filtered drill). Wired through `analysis_xfilter`
    # so it responds to country/peril/policy_id chips when they're set;
    # otherwise it just reflects the upstream Scope.
    # Waterfall is now new_chart_block(chart_type="waterfall") over LONG data,
    # so the wide price-build-up measures are pivoted to (step, value) first.
    policy_wf_long = new_pivot_longer_block(
      cols = as.list(waterfall_measures),
      names_to = "step", values_to = "value",
      values_drop_na = FALSE, names_prefix = "",
      block_name = "Price build-up (long form)"
    ),
    policy_waterfall = new_chart_block(
      chart_type = "waterfall",
      group = "step", metric = "value", agg_fn = "sum",
      waterfall_totals = "model_price",
      block_name = "Price build-up"
    ),

    # === COMPARE — Challenger vs Base, portfolio grain ===
    # x = Challenger, y = Base so compare_frames computes Challenger - Base
    # (positive when Challenger increases the price — natural narrative).
    compare_portfolio = new_compare_block(
      key_cols     = c("policy_id", "location_id", "country", "peril"),
      measure_cols = waterfall_measures,
      metric       = "diff",
      block_name   = "Compare"
    ),
    compare_portfolio_xfilter = new_crossfilter_block(
      active_dims = list(.tbl = c("country", "peril", "policy_id")),
      measure     = ".tbl.model_price",
      agg_func    = "sum",
      block_name  = "Diff filter"
    ),
    compare_wf_long = new_pivot_longer_block(
      cols = list("base_premium", "exposure_premium",
                  "risk_premium", "model_price"),
      names_to = "step", values_to = "value",
      values_drop_na = FALSE, names_prefix = "",
      block_name = "Diff waterfall (long form)"
    ),
    compare_portfolio_waterfall = new_chart_block(
      chart_type = "waterfall",
      group = "step", metric = "value", agg_fn = "sum",
      waterfall_totals = "model_price",
      block_name = "Diff waterfall"
    )
  ),

  links = links(
    from = c(
      # Upstream scope picker — filters portfolio_inputs on policies.policy_id,
      # cascades to locations + claims via FK. Both engines read the scoped dm.
      "portfolio_inputs",
      # Shared defaults seed each side's grid.
      "base_rate_src", "base_rate_src",
      # Base run: params dm = country_factor + edited base_rate + expenses +
      # cat_factor; then price (with scoped inputs) → premium.
      "country_factor_src", "base_grid", "expenses_src", "cat_factor_src",
      "policy_picker", "base_params",
      "base_price",
      # Challenger run: same shape, mirrored.
      "country_factor_src", "chal_grid", "expenses_src", "cat_factor_src",
      "policy_picker", "chal_params",
      "chal_price",
      # Analysis tab: crossfilter sits between Base premium and every chart.
      "base_premium",
      "analysis_xfilter", "portfolio_kpi_sum",
      "analysis_xfilter", "analysis_xfilter",
      "analysis_xfilter", "analysis_xfilter",
      # Compare: x = Challenger, y = Base → diff → crossfilter → waterfall.
      "chal_premium", "base_premium",
      "compare_portfolio", "compare_portfolio_xfilter",
      # waterfall plumbing: wide measures -> long (step, value)
      "policy_wf_long", "compare_wf_long"
    ),
    to = c(
      "policy_picker",
      "base_grid", "chal_grid",
      "base_params", "base_params", "base_params", "base_params",
      "base_price",  "base_price",
      "base_premium",
      "chal_params", "chal_params", "chal_params", "chal_params",
      "chal_price",  "chal_price",
      "chal_premium",
      "analysis_xfilter",
      "portfolio_kpi_sum", "portfolio_kpi",
      "portfolio_drill_country", "portfolio_drill_peril",
      "policy_scatter", "policy_wf_long",
      "compare_portfolio", "compare_portfolio",
      "compare_portfolio_xfilter", "compare_wf_long",
      "policy_waterfall", "compare_portfolio_waterfall"
    ),
    input = c(
      "data",
      "data", "data",
      "country_factor", "base_rate", "expenses", "cat_factor",
      "inputs",  "params",
      "data",
      "country_factor", "base_rate", "expenses", "cat_factor",
      "inputs",  "params",
      "data",
      "data",
      "data", "data",
      "data", "data", "data", "data",
      "x", "y",
      "data", "data",
      "data", "data"
    )
  ),

  # Tabs follow a left-to-right reveal:
  #   1. Engine selection — configure the engine + rates (no "Challenger"
  #      vocabulary yet; the audience just sees "the engine").
  #   2. Analysis — KPI + drilldowns over the whole book, plus scatter+waterfall
  #      for single-policy drill-in (scatter click filters the waterfall).
  #   3. Compare — introduce a second engine config (Challenger) AND see the
  #      Challenger - Base diff in one place. Scope (policy_picker) is
  #      reachable here too so the user can switch between portfolio and
  #      single-policy without leaving the tab. Works at any scope since
  #      policy_picker filters upstream.
  layouts = list(
    Engine_selection = dock_layout(
      "portfolio_inputs",
      "policy_picker",
      "base_grid", "base_price",
      "policy_waterfall",
      "base_premium",
      name = "Engine selection"
    ),
    Analysis = dock_layout(
      "analysis_xfilter",
      "portfolio_kpi_sum", "portfolio_kpi",
      "portfolio_drill_country", "portfolio_drill_peril",
      "policy_scatter", "policy_waterfall",
      name = "Analysis"
    ),
    Compare = dock_layout(
      "policy_picker",
      "chal_grid", "chal_price", "chal_premium",
      "compare_portfolio_xfilter", "compare_portfolio_waterfall",
      name = "Compare"
    )
  ),
  active = "Engine_selection"
)

serve(
  board,
  plugins = custom_plugins(c(
    ai_ctrl_block(),
    manage_project(),
    generate_flat_code()
  ))
)
