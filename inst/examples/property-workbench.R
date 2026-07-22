# Property Workbench — Base vs Challenger portfolio simulations
#
# Run with:
#   source(system.file("examples/property-workbench.R", package = "blockr.insurance"))
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
  blockr.tabular_display = blockr.ui::html_table_display,
  # Dock's hidden-output detection misreports block visibility — without
  # this, blocks stay suspended and panels never render.
  blockr.lazy_eval         = FALSE,
  blockr.ai_model          = "gpt-4o-mini"
)

# ---- Package loading (dual: installed vs local source) ---------------------
# `dev_local = FALSE` (the default, and what ships) attaches the INSTALLED
# packages with library(). Set it to TRUE -- or source this file from the
# dev/property-workbench.R wrapper -- to load every blockr package from its
# LOCAL source checkout with pkgload::load_all(). One board, two loaders, no drift.
if (!exists("dev_local")) dev_local <- FALSE

blockr_pkgs <- c(
  "blockr.core",
  "blockr.dock",
  "blockr.dm",
  "blockr.dplyr",
  "blockr.viz",
  "blockr.input",     # editable base_rate parameter grids (Base / Challenger)
  "blockr.extra",
  "blockr.session",
  "blockr.ai",
  "blockr.code",      # show / export the generated R code
  "blockr.insurance"  # pricing engines + portfolio data, this example
)

for (pkg in blockr_pkgs) {
  if (dev_local) pkgload::load_all(pkg, quiet = TRUE)
  else library(pkg, character.only = TRUE)
}

# ---- Curate the block browser ----------------------------------------------
# Keep ONLY `dataset` and `glue` from blockr.core; drop its low-level / noise
# blocks (subset, merge, rbind, head, scatter, csv, filebrowser, upload) via
# unregister_blocks(), selecting by the registry `package` attribute so only
# core blocks are affected.
core_keep <- c("dataset_block", "glue_block")
core_drop <- setdiff(
  names(Filter(
    function(entry) identical(attr(entry, "package"), "blockr.core"),
    available_blocks()
  )),
  core_keep
)
unregister_blocks(core_drop)

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
  # Nested column/row layouts (matching dev/json/Actuarial_Workbench.json).
  # Each tab is a horizontal split into columns; group() stacks blocks
  # vertically within a column; panels() puts two blocks on one tabbed
  # panel. Upstream plumbing (portfolio_inputs, base_premium, the *_sum and
  # *_wf_long helpers) is left out of the layouts so it stays hidden.
  layouts = list(
    # Engine selection — two columns. Left: Scope over the Base engine.
    # Right (wide): the Base rate grid over the price build-up waterfall,
    # so a rate edit and its effect sit one above the other.
    Engine_selection = dock_layout(
      group("policy_picker", "base_price",       sizes = c(1, 2.8)),
      group("base_grid",     "policy_waterfall", sizes = c(1, 1)),
      sizes = c(1, 3.4),
      name = "Engine selection"
    ),
    # Analysis — three columns. Left (narrow): crossfilter over the KPI
    # tile. Middle: the two drilldown bars stacked. Right: scatter and
    # waterfall sharing one tabbed panel for single-policy drill-in.
    Analysis = dock_layout(
      group("analysis_xfilter",        "portfolio_kpi",         sizes = c(1.9, 1)),
      group("portfolio_drill_country", "portfolio_drill_peril", sizes = c(1, 1.1)),
      panels("policy_scatter", "policy_waterfall"),
      sizes = c(1, 2.9, 2.3),
      name = "Analysis"
    ),
    # Compare — three columns. Left: Scope over the diff crossfilter.
    # Middle: Challenger engine over its rate grid. Right: the diff
    # waterfall and Challenger premium sharing a tabbed panel.
    Compare = dock_layout(
      group("policy_picker", "compare_portfolio_xfilter", sizes = c(1, 3.3)),
      group("chal_price",    "chal_grid",                 sizes = c(1, 3.6)),
      panels("compare_portfolio_waterfall", "chal_premium"),
      sizes = c(1.5, 1.6, 2.5),
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
