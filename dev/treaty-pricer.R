# Treaty pricer — tower → fit → simulate → structure → price.
#
# A simulation-based pricer for a layered excess-of-loss reinsurance treaty.
# ONE treaty tower drives everything: the client hands over the layer structure
# (limit / retention / AAD / AAL / subject premium / reinsurance rate) and the
# actuary adds an expected loss per layer. `retention` + `expected_loss` are
# matched to a piecewise-Pareto severity curve (via the Pareto package); the
# same tower's structure is then applied to the simulated losses to get a price.
#
# Built from bare, transparent blocks: everything expressible as a dplyr verb
# is a blockr.dplyr block, and only a few small function blocks remain — the
# Pareto calls, the fit table, and the per-claim layering (the one step that
# isn't a dplyr verb). Aggregation and pricing are summarize/mutate blocks you
# can read off the UI; every VIEW is a blockr.viz tile / chart / table.
#
# The actuary's inputs, all on the one editable `treaty_tower` grid + one knob:
#   - expected loss per layer → the `expected_loss` column (judgment). With the
#     layer `retention`s as attachment points, these calibrate the severity fit.
#   - the frequency anchor    → the `frequency` field on the `fit` function
#     block (FQ at the lowest attachment point). The method SOLVES the higher-
#     layer frequencies, so a single anchor is supplied, not a column — per-
#     layer frequencies over-determine the match and the αs go degenerate.
#
# Screens (left to right, designed so editing a number visibly moves three
# things at once — that's the demo):
#   - Pricer (cockpit): the treaty tower + programme KPI tile + a per-layer
#     QUOTE chart (loss vs technical premium, grouped bars) + a premium BUILD-UP
#     waterfall (pure loss → +risk → +expense → +profit → technical). Edit a
#     layer and the KPIs, the quote and the waterfall recompute live.
#   - Loss model: the fit function block grouped with its fitted αs (chart +
#     table) below it; simulate + the exceedance / return-period curve; and the
#     specified-vs-simulated reconciliation (grouped bar) — the severity-fit and
#     simulation detail, away from the price view.
#   - Layer detail: one drill path — click a layer in the bar to break out its
#     annual-loss-distribution histogram + a scorecard (expected / worst-year /
#     technical).
#   - Compare: a CHALLENGER structure tower run in parallel on the same
#     simulated losses, diffed against the base (challenger − base) as a table
#     and a per-layer bar. Restructure the challenger → watch the diff move.
#   - Workflow: the live block graph (blockr.dag), blocks grouped into coloured
#     STACKS by role so the data flow reads at a glance.
#
# Run from an R session at the workspace root:
#   source("blockr.insurance/dev/treaty-pricer.R")

pkgload::load_all("blockr.core")
pkgload::load_all("blockr.ui")
pkgload::load_all("blockr.dock")
pkgload::load_all("blockr.dplyr")
pkgload::load_all("blockr.input")
pkgload::load_all("blockr.extra")
pkgload::load_all("blockr.io")
pkgload::load_all("blockr.viz")
pkgload::load_all("blockr.dag")
pkgload::load_all("blockr.session")
pkgload::load_all("blockr.code")
pkgload::load_all("blockr.ai")
pkgload::load_all("blockr.assistant")
pkgload::load_all("blockr.insurance")

stopifnot(requireNamespace("Pareto", quietly = TRUE))

options(
  blockr.dock_is_locked = FALSE,
  blockr.lazy_eval      = FALSE,
  blockr.html_table_preview = TRUE,   # nicer HTML data previews in blocks
  blockr.ai_model       = "gpt-5.1",  # prod model; needs OPENAI_API_KEY set
  # The assistant's chat client (slide 8 "extend"); prod model.
  blockr.chat_function = function(system_prompt = NULL, params = NULL) {
    ellmer::chat_openai(
      model = "gpt-5.1", system_prompt = system_prompt, echo = "none"
    )
  },
  blockr.assistant_immediate_commit = TRUE
)

# ONE treaty tower, loaded from a CSV bundled in the package (showcasing the
# universal file reader — swap to upload via the read block's cogwheel). The
# client hands over the layer structure (limit / retention / aggregates); the
# actuary fills in an `expected_loss` per layer. The SAME table drives both
# halves of the method: `retention` + `expected_loss` calibrate the severity fit
# (layers are bounded by consecutive retentions, so no separate reference
# tower), while `limit / retention / agg_*` define the deal applied to the
# simulated losses. The price is the TECHNICAL PREMIUM the build-up produces —
# no quoted premium or loss ratio; this is a pricing tool, the premium is the
# output. Columns are ordered deductible → cap at each level: per-occurrence
# (retention, limit) then annual-aggregate (agg_retention, agg_limit), with the
# actuary's expected_loss last.
treaty_csv <- system.file("extdata", "treaty_tower.csv",
                          package = "blockr.insurance")

# The challenger reads the SAME file, so base and challenger start IDENTICAL and
# the Compare diff is zero out of the box. Change a structure value on the
# challenger and the diff appears — the cleanest way to show a restructuring.

# The per-claim layering, shared verbatim by the base and challenger runs:
# per-occurrence layering → annual total per layer → annual aggregate (AAD/AAL).
# Also carries `layer_loss` (the annual layer loss BEFORE the aggregate) and
# `specified` (the actuary's input loss pick, when present) so a reconciliation
# view can check specified vs simulated without a join. The challenger tower has
# no expected_loss column, so `specified` falls back to NA there (unused).
layering_fn <- "function(x, y) {
  do.call(rbind, lapply(seq_len(nrow(y)), function(i) {
    loss   <- pmin(pmax(x$severity - y$retention[i], 0), y$limit[i])
    annual <- tapply(loss, x$year, sum)
    data.frame(
      layer_id   = y$layer_id[i],
      year       = as.integer(names(annual)),
      layer_loss = as.numeric(annual),
      ceded      = as.numeric(pmin(pmax(annual - y$agg_retention[i], 0), y$agg_limit[i])),
      specified  = if (is.null(y$expected_loss)) NA_real_ else y$expected_loss[i]
    )
  }))
}"

# Premium build-up applied per layer: pure loss plus named loadings. The
# technical premium IS the price — pure_loss × (1 + 0.15 + 0.05 + 0.10) = × 1.30.
# pure_loss and technical_premium are absolute (waterfall totals); the three
# loadings are deltas.
buildup_mutations <- list(
  list(name = "pure_loss",         expr = "expected_loss"),
  list(name = "risk_load",         expr = "expected_loss * 0.15"),
  list(name = "expense_load",      expr = "expected_loss * 0.05"),
  list(name = "profit_load",       expr = "expected_loss * 0.10"),
  list(name = "technical_premium", expr = "expected_loss * 1.30")
)

board_blocks <- c(

    # === TREATY TOWER — the single input (client structure + actuary loss) ===
    treaty_read = new_read_block(
      path = treaty_csv, source = "path",
      block_name = "Upload tower (CSV)"),
    treaty_tower = blockr.input::new_grid_edit_block(
      state = list(key_col = "layer_id"),
      block_name = "Treaty tower"),

    # === FIT (Pareto) — emits the fitted model; owns the frequency anchor.
    # `frequency` renders as a numericInput on the block — the second actuary
    # input. The method solves the higher-layer frequencies from this anchor.
    fit = new_function_block(
      fn = "function(data, frequency = 10) {
  Pareto::PiecewisePareto_Match_Layer_Losses(
    Attachment_Points     = data$retention,
    Expected_Layer_Losses = data$expected_loss,
    FQ_at_lowest_AttPt    = frequency
  )
}",
      block_name = "Fit"
    ),
    fit_table = new_function_block(
      fn = "function(data) {
  data.frame(
    threshold = round(data$t, 2),
    alpha     = round(data$alpha, 3),
    freq      = data$FQ
  )
}",
      block_name = "Fit αs"
    ),
    fit_chart = new_chart_block(
      chart_type = "bar", group = "threshold", metric = "alpha",
      agg_fn = "mean", sort_by = "threshold", sort_dir = "asc",
      orientation = "vertical",
      block_name = "Pareto αs"
    ),
    fit_table_view = new_table_block(
      rowname = "threshold", values = c("alpha", "freq"), digits = 3L,
      block_name = "α table"
    ),

    # === SIMULATE (Pareto) — one row per simulated claim ===
    simulate = new_function_block(
      fn = "function(data, nyears = 5000, seed = 1) {
  set.seed(seed)
  losses <- Pareto::Simulate_Losses(data, nyears = nyears)
  data.frame(
    year     = row(losses)[!is.na(losses)],
    severity = losses[!is.na(losses)]
  )
}",
      block_name = "Simulate"
    ),

    # === APPLY STRUCTURE — per-claim layering (the only non-dplyr step) ===
    # y is the treaty tower itself (limit/retention/agg); x is the simulated
    # claims. Same tower that calibrated the fit now prices the deal.
    apply_structure = blockr.extra::new_function_xy_block(
      fn = layering_fn,
      block_name = "Apply structure"
    ),

    # === PRICE (dplyr) ===
    layer_stats = new_summarize_block(
      summaries = list(
        list(type = "simple", name = "expected_loss",
             func = "mean", col = "ceded")
      ),
      by = list("layer_id"),
      block_name = "Ceded / layer"
    ),
    priced = new_mutate_block(
      mutations = buildup_mutations,
      by = list(),
      block_name = "Price"
    ),

    # === PROGRAMME KPIs (blockr.viz tiles) ===
    prog_kpi_sum = new_summarize_block(
      summaries = list(
        list(type = "expr", name = "Expected ceded loss",
             expr = "sum(expected_loss, na.rm = TRUE)"),
        list(type = "expr", name = "Technical premium",
             expr = "sum(technical_premium, na.rm = TRUE)")
      ),
      by = list(),
      block_name = "Totals"
    ),
    prog_kpi = new_tile_block(
      value = c("Expected ceded loss", "Technical premium"),
      format = "number", unit = "USD m",
      block_name = "KPIs"
    ),

    # === QUOTE CHART — losses vs technical premium per layer (grouped bar) ===
    # Pivot the two priced measures to long, then a grouped bar puts expected
    # ceded loss and technical premium side-by-side for each layer.
    quote_long = new_pivot_longer_block(
      cols = list("expected_loss", "technical_premium"),
      names_to = "measure", values_to = "value",
      values_drop_na = FALSE, names_prefix = "",
      block_name = "Quote (long)"
    ),
    quote_chart = new_chart_block(
      chart_type = "bar", group = "layer_id", metric = "value",
      color = "measure", agg_fn = "sum", bar_mode = "grouped",
      sort_by = "layer_id", sort_dir = "asc", orientation = "vertical",
      block_name = "Quote"
    ),

    # === PREMIUM BUILD-UP WATERFALL ===
    # Sum the per-layer build-up to a ONE-ROW programme build-up first, then
    # pivot to long (step, value) and walk it as a waterfall. Summing upstream
    # (rather than leaning on the chart's cross-layer aggregation) gives an
    # unambiguous programme bridge: pure loss → +loadings → technical premium.
    prog_buildup = new_summarize_block(
      summaries = list(
        list(type = "simple", name = "pure_loss",
             func = "sum", col = "pure_loss"),
        list(type = "simple", name = "risk_load",
             func = "sum", col = "risk_load"),
        list(type = "simple", name = "expense_load",
             func = "sum", col = "expense_load"),
        list(type = "simple", name = "profit_load",
             func = "sum", col = "profit_load"),
        list(type = "simple", name = "technical_premium",
             func = "sum", col = "technical_premium")
      ),
      by = list(),
      block_name = "Build-up"
    ),
    price_buildup_long = new_pivot_longer_block(
      cols = list("pure_loss", "risk_load", "expense_load",
                  "profit_load", "technical_premium"),
      names_to = "step", values_to = "value",
      values_drop_na = FALSE, names_prefix = "",
      block_name = "Build-up (long)"
    ),
    price_waterfall = new_chart_block(
      chart_type = "waterfall", group = "step", metric = "value",
      agg_fn = "sum", waterfall_totals = "technical_premium",
      block_name = "Premium build-up"
    ),

    # === PER-LAYER BAR — drill source #1 (year detail) ===
    layer_bar = new_chart_block(
      chart_type = "bar", group = "layer_id", metric = "ceded", agg_fn = "mean",
      drill = "layer_id", sort_by = "layer_id", sort_dir = "asc",
      block_name = "By layer"
    ),

    # === LAYER DRILL TARGETS (of the bar) ===
    # Histogram of the selected layer's ANNUAL ceded loss: floor(ceded) bins,
    # one bar = how many simulated years landed in that band. Replaces the old
    # ceded-by-sim-year line (sim years are exchangeable, so that ordering was
    # meaningless). The spike at 0 = no-loss years; the long right tail = the
    # bad years that the worst-year card reports.
    layer_hist_bin = new_mutate_block(
      mutations = list(list(name = "ceded_band", expr = "floor(ceded)")),
      by = list(),
      block_name = "Bin ceded"
    ),
    layer_hist = new_chart_block(
      chart_type = "bar", group = "ceded_band", metric = "ceded",
      agg_fn = "count", sort_by = "ceded_band", sort_dir = "asc",
      orientation = "vertical",
      block_name = "Loss distribution"
    ),
    layer_detail_sum = new_summarize_block(
      summaries = list(
        list(type = "expr", name = "Expected ceded loss",
             expr = "mean(ceded, na.rm = TRUE)"),
        list(type = "expr", name = "Worst-year ceded",
             expr = "max(ceded, na.rm = TRUE)"),
        list(type = "expr", name = "Technical premium",
             expr = "mean(ceded, na.rm = TRUE) * 1.30")
      ),
      by = list(),
      block_name = "Layer totals"
    ),
    layer_detail_tile = new_tile_block(
      value = c("Expected ceded loss", "Worst-year ceded", "Technical premium"),
      format = "number", unit = "USD m",
      block_name = "Layer KPIs"
    ),

    # === PROGRAMME EXCEEDANCE CURVE ===
    per_year = new_summarize_block(
      summaries = list(
        list(type = "simple", name = "total_ceded",
             func = "sum", col = "ceded")
      ),
      by = list("year"),
      block_name = "Per year"
    ),
    exceed_rank = new_mutate_block(
      mutations = list(
        list(name = "rank",          expr = "rank(-total_ceded)"),
        list(name = "return_period", expr = "dplyr::n() / rank")
      ),
      by = list(),
      block_name = "Return period"
    ),
    exceedance = new_chart_block(
      chart_type = "line", x = "return_period", y = "total_ceded",
      metric = ".count",
      block_name = "Exceedance"
    ),

    # === RECONCILIATION — specified vs simulated layer loss (option C) ===
    # Validates the fit: `specified` (the actuary's pick) vs `simulated` (the
    # mean BEFORE-aggregate annual layer loss). They should match up to sampling
    # noise — closer on the thicker low layers, wobblier on the thin top layer.
    recon = new_summarize_block(
      summaries = list(
        list(type = "expr", name = "specified",
             expr = "dplyr::first(specified)"),
        list(type = "simple", name = "simulated",
             func = "mean", col = "layer_loss")
      ),
      by = list("layer_id"),
      block_name = "Recon"
    ),
    # Long form so the two series (specified, simulated) can be coloured apart,
    # then a GROUPED bar (bar_mode = "grouped") puts them side-by-side per layer.
    recon_long = new_pivot_longer_block(
      cols = list("specified", "simulated"),
      names_to = "source", values_to = "value",
      values_drop_na = FALSE, names_prefix = "",
      block_name = "Recon (long)"
    ),
    recon_chart = new_chart_block(
      chart_type = "bar", group = "layer_id", metric = "value",
      color = "source", agg_fn = "sum", bar_mode = "grouped",
      sort_by = "layer_id", sort_dir = "asc", orientation = "vertical",
      block_name = "Specified vs simulated"
    ),

    # === CHALLENGER — parallel structure on the SAME simulated losses ===
    # Reads the same CSV as the base, so it starts identical (diff = 0).
    challenger_read = new_read_block(
      path = treaty_csv, source = "path",
      block_name = "Upload challenger (CSV)"),
    challenger_tower = blockr.input::new_grid_edit_block(
      state = list(key_col = "layer_id"),
      block_name = "Challenger"),
    chal_apply_structure = blockr.extra::new_function_xy_block(
      fn = layering_fn,
      block_name = "Apply challenger"
    ),
    chal_layer_stats = new_summarize_block(
      summaries = list(
        list(type = "simple", name = "expected_loss",
             func = "mean", col = "ceded")
      ),
      by = list("layer_id"),
      block_name = "Challenger ceded"
    ),
    chal_priced = new_mutate_block(
      mutations = buildup_mutations,
      by = list(),
      block_name = "Challenger price"
    ),
    # x = challenger, y = base → diff = challenger − base (negative = cheaper).
    compare = new_compare_block(
      key_cols     = "layer_id",
      measure_cols = c("expected_loss", "technical_premium"),
      metric       = "diff",
      block_name   = "Challenger − base (per layer)"
    ),
    compare_table = new_table_block(
      rowname = "layer_id",
      values = c("expected_loss", "technical_premium"),
      cell_color = drilldown_table_color("diverging",
        columns = c("expected_loss", "technical_premium")),
      digits = 2L,
      block_name = "Diff table"
    ),
    compare_bar = new_chart_block(
      chart_type = "bar", group = "layer_id", metric = "technical_premium",
      agg_fn = "sum", sort_by = "layer_id", sort_dir = "asc",
      block_name = "Premium Δ"
    )
)

# Hide the redundant data-frame OUTPUT preview on the display blocks (chart /
# table / tile / grid): for these the block's own render IS the input section,
# and the output accordion is just a duplicate data preview of the same thing.
for (.id in c(
  "treaty_read", "challenger_read",
  "treaty_tower", "challenger_tower", "prog_kpi", "layer_detail_tile",
  "quote_chart", "price_waterfall", "fit_chart", "recon_chart", "exceedance",
  "layer_bar", "layer_hist", "compare_bar", "fit_table_view", "compare_table",
  "fit", "simulate"
)) {
  attr(board_blocks[[.id]], "visible") <- "inputs"
}

board <- new_dock_board(
  blocks = board_blocks,

  links = links(
    from = c(
      # tower -> fit -> simulate (the tower also feeds apply_structure below)
      "treaty_read", "treaty_tower", "fit", "fit_table", "fit_table", "fit",
      # apply layering: x = sim claims, y = the treaty tower itself
      "simulate", "treaty_tower",
      # price chain
      "apply_structure", "layer_stats",
      # programme KPI tile
      "priced", "prog_kpi_sum",
      # quote chart (priced -> long -> grouped bar)
      "priced", "quote_long",
      # build-up waterfall (programme sum -> long -> waterfall)
      "priced", "prog_buildup", "price_buildup_long",
      # per-layer bar = the single drill source -> histogram + scorecard
      "apply_structure", "layer_bar", "layer_hist_bin",
      "layer_bar", "layer_detail_sum",
      # exceedance curve
      "apply_structure", "per_year", "exceed_rank",
      # reconciliation: apply_structure -> recon -> recon_long -> recon_chart
      "apply_structure", "recon", "recon_long",
      # challenger run (reuses the base fit's simulated losses)
      "challenger_read", "simulate", "challenger_tower",
      "chal_apply_structure", "chal_layer_stats",
      # compare: x = challenger, y = base
      "chal_priced", "priced",
      "compare", "compare"
    ),
    to = c(
      "treaty_tower", "fit", "fit_table", "fit_chart", "fit_table_view", "simulate",
      "apply_structure", "apply_structure",
      "layer_stats", "priced",
      "prog_kpi_sum", "prog_kpi",
      "quote_long", "quote_chart",
      "prog_buildup", "price_buildup_long", "price_waterfall",
      "layer_bar", "layer_hist_bin", "layer_hist",
      "layer_detail_sum", "layer_detail_tile",
      "per_year", "exceed_rank", "exceedance",
      "recon", "recon_long", "recon_chart",
      "challenger_tower", "chal_apply_structure", "chal_apply_structure",
      "chal_layer_stats", "chal_priced",
      "compare", "compare",
      "compare_table", "compare_bar"
    ),
    input = c(
      "data", "data", "data", "data", "data", "data",
      "x", "y",
      "data", "data",
      "data", "data",
      "data", "data",
      "data", "data", "data",
      "data", "data", "data",
      "data", "data",
      "data", "data", "data",
      "data", "data", "data",
      "data", "x", "y",
      "data", "data",
      "x", "y",
      "data", "data"
    )
  ),

  # Blocks grouped into coloured STACKS by role — these drive the Workflow
  # graph so the data flow reads at a glance (one block belongs to one stack).
  stacks = list(
    tower = new_dock_stack(
      c("treaty_read", "treaty_tower"),
      name = "Treaty tower", color = "#2162B7"),
    fit = new_dock_stack(
      c("fit", "fit_table", "fit_chart", "fit_table_view"),
      name = "Fit", color = "#1FA06E"),
    simulate = new_dock_stack(
      c("simulate", "per_year", "exceed_rank", "exceedance"),
      name = "Simulate", color = "#E8843C"),
    structure = new_dock_stack(
      c("apply_structure", "recon", "recon_long", "recon_chart"),
      name = "Apply structure", color = "#7A5BB0"),
    price = new_dock_stack(
      c("layer_stats", "priced", "prog_kpi_sum", "prog_kpi",
        "quote_long", "quote_chart", "prog_buildup", "price_buildup_long",
        "price_waterfall"),
      name = "Price", color = "#C9A227"),
    drill = new_dock_stack(
      c("layer_bar", "layer_hist_bin", "layer_hist",
        "layer_detail_sum", "layer_detail_tile"),
      name = "Layer detail", color = "#5B6470"),
    challenger = new_dock_stack(
      c("challenger_read", "challenger_tower", "chal_apply_structure",
        "chal_layer_stats", "chal_priced", "compare", "compare_table",
        "compare_bar"),
      name = "Challenger", color = "#D14C4C")
  ),

  extensions = list(
    dag       = new_dag_extension(),
    assistant = new_assistant_extension()
  ),

  # Five screens. The cockpit is the star: edit an expected loss or a structure
  # rate and the fit, the tiles, the quote and the build-up waterfall all move.
  # Each screen is a GRID (rows of side-by-side panels) rather than one cramped
  # row — top-level `orientation = "vertical"` stacks the rows; each nested
  # list splits horizontally.
  layouts = list(
    # Cockpit: the one treaty tower on top, programme KPI tile in the middle,
    # the price visuals below. Edit a layer's expected loss / retention / limit
    # and the KPIs, the quote and the build-up waterfall all move. (The fitted
    # αs live on the Loss model tab — that's fit detail, not a pricing view.)
    # Top row, three columns: upload the tower CSV (narrow), tweak it in the grid
    # (wide), programme KPIs (narrow). The two price charts fill the row below.
    Pricer = dock_layout(
      group("treaty_read", "treaty_tower", "prog_kpi", sizes = c(1, 2.5, 1)),
      list("quote_chart", "price_waterfall"),
      orientation = "vertical", sizes = c(1, 1),
      name = "Pricer"),
    # Loss model, a 2x2 grid: the Pareto fit (sharing one tabbed panel with its
    # αs chart + table, fit open) and the simulate block on top; the specified-
    # vs-simulated reconciliation and the exceedance curve below.
    Loss_model = dock_layout(
      list(
        # Tabs render with the LAST one active, so put `fit` last to open on it.
        panels("fit_chart", "fit_table_view", "fit", active = "fit"),
        "simulate"
      ),
      list("recon_chart", "exceedance"),
      orientation = "vertical", sizes = c(1, 1),
      name = "Loss model"),
    # One drill path only: click a layer in the bar -> its loss-distribution
    # histogram + scorecard.
    Layer_detail = dock_layout(
      "layer_bar", "layer_hist", "layer_detail_tile",
      sizes = c(1.4, 1.4, 1),
      name = "Layer detail"),
    # Base treaty tower and challenger side by side, with the diff below.
    Compare = dock_layout(
      list("treaty_tower", "challenger_tower"),
      list("compare_table", "compare_bar"),
      orientation = "vertical", sizes = c(1, 1.2),
      name = "Compare"),
    # Workflow / extend: the live block graph + the assistant side by side, so
    # you can extend the running board by prompting (slide 8).
    Workflow = dock_layout(
      "ext_panel-assistant_extension", "ext_panel-dag_extension",
      sizes = c(1, 1.6),
      name = "Workflow")
  ),
  active = "Pricer"
)

serve(
  board,
  plugins = custom_plugins(c(
    ai_ctrl_block(),      # per-block AI support (configure a block by prompt)
    manage_project(),
    generate_flat_code()
  ))
)
