# Treaty pricer — calibration → fit → simulate → structure → price.
#
# A simulation-based pricer for a layered excess-of-loss reinsurance treaty,
# separating the LOSS MODEL (a tower of reference layers → piecewise-Pareto
# severity, via the Pareto package) from the STRUCTURE (per-layer limit /
# retention / AAD / AAL / subject premium / reinsurance rate).
#
# Built from bare, transparent blocks: everything expressible as a dplyr verb
# is a blockr.dplyr block, and only four small function blocks remain — the two
# Pareto calls, the fit table, and the per-claim layering (the one step that
# isn't a dplyr verb). Aggregation and pricing are summarize/mutate blocks you
# can read off the UI; every VIEW is a blockr.viz tile / chart / table.
#
# Views (blockr.viz):
#   - Programme KPI tiles (expected ceded loss, technical premium, RI premium)
#     + a blended-loss-ratio percent tile, off a one-row programme summarize.
#   - A per-layer quote TABLE with in-cell data bars on the money columns.
#   - A per-layer expected-ceded BAR that is ALSO a drill source: click a layer
#     to break out its year-by-year ceded trajectory + a selected-layer
#     scorecard (the chart emits the raw simulated frame filtered to that layer).
#   - A clickable per-layer KPI TILE matrix (drill=TRUE on layer_id): click a
#     layer card to break out that layer's full quote row as a detail table.
#   - The programme exceedance / return-period curve, and the fitted Pareto
#     alphas as a bar + table.
#
# Pipeline:
#   calibration tower ─► fit ─┬─► fit_table ─┬─► fit_chart (αs by threshold)
#                             │              └─► fit_table_view
#                             └─► simulate ─► apply structure ─┬─► layer_stats ─► priced ─┬─► prog_kpi_sum ─► KPI tiles
#                                                              │                          ├─► quote_table (data bars)
#                                                              │                          └─► layer_kpi_tile ─► layer_quote_detail
#                                                              ├─► per_year ─► exceed_rank ─► exceedance
#                                                              └─► layer_bar (drill) ─┬─► layer_detail_line (ceded by year)
#                                                                                     └─► layer_detail_sum ─► layer_detail_tile
#
# Run from an R session at the workspace root:
#   source("blockr.insurance/dev/treaty-pricer.R")

pkgload::load_all("blockr.core")
pkgload::load_all("blockr.dock")
pkgload::load_all("blockr.dplyr")
pkgload::load_all("blockr.input")
pkgload::load_all("blockr.extra")
pkgload::load_all("blockr.viz")
pkgload::load_all("blockr.insurance")

stopifnot(requireNamespace("Pareto", quietly = TRUE))

options(
  blockr.dock_is_locked = FALSE,
  blockr.lazy_eval      = FALSE
)

calibration_seed <- tibble::tibble(
  layer_id      = 1:5,
  limit         = c(1, 3, 5, 5, 10),
  retention     = c(1, 2, 5, 10, 15),
  expected_loss = c(6, 3, 2, 1, 0.5)
)

structure_seed <- tibble::tibble(
  layer_id        = 1:4,
  limit           = c(1, 3, 5, 15),
  retention       = c(1, 2, 5, 10),
  agg_retention   = c(5, 0, 0, 0),
  agg_limit       = c(30, 30, 50, 50),
  subject_premium = c(100, 100, 100, 100),
  ri_rate         = c(0.05, 0.09, 0.07, 0.05)
)

board <- new_dock_board(
  blocks = c(

    # === TOWERS ===
    calibration_seed = new_static_block(calibration_seed,
      block_name = "Loss model seed"),
    calibration_tower = blockr.input::new_grid_block(
      state = list(key_col = "layer_id"),
      block_name = "Loss model (expected losses)"),
    structure_seed = new_static_block(structure_seed,
      block_name = "Structure seed"),
    structure_tower = blockr.input::new_grid_block(
      state = list(key_col = "layer_id"),
      block_name = "Treaty structure"),

    # === FIT (Pareto) — emits the fitted model; owns the frequency ===
    fit = new_function_block(
      fn = function(data, frequency = 10) {
        Pareto::PiecewisePareto_Match_Layer_Losses(
          data$retention, data$expected_loss, FQ_at_lowest_AttPt = frequency
        )
      },
      block_name = "Fit (frequency owner)"
    ),
    fit_table = new_function_block(
      fn = function(data) {
        data.frame(threshold = data$t, alpha = round(data$alpha, 3),
                   freq = data$FQ)
      },
      block_name = "Fit (piecewise Pareto αs)"
    ),
    # Fitted tail-index profile: one bar per Pareto threshold, height = alpha.
    fit_chart = new_chart_block(
      chart_type = "bar", group = "threshold", metric = "alpha",
      agg_fn = "mean", sort_by = "threshold", sort_dir = "asc",
      orientation = "vertical",
      block_name = "Fitted Pareto αs (by threshold)"
    ),
    fit_table_view = new_table_block(
      rowname = "threshold", values = c("alpha", "freq"), digits = 3L,
      block_name = "Fitted Pareto αs (table)"
    ),

    # === SIMULATE (Pareto) — one row per simulated claim ===
    simulate = new_function_block(
      fn = function(data, nyears = 5000, seed = 1) {
        set.seed(seed)
        m <- Pareto::Simulate_Losses(data, nyears = nyears)
        data.frame(year = row(m)[!is.na(m)], severity = m[!is.na(m)])
      },
      block_name = "Simulated claims"
    ),

    # === APPLY STRUCTURE — the only non-dplyr step ===
    # Per claim: per-occurrence layering; summed to an annual total per layer;
    # then the annual aggregate (AAD/AAL). Carries the reinsurance premium.
    apply_structure = blockr.extra::new_function_xy_block(
      fn = "function(x, y) {
        do.call(rbind, lapply(seq_len(nrow(y)), function(i) {
          loss   <- pmin(pmax(x$severity - y$retention[i], 0), y$limit[i])
          annual <- tapply(loss, x$year, sum)
          data.frame(
            layer_id   = y$layer_id[i],
            year       = as.integer(names(annual)),
            ceded      = as.numeric(pmin(pmax(annual - y$agg_retention[i], 0), y$agg_limit[i])),
            reins_prem = y$ri_rate[i] * y$subject_premium[i]
          )
        }))
      }",
      block_name = "Apply structure (per-occ + AAD/AAL)"
    ),

    # === PRICE (dplyr) ===
    # Expected ceded loss per layer, then the price: technical premium = pure
    # loss × (1 + surcharge); edit the 1.3 to change the surcharge. Loss ratio
    # against the quoted reinsurance premium shown alongside.
    layer_stats = new_summarize_block(
      summaries = list(
        list(type = "simple", name = "expected_loss",
             func = "mean", col = "ceded")
      ),
      by = list("layer_id", "reins_prem"),
      block_name = "Expected ceded loss per layer"
    ),
    priced = new_mutate_block(
      mutations = list(
        list(name = "technical_premium", expr = "expected_loss * 1.3"),
        list(name = "loss_ratio",        expr = "expected_loss / reins_prem")
      ),
      by = list(),
      block_name = "Treaty stats (price)"
    ),

    # === PROGRAMME KPIs (blockr.viz tiles) ===
    # The tile is a pure renderer: sum the priced layers into a ONE-ROW frame
    # whose column names ARE the card labels; the tiles then render them.
    # Blended loss ratio = sum(loss) / sum(premium), not a mean of ratios.
    prog_kpi_sum = new_summarize_block(
      summaries = list(
        list(type = "expr", name = "Expected ceded loss",
             expr = "sum(expected_loss, na.rm = TRUE)"),
        list(type = "expr", name = "Technical premium",
             expr = "sum(technical_premium, na.rm = TRUE)"),
        list(type = "expr", name = "Reinsurance premium",
             expr = "sum(reins_prem, na.rm = TRUE)"),
        list(type = "expr", name = "Blended loss ratio",
             expr = "sum(expected_loss, na.rm = TRUE) / sum(reins_prem, na.rm = TRUE)")
      ),
      by = list(),
      block_name = "Programme totals"
    ),
    prog_kpi = new_tile_block(
      value = c("Expected ceded loss", "Technical premium", "Reinsurance premium"),
      format = "number", unit = "USD m",
      block_name = "Programme KPIs"
    ),
    prog_lr_tile = new_tile_block(
      value = "Blended loss ratio", format = "percent",
      block_name = "Blended loss ratio"
    ),

    # === QUOTE TABLE (blockr.viz table, in-cell data bars) ===
    quote_table = new_table_block(
      rowname = "layer_id",
      values = c("expected_loss", "reins_prem", "technical_premium", "loss_ratio"),
      cell_color = drilldown_table_color("bar",
        columns = c("expected_loss", "technical_premium")),
      digits = 2L,
      block_name = "Quote (per layer)"
    ),

    # === PER-LAYER BAR — drill source #1 (year detail) ===
    # Mean ceded = expected ceded loss per layer. drill="layer_id": a bar click
    # emits apply_structure FILTERED to that layer (the raw simulated frame, so
    # the per-year trajectory survives) to the layer-detail views below.
    layer_bar = new_chart_block(
      chart_type = "bar", group = "layer_id", metric = "ceded", agg_fn = "mean",
      drill = "layer_id", sort_by = "layer_id", sort_dir = "asc",
      block_name = "Expected ceded loss (per layer — click to drill)"
    ),

    # === PER-LAYER KPI MATRIX — drill source #2 (the clickable tile) ===
    # One row per layer over the priced frame; drill=TRUE + by="layer_id" makes
    # a card click emit `priced` filtered to that layer, feeding the quote
    # detail table below. (Tiles don't aggregate, so they read the one-row-per-
    # layer priced frame, not the raw simulated frame.)
    layer_kpi_tile = new_tile_block(
      value = c("expected_loss", "technical_premium"),
      by = "layer_id", layout = "table", format = "number", unit = "USD m",
      drill = TRUE,
      block_name = "Per-layer KPIs (click a layer)"
    ),
    layer_quote_detail = new_table_block(
      rowname = "layer_id",
      values = c("expected_loss", "reins_prem", "technical_premium", "loss_ratio"),
      digits = 2L,
      block_name = "Selected layer — quote"
    ),

    # === LAYER DETAIL (drill targets of the bar) ===
    layer_detail_line = new_chart_block(
      chart_type = "line", x = "year", y = "ceded", metric = ".count",
      block_name = "Selected layer — ceded by sim-year"
    ),
    layer_detail_sum = new_summarize_block(
      summaries = list(
        list(type = "expr", name = "Expected ceded loss",
             expr = "mean(ceded, na.rm = TRUE)"),
        list(type = "expr", name = "Worst-year ceded",
             expr = "max(ceded, na.rm = TRUE)"),
        list(type = "expr", name = "Reinsurance premium",
             expr = "dplyr::first(reins_prem)")
      ),
      by = list(),
      block_name = "Selected layer totals"
    ),
    layer_detail_tile = new_tile_block(
      value = c("Expected ceded loss", "Worst-year ceded", "Reinsurance premium"),
      format = "number", unit = "USD m",
      block_name = "Selected layer KPIs"
    ),

    # === PROGRAMME EXCEEDANCE CURVE ===
    per_year = new_summarize_block(
      summaries = list(
        list(type = "simple", name = "total_ceded",
             func = "sum", col = "ceded")
      ),
      by = list("year"),
      block_name = "Per-year programme cession"
    ),
    exceed_rank = new_mutate_block(
      mutations = list(
        list(name = "rank",          expr = "rank(-total_ceded)"),
        list(name = "return_period", expr = "dplyr::n() / rank")
      ),
      by = list(),
      block_name = "Add return period"
    ),
    exceedance = new_chart_block(
      chart_type = "line", x = "return_period", y = "total_ceded",
      metric = ".count",
      block_name = "Exceedance curve (programme cession)"
    )
  ),

  links = links(
    from = c(
      # towers
      "calibration_seed", "structure_seed",
      # loss model: fit -> fit_table -> {fit_chart, fit_table_view}; fit -> simulate
      "calibration_tower", "fit", "fit_table", "fit_table", "fit",
      # apply structure (x = claims, y = structure)
      "simulate", "structure_tower",
      # price chain
      "apply_structure", "layer_stats",
      # programme KPI tiles
      "priced", "prog_kpi_sum", "prog_kpi_sum",
      # quote table + clickable per-layer KPI tile -> detail
      "priced", "priced", "layer_kpi_tile",
      # per-layer bar (drill) -> year-detail line + scorecard
      "apply_structure", "layer_bar", "layer_bar", "layer_detail_sum",
      # exceedance curve
      "apply_structure", "per_year", "exceed_rank"
    ),
    to = c(
      "calibration_tower", "structure_tower",
      "fit", "fit_table", "fit_chart", "fit_table_view", "simulate",
      "apply_structure", "apply_structure",
      "layer_stats", "priced",
      "prog_kpi_sum", "prog_kpi", "prog_lr_tile",
      "quote_table", "layer_kpi_tile", "layer_quote_detail",
      "layer_bar", "layer_detail_line", "layer_detail_sum", "layer_detail_tile",
      "per_year", "exceed_rank", "exceedance"
    ),
    input = c(
      "data", "data",
      "data", "data", "data", "data", "data",
      "x", "y",
      "data", "data",
      "data", "data", "data",
      "data", "data", "data",
      "data", "data", "data", "data",
      "data", "data", "data"
    )
  ),

  layouts = list(
    Quote = dock_layout(
      "prog_kpi", "prog_lr_tile", "quote_table", "layer_bar", "layer_kpi_tile",
      name = "Quote"),
    Layer_detail = dock_layout(
      "layer_bar", "layer_detail_line", "layer_detail_tile",
      "layer_kpi_tile", "layer_quote_detail",
      name = "Layer detail"),
    Loss_model = dock_layout(
      "calibration_tower", "fit_table_view", "fit_chart", name = "Loss model"),
    Simulation = dock_layout(
      "structure_tower", "exceedance", name = "Simulation")
  ),
  active = "Quote"
)

serve(board)
