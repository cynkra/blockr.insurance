# Portfolio explorer — same book, two pricing scenarios.
#
# Run from an R session after installing blockr.insurance:
#
#   library(blockr.insurance)
#   source(system.file("examples", "portfolio_explorer.R",
#                      package = "blockr.insurance"))
#
# The two bundled fixtures (`portfolio-property/` and
# `portfolio-property-comparison/`) share inputs verbatim — same locations,
# same claims — and differ only in the parameter set the engine runs with
# (the comparison set bumps Italian `base_rate` by 30%). This is the
# rate-change-study comparison shape: identical book, two pricing
# scenarios, so per-location keys match exactly and the per-location diff
# is meaningful.
#
# Four workspaces:
#
#   Setup        Two portfolio-premium tables (base + comparison).
#
#   Base         Crossfilter on the base portfolio (country / policy_id /
#                peril) → drill-down chart.
#
#   Alternative  Crossfilter on the comparison portfolio (independent of
#                base — both filters are local) → drill-down chart.
#
#   Comparison   Per-location compare(base, comparison) on
#                (policy_id, location_id), with a diff drill-down and a
#                waterfall summing the diffs across base / exposure /
#                risk / model price.
#
# Note: the two crossfilters are independent. To compare like-for-like,
# set the same dim selections on both. A future blockr.dm enhancement
# (filter-state-aware semi-filter) would let one crossfilter drive both.
#
# `new_crossfilter_block()` accepts a plain data frame — it wraps it
# internally as a single-table dm under the synthetic name `.tbl`, which
# is why `active_dims` is keyed by `.tbl` below. No upstream `new_dm_block`
# / `new_dm_pull_block` plumbing is needed.

options(
  blockr.dock_is_locked = FALSE,
  blockr.html_table_preview = TRUE
)

pkgload::load_all("blockr.core")
pkgload::load_all("blockr.dock")
pkgload::load_all("blockr.dm")
pkgload::load_all("blockr.bi")
pkgload::load_all("blockr.extra")
pkgload::load_all("blockr.sandbox")
pkgload::load_all("blockr.insurance")

base_dir       <- default_portfolio_dir()
comparison_dir <- default_comparison_portfolio_dir()
stopifnot(nzchar(base_dir), nzchar(comparison_dir))

active_dims  <- list(.tbl = c("country", "policy_id", "peril"))
# `base_premium` is per-location (one value per insured location, ~69 unique
# across the portfolio) so it qualifies as a measure. Broadcast columns like
# `model_price` and `risk_premium` only have one value per policy (~5 unique
# values) and are silently classified as categorical by `new_crossfilter_block`'s
# low-cardinality heuristic, which is why they cannot be used as the measure.
crossfilter_measure  <- ".tbl.model_price"
crossfilter_agg_func <- "sum"

board <- new_dock_board(
  blocks = c(

    # === SETUP ===
    base_premium = new_portfolio_premium_block(
      dir        = base_dir,
      block_name = "Base premium (portfolio)"
    ),
    comparison_premium = new_portfolio_premium_block(
      dir        = comparison_dir,
      block_name = "Comparison premium (portfolio)"
    ),

    # === BASE (full portfolio 1) ===
    base_xfilter = new_crossfilter_block(
      active_dims = active_dims,
      measure     = crossfilter_measure,
      agg_func    = crossfilter_agg_func,
      block_name  = "Base filter (country / policy_id / peril)"
    ),
    base_drilldown = new_drilldown_chart_block(
      group_by   = "policy_id",
      metric     = "model_price",
      agg_fn     = "sum",
      chart_type = "bar",
      block_name = "Base model_price by policy"
    ),

    # === ALTERNATIVE (full portfolio 2; local crossfilter) ===
    comparison_xfilter = new_crossfilter_block(
      active_dims = active_dims,
      measure     = crossfilter_measure,
      agg_func    = crossfilter_agg_func,
      block_name  = "Comparison filter (country / policy_id / peril)"
    ),
    comparison_drilldown = new_drilldown_chart_block(
      group_by   = "policy_id",
      metric     = "model_price",
      agg_fn     = "sum",
      chart_type = "bar",
      block_name = "Comparison model_price by policy"
    ),

    # === COMPARISON (per-location diff) ===
    # `country` and `peril` are added to key_cols (alongside the actual
    # keys) only so they survive `compare_frames`'s key-cols-only
    # projection — that lets the diff crossfilter slice by them.
    compare = new_compare_block(
      key_cols     = c("policy_id", "location_id", "country", "peril"),
      measure_cols = c("base_premium", "exposure_premium",
                       "risk_premium", "model_price"),
      metric       = "diff",
      block_name   = "Compare base vs comparison"
    ),
    compare_xfilter = new_crossfilter_block(
      active_dims = active_dims,
      measure     = crossfilter_measure,
      agg_func    = crossfilter_agg_func,
      block_name  = "Diff filter (country / policy_id / peril)"
    ),
    compare_drilldown = new_drilldown_chart_block(
      group_by   = "country",
      metric     = "model_price",
      agg_fn     = "sum",
      chart_type = "bar",
      block_name = "Diff in model_price by country"
    ),
    compare_waterfall = new_waterfall_block(
      measures   = c("base_premium", "exposure_premium",
                     "risk_premium", "model_price"),
      block_name = "Diff waterfall"
    )
  ),

  links = links(
    from = c(
      # Base
      "base_premium",        "base_xfilter",
      # Alternative
      "comparison_premium",  "comparison_xfilter",
      # Comparison
      "base_xfilter",        "comparison_xfilter",
      "compare",             "compare_xfilter",     "compare_xfilter"
    ),
    to = c(
      "base_xfilter",        "base_drilldown",
      "comparison_xfilter",  "comparison_drilldown",
      "compare",             "compare",
      "compare_xfilter",     "compare_drilldown",   "compare_waterfall"
    ),
    input = c(
      "data",                "data",
      "data",                "data",
      "x",                   "y",
      "data",                "data",                "data"
    )
  ),

  layout = dock_layouts(
    Setup = dock_view(
      "base_premium", "comparison_premium"
    ),
    Base = dock_view(
      "base_xfilter", "base_drilldown",
      active = TRUE
    ),
    Alternative = dock_view(
      "comparison_xfilter", "comparison_drilldown"
    ),
    Comparison = dock_view(
      "compare", "compare_xfilter",
      "compare_drilldown", "compare_waterfall"
    )
  )
)

serve(board)
