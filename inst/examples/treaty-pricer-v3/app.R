# Treaty pricer v3 — same model as v2, leaner blocks.
#
# v3 strips the function blocks down to bare, transparent R (no NULL/NA guards)
# and pushes everything expressible as a dplyr verb into blockr.dplyr blocks.
# Only four small function blocks remain — the two Pareto calls, the fit table,
# and the per-claim layering (the one step that isn't a dplyr verb). Aggregation
# and pricing are standard summarize/mutate blocks you can read off the UI.
#
# Pipeline:
#   calibration tower ─► fit ─► (fit table)
#                          └──► simulate ─► apply structure ─┬─► summarize ─► mutate (price)
#   structure tower ──────────────────────►                 ├─► summarize ─► mutate ─► exceedance
#                                                            └─► per-layer bar
#
# In the dev container:
#   Rscript blockr.insurance/dev/treaty-pricer-v3.R

pkgload::load_all("blockr.ui")
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
      state = list(
        summaries = list(
          list(type = "simple", name = "expected_loss",
               func = "mean", col = "ceded")
        ),
        by = list("layer_id", "reins_prem")
      ),
      block_name = "Expected ceded loss per layer"
    ),
    priced = new_mutate_block(
      state = list(
        mutations = list(
          list(name = "technical_premium", expr = "expected_loss * 1.3"),
          list(name = "loss_ratio",        expr = "expected_loss / reins_prem")
        ),
        by = list()
      ),
      block_name = "Treaty stats (price)"
    ),

    # === DISTRIBUTIONS (dplyr + charts) ===
    per_year = new_summarize_block(
      state = list(
        summaries = list(
          list(type = "simple", name = "total_ceded",
               func = "sum", col = "ceded")
        ),
        by = list("year")
      ),
      block_name = "Per-year programme cession"
    ),
    exceed_rank = new_mutate_block(
      state = list(
        mutations = list(
          list(name = "rank",          expr = "rank(-total_ceded)"),
          list(name = "return_period", expr = "dplyr::n() / rank")
        ),
        by = list()
      ),
      block_name = "Add return period"
    ),
    exceedance = new_chart_block(
      chart_type = "line", x = "return_period", y = "total_ceded",
      block_name = "Exceedance curve (programme cession)"
    ),
    layer_bar = new_chart_block(
      chart_type = "bar", group = "layer_id", metric = "ceded", agg_fn = "mean",
      block_name = "Expected ceded loss (per layer)"
    )
  ),

  links = links(
    from = c(
      "calibration_seed", "structure_seed",
      "calibration_tower", "fit", "fit",
      "simulate", "structure_tower",
      "apply_structure", "layer_stats",
      "apply_structure", "per_year", "exceed_rank",
      "apply_structure"
    ),
    to = c(
      "calibration_tower", "structure_tower",
      "fit", "fit_table", "simulate",
      "apply_structure", "apply_structure",
      "layer_stats", "priced",
      "per_year", "exceed_rank", "exceedance",
      "layer_bar"
    ),
    input = c(
      "data", "data",
      "data", "data", "data",
      "x", "y",
      "data", "data",
      "data", "data", "data",
      "data"
    )
  ),

  layouts = list(
    Quote = dock_layout("structure_tower", "priced", active = TRUE),
    `Loss model` = dock_layout("calibration_tower", "fit_table"),
    Simulation = dock_layout("simulate", "exceedance", "layer_bar"),
    Diagnostics = dock_layout("apply_structure", "layer_stats")
  )
)

serve(board)
