# Reinsurance — Tower → piecewise Pareto → simulated data
#
# Riegel (2018, Matching tower information with piecewise Pareto) showed
# that for any consistent tower of expected layer losses there exists a
# piecewise Pareto severity reproducing those losses. Ulrich Riegel's
# `Pareto` package on CRAN implements the fit. This app exposes the
# claim end-to-end: an actuary edits the tower, the simulation draws
# many years from the fitted collective model, summarize confirms the
# empirical layer means match the tower input, and the exceedance curve
# falls out of the simulated data.
#
# Seed: Riegel's Example1 (Pareto::Example1_AP / Example1_EL).
#
# Run locally (with all packages installed):
#   shiny::runApp("blockr.insurance/inst/examples/pareto")
#
# Install (one-off):
#   install.packages(c("Pareto", "tibble", "pak"))
#   pak::pak(c(
#     "blockr-org/blockr.core", "blockr-org/blockr.dock",
#     "blockr-org/blockr.dplyr", "blockr-org/blockr.input",
#     "blockr-org/blockr.extra", "blockr-org/blockr.ggplot",
#     "blockr-org/blockr.insurance"
#   ))

# Absolute paths because shiny::runApp() sources this file from the app
# dir (inst/examples/pareto/), where the workspace-relative paths
# "blockr.core" etc. don't resolve.
pkgload::load_all("blockr.core")
pkgload::load_all("blockr.dock")
pkgload::load_all("blockr.dplyr")
pkgload::load_all("blockr.input")
pkgload::load_all("blockr.extra")
pkgload::load_all("blockr.viz")
pkgload::load_all("blockr.insurance")

stopifnot(requireNamespace("Pareto", quietly = TRUE))

options(
  blockr.dock_is_locked     = FALSE,
  blockr.lazy_eval          = FALSE
)

# Seed tower — Riegel Example1. `layer_id` is the grid_block key so the
# user can edit attachments and expected losses without breaking upserts.
tower_seed <- tibble::tibble(
  layer_id      = seq_along(Pareto::Example1_AP),
  attachment    = as.numeric(Pareto::Example1_AP),
  expected_loss = as.numeric(Pareto::Example1_EL)
)

board <- new_dock_board(
  blocks = c(

    # === TOWER ===
    tower_seed = new_static_block(
      data       = tower_seed,
      block_name = "Tower (Riegel Example1)"
    ),
    # Namespace-qualified because blockr.ggplot ALSO exports a
    # `new_grid_block` (a facet config block); the load order would
    # otherwise mask blockr.input's table editor.
    tower = blockr.input::new_grid_block(
      state      = list(key_col = "layer_id"),
      block_name = "Tower editor"
    ),

    # === FIT (αs) ===
    fit_summary = new_function_block(
      fn = function(data) {
        if (is.null(data) || !nrow(data)) {
          return(data.frame(piece = integer(), alpha = numeric()))
        }
        ap <- as.numeric(data$attachment)
        el <- as.numeric(data$expected_loss)
        ord <- order(ap)
        ap <- ap[ord]; el <- el[ord]
        fit <- Pareto::PiecewisePareto_Match_Layer_Losses(ap, el)
        data.frame(
          piece      = seq_along(fit$t),
          t_lower    = fit$t,
          t_upper    = c(fit$t[-1], Inf),
          alpha      = round(fit$alpha, 4),
          freq_above = round(fit$FQ, 6)
        )
      },
      block_name = "Fit (piecewise Pareto αs)"
    ),

    # === SIMULATE (long) ===
    # One row per (year, layer): year, attachment, layer_loss. Long is
    # the canonical shape — verify groups by attachment, the exceedance
    # branch aggregates by year via summarize.
    simulate = new_function_block(
      fn = function(data, nyears = 5000L, seed = 1L) {
        # function_block sends NULL params on initial render before the UI
        # syncs — fall back to declared defaults so Simulate_Losses doesn't
        # collapse to a scalar NaN.
        if (is.null(nyears) || !is.finite(nyears) || nyears < 2L) nyears <- 5000L
        if (is.null(seed)) seed <- 1L
        if (is.null(data) || !nrow(data)) {
          return(data.frame(
            year = integer(), attachment = numeric(), layer_loss = numeric()
          ))
        }
        nyears <- as.integer(nyears)
        ap <- as.numeric(data$attachment)
        el <- as.numeric(data$expected_loss)
        ord <- order(ap); ap <- ap[ord]; el <- el[ord]
        fit <- Pareto::PiecewisePareto_Match_Layer_Losses(ap, el)
        covers <- c(diff(ap), Inf)
        set.seed(seed)
        sim_mat <- Pareto::Simulate_Losses(fit, nyears = nyears)
        layer_losses <- vapply(
          seq_along(ap),
          function(k) {
            rowSums(pmin(pmax(sim_mat - ap[k], 0), covers[k]), na.rm = TRUE)
          },
          numeric(nyears)
        )
        data.frame(
          year       = rep(seq_len(nyears), times = length(ap)),
          attachment = rep(ap, each = nyears),
          layer_loss = as.vector(layer_losses)
        )
      },
      block_name = "Simulated years"
    ),

    # === VERIFY — Riegel's claim, empirically ===
    # group_by(attachment) + mean(layer_loss) → one row per tower layer.
    # The `mean_loss` column should match the tower's `expected_loss`
    # column. Convergence is tight on lower layers and noisier on the
    # tail (the top layer extends to ∞); crank nyears in the simulator
    # cogwheel to tighten it.
    verify_summary = new_summarize_block(
      state = list(
        summaries = list(
          list(type = "simple", name = "mean_loss",
               func = "mean", col = "layer_loss"),
          list(type = "simple", name = "n_years",
               func = "n", col = "year")
        ),
        by = list("attachment")
      ),
      block_name = "Empirical layer means"
    ),

    # === EXCEEDANCE AGGREGATE ===
    # Sum layer losses to a per-year total, then add return_period via
    # mutate so the chart can plot it directly.
    total_by_year = new_summarize_block(
      state = list(
        summaries = list(
          list(type = "simple", name = "total_loss",
               func = "sum", col = "layer_loss")
        ),
        by = list("year")
      ),
      block_name = "Per-year totals"
    ),
    exceedance_rank = new_mutate_block(
      state = list(
        mutations = list(
          list(name = "rank",          expr = "rank(-total_loss)"),
          list(name = "return_period", expr = "dplyr::n() / rank")
        ),
        by = list()
      ),
      block_name = "Add return period"
    ),

    # === EXCEEDANCE CURVE ===
    # drilldown_chart line, x=return_period y=total_loss. echarts-backed
    # (nicer than ggplot for this kind of plot).
    exceedance = new_chart_block(
      chart_type = "line",
      x          = "return_period",
      y          = "total_loss",
      block_name = "Exceedance curve"
    )
  ),

  links = links(
    from = c(
      "tower_seed", "tower", "tower",
      "simulate", "simulate",
      "total_by_year", "exceedance_rank"
    ),
    to = c(
      "tower", "fit_summary", "simulate",
      "verify_summary", "total_by_year",
      "exceedance_rank", "exceedance"
    ),
    input = c(
      "data", "data", "data",
      "data", "data",
      "data", "data"
    )
  ),

  # Layout: tower editor on the left; everything else grouped on the
  # right as a tab stack. tower_seed (static seed) is intentionally
  # omitted — per blockr.dock semantics, omission = hide, but it still
  # runs because the link tower_seed → tower is upstream of it.
  # Layout: tower editor on the left; everything else tabbed on the right.
  # In dock_view, a character vector is one leaf (= one tab group);
  # `list(...)` would split further. tower_seed is intentionally omitted
  # — per blockr.dock semantics, omission = hide, but it still runs
  # because the link tower_seed → tower keeps it upstream.
  # Multi-view layout: tower editor on the left in every view, content
  # blocks on the right. Top-level views become tabs in the dock header.
  # Active view = Chart (so the exceedance curve is visible by default).
  # tower_seed (static seed) is omitted — per blockr.dock semantics,
  # omission = hide, but it still runs because the link
  # tower_seed → tower keeps it upstream.
  layout = dock_layouts(
    Chart      = dock_view("tower", "exceedance", active = TRUE),
    Verify     = dock_view("tower", "verify_summary"),
    Fit        = dock_view("tower", "fit_summary"),
    Simulation = dock_view(
      "tower", "simulate", "total_by_year", "exceedance_rank"
    )
  )
)

serve(board)
