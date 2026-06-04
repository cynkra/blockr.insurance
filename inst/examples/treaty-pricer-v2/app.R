# Treaty pricer v2 — calibration → fit → simulate → structure → price
#
# A simulation-based pricer for a layered excess-of-loss reinsurance treaty.
# It separates the two things an XL pricing exercise actually needs:
#
#   * a LOSS MODEL — a tower of reference layers with expected losses plus an
#     expected claim frequency. Riegel (2018) showed any consistent tower of
#     expected layer losses is reproduced by a piecewise-Pareto severity; the
#     `Pareto` package fits it. Frequency is an explicit input (it fixes the
#     volatility the simulation depends on, which the expected losses alone do
#     not pin down).
#
#   * a STRUCTURE — the treaty as placed: per-layer limit, retention, annual
#     aggregate deductible (AAD) and annual aggregate limit (AAL), the subject
#     premium and the reinsurance rate.
#
# Pipeline:
#   calibration tower ─► fit (frequency) ─► piecewise-Pareto model
#                                            ├─► readable fit table
#                                            └─► simulate (N years of claims)
#   structure tower ───────────────────────────► apply structure (per-occurrence
#                                                  layering, then AAD/AAL)
#                                                    ├─► treaty stats (price)
#                                                    └─► exceedance + per-layer charts
#
# Pricing convention: expected loss is the risk-neutral fair cost; the technical
# premium adds a surcharge (loading on the pure premium), one dial. The loss
# ratio against the quoted reinsurance premium is shown alongside.
#
# Run locally (with all packages installed):
#   shiny::runApp("blockr.insurance/inst/examples/treaty-pricer-v2")
#
# In the dev container:
#   Rscript blockr.insurance/dev/treaty-pricer-v2.R

pkgload::load_all("blockr.ui")
pkgload::load_all("blockr.core")
pkgload::load_all("blockr.dock")
pkgload::load_all("blockr.dplyr")
pkgload::load_all("blockr.input")
pkgload::load_all("blockr.extra")
pkgload::load_all("blockr.bi")
pkgload::load_all("blockr.insurance")

stopifnot(requireNamespace("Pareto", quietly = TRUE))

options(
  blockr.dock_is_locked = FALSE,
  blockr.lazy_eval      = FALSE
)

# Seed loss model: reference layers (limit/retention) with expected annual
# losses. `retention` are the attachment points; the fit only needs those plus
# the expected losses (the tower is contiguous, so each limit = the gap to the
# next attachment). `layer_id` is the grid key.
calibration_seed <- tibble::tibble(
  layer_id      = 1:5,
  limit         = c(1, 3, 5, 5, 10),
  retention     = c(1, 2, 5, 10, 15),
  expected_loss = c(6, 3, 2, 1, 0.5)
)

# Seed structure: the treaty as placed. agg_retention = AAD, agg_limit = AAL.
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

    # === LOSS MODEL (calibration tower) ===
    calibration_seed = new_static_block(
      data       = calibration_seed,
      block_name = "Loss model seed"
    ),
    calibration_tower = blockr.input::new_grid_block(
      state      = list(key_col = "layer_id"),
      block_name = "Loss model (expected losses)"
    ),

    # === STRUCTURE (treaty tower) ===
    structure_seed = new_static_block(
      data       = structure_seed,
      block_name = "Structure seed"
    ),
    structure_tower = blockr.input::new_grid_block(
      state      = list(key_col = "layer_id"),
      block_name = "Treaty structure (limit, retention, AAD, AAL, premium)"
    ),

    # === FIT ===
    # Single owner of the frequency. Emits the whole PPP_Model object (not a
    # rebuilt table — reconstructing from t/alpha/FQ drops fields Simulate_Losses
    # needs). Hidden from the layout: it is plumbing, its raw value is a list.
    fit = new_function_block(
      fn = function(data, frequency = 10) {
        if (is.null(frequency) || !is.finite(frequency) || frequency <= 0) {
          frequency <- 10
        }
        if (is.null(data) || !nrow(data)) return(NULL)
        ap <- as.numeric(data$retention)
        el <- as.numeric(data$expected_loss)
        ord <- order(ap)
        Pareto::PiecewisePareto_Match_Layer_Losses(
          ap[ord], el[ord], FQ_at_lowest_AttPt = frequency
        )
      },
      block_name = "Fit (frequency owner)"
    ),

    # === FIT TABLE (readable diagnostic) ===
    fit_table = new_function_block(
      fn = function(data) {
        if (is.null(data) || !inherits(data, "PPP_Model")) {
          return(data.frame(piece = integer(), alpha = numeric()))
        }
        data.frame(
          piece      = seq_along(data$t),
          t_lower    = round(data$t, 3),
          t_upper    = round(c(data$t[-1], Inf), 3),
          alpha      = round(data$alpha, 4),
          freq_above = round(data$FQ, 6)
        )
      },
      block_name = "Fit (piecewise Pareto αs)"
    ),

    # === SIMULATE ===
    # Consumes the fitted model (so frequency is inherited, never re-entered).
    # Returns one row per simulated claim (year, severity); years with no claims
    # are kept as a single zero so every year is represented downstream.
    simulate = new_function_block(
      fn = function(data, nyears = 5000L, seed = 1L) {
        if (is.null(nyears) || !is.finite(nyears) || nyears < 2L) nyears <- 5000L
        if (is.null(seed)) seed <- 1L
        if (is.null(data) || !inherits(data, "PPP_Model")) {
          return(data.frame(year = integer(), severity = numeric()))
        }
        nyears <- as.integer(nyears)
        set.seed(as.integer(seed))
        sim <- Pareto::Simulate_Losses(data, nyears = nyears)
        if (is.null(dim(sim))) sim <- matrix(sim, nrow = nyears)
        nz   <- !is.na(sim)
        df   <- data.frame(year = row(sim)[nz], severity = sim[nz])
        miss <- setdiff(seq_len(nyears), unique(df$year))
        if (length(miss)) {
          df <- rbind(df, data.frame(year = miss, severity = 0))
        }
        df[order(df$year), , drop = FALSE]
      },
      block_name = "Simulated claims"
    ),

    # === APPLY STRUCTURE ===
    # x = simulated claims, y = structure tower. Per claim: per-occurrence
    # layering min(max(sev−ret,0),limit); sum to an annual total per layer; then
    # the aggregate features min(max(annual−AAD,0),AAL). Carries the reinsurance
    # premium (rate × subject premium) per layer so the stats block is standalone.
    apply_structure = blockr.extra::new_function_xy_block(
      fn = "function(x, y) {
        if (is.null(x) || !nrow(x) || is.null(y) || !nrow(y)) {
          return(data.frame(
            year = integer(), layer_id = integer(),
            total_in_layer = numeric(), ceded = numeric(),
            reins_prem = numeric()
          ))
        }
        yrs <- sort(unique(x$year))
        out <- lapply(seq_len(nrow(y)), function(i) {
          ret  <- as.numeric(y$retention[i])
          lim  <- as.numeric(y$limit[i])
          aad  <- as.numeric(y$agg_retention[i])
          aal  <- as.numeric(y$agg_limit[i])
          prem <- as.numeric(y$ri_rate[i]) * as.numeric(y$subject_premium[i])
          contrib <- pmin(pmax(x$severity - ret, 0), lim)
          annual  <- tapply(contrib, factor(x$year, levels = yrs), sum)
          annual  <- as.numeric(annual); annual[is.na(annual)] <- 0
          ceded   <- pmin(pmax(annual - aad, 0), aal)
          data.frame(
            year = yrs, layer_id = y$layer_id[i],
            total_in_layer = annual, ceded = ceded, reins_prem = prem
          )
        })
        do.call(rbind, out)
      }",
      block_name = "Apply structure (per-occ + AAD/AAL)"
    ),

    # === TREATY STATS (the price) ===
    treaty_stats = new_function_block(
      fn = function(data, surcharge = 0.30) {
        if (is.null(data) || !nrow(data)) {
          return(data.frame(
            layer_id = integer(), expected_loss = numeric(),
            technical_premium = numeric(), reinsurance_premium = numeric(),
            loss_ratio = numeric()
          ))
        }
        if (is.null(surcharge) || !is.finite(surcharge)) surcharge <- 0.30
        ids <- sort(unique(data$layer_id))
        out <- lapply(ids, function(id) {
          d    <- data[data$layer_id == id, , drop = FALSE]
          el   <- mean(d$ceded)
          prem <- d$reins_prem[1]
          data.frame(
            layer_id            = id,
            expected_loss       = round(el, 3),
            technical_premium   = round(el * (1 + surcharge), 3),
            reinsurance_premium = round(prem, 3),
            loss_ratio          = round(el / prem, 4)
          )
        })
        do.call(rbind, out)
      },
      block_name = "Treaty stats (price)"
    ),

    # === DISTRIBUTIONS ===
    per_year_ceded = new_summarize_block(
      state = list(
        summaries = list(
          list(type = "simple", name = "total_ceded",
               func = "sum", col = "ceded")
        ),
        by = list("year")
      ),
      block_name = "Per-year programme cession"
    ),
    exceedance_rank = new_mutate_block(
      state = list(
        mutations = list(
          list(name = "rank",          expr = "rank(-total_ceded)"),
          list(name = "return_period", expr = "dplyr::n() / rank")
        ),
        by = list()
      ),
      block_name = "Add return period"
    ),
    exceedance = new_drilldown_chart_block(
      chart_type = "line",
      x          = "return_period",
      y          = "total_ceded",
      block_name = "Exceedance curve (programme cession)"
    ),
    layer_bar = new_drilldown_chart_block(
      chart_type = "bar",
      group      = "layer_id",
      metric     = "ceded",
      agg_fn     = "mean",
      block_name = "Expected ceded loss (per layer)"
    )
  ),

  links = links(
    from = c(
      "calibration_seed", "calibration_tower", "fit", "fit",
      "structure_seed",
      "simulate", "structure_tower",
      "apply_structure", "apply_structure", "apply_structure",
      "per_year_ceded", "exceedance_rank"
    ),
    to = c(
      "calibration_tower", "fit", "fit_table", "simulate",
      "structure_tower",
      "apply_structure", "apply_structure",
      "treaty_stats", "per_year_ceded", "layer_bar",
      "exceedance_rank", "exceedance"
    ),
    input = c(
      "data", "data", "data", "data",
      "data",
      "x", "y",
      "data", "data", "data",
      "data", "data"
    )
  ),

  # Quote leads with the structure + price. Loss model groups the calibration
  # tower with its readable fit. Simulation shows the pay-off charts. Diagnostics
  # exposes the raw cession. `fit` is omitted from every view (hidden) but still
  # runs as upstream plumbing.
  layouts = list(
    Quote = dock_layout(
      "structure_tower", "treaty_stats", active = TRUE
    ),
    `Loss model` = dock_layout(
      "calibration_tower", "fit_table"
    ),
    Simulation = dock_layout(
      "simulate", "exceedance", "layer_bar"
    ),
    Diagnostics = dock_layout(
      "apply_structure"
    )
  )
)

serve(board)
