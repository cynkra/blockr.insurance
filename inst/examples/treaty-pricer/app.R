# Reinsurance — Tower → Pareto fit → simulate → treaty terms → quote
#
# Layered XL treaty pricer. Extends the Pareto demo by adding the
# downstream that's missing from the market today: turning a simulated
# severity into a treaty-aware quote with reinstatement accounting.
#
# Pipeline:
#   tower (per-layer attachment, EL, treaty terms)
#     → Pareto fit (piecewise alphas, Riegel matching)
#     → simulate (N years × layers, raw layer losses)
#     → cession (apply treaty terms: capacity cap, reinstatement consumption)
#     → quote (per-layer pure premium, reinstatement load, risk load, RoL)
#
# MVP scope:
#   IN  — layers, reinstatements, implicit AAL from total capacity
#         (= cover_width × (1 + n_reinstatements))
#   OUT — explicit AAD, sublayers, profit commission, stability clauses,
#         multi-peril occurrence definitions, multi-treaty programmes
#
# Math conventions (Luca review items):
#   1. Reinstatement consumption: annual-aggregate approximation. We treat
#      the year's total layer loss as one lump, allocate first cover_width
#      to original cover, then consume reinstatements pro-rata to the
#      excess. Correct math would allocate event-by-event; this is a
#      defensible MVP simplification but breaks if intra-year event
#      ordering matters for a specific treaty wording.
#   2. Reinstatement premium: pure_premium × reinstatement_pct ×
#      reinstatements_consumed. Uses pure premium as the deposit basis
#      (resolves the circularity premium ↔ deposit).
#   3. Risk loading: standard deviation principle, μ + λ·σ over the
#      simulated annual layer cession, λ user-editable (default 0.2).
#
# Top layer note: with the Riegel tower convention the top layer's cover
# is infinite (the severity extends to ∞). Reinstatements and capacity
# cap are meaningless there; RoL is reported as NA. Document this in the
# demo — for a real treaty the top would have a finite cap.
#
# Seed: Riegel's Example1, with a default treaty: 1 reinstatement at
# 100% per layer.
#
# Run locally (with all packages installed):
#   shiny::runApp("blockr.insurance/inst/examples/treaty-pricer")
#
# In the dev container:
#   Rscript blockr.insurance/dev/treaty-pricer.R

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

# Seed tower: Riegel Example1 with default treaty terms (1 reinstatement
# at 100%, common starting point for XL pricing exercises). layer_id is
# the grid_block key; downstream joins on it so attachment edits don't
# break the wiring.
tower_seed <- tibble::tibble(
  layer_id           = seq_along(Pareto::Example1_AP),
  attachment         = as.numeric(Pareto::Example1_AP),
  expected_loss      = as.numeric(Pareto::Example1_EL),
  n_reinstatements   = rep(1L, length(Pareto::Example1_AP)),
  reinstatement_pct  = rep(1.0, length(Pareto::Example1_AP))
)

board <- new_dock_board(
  blocks = c(

    # === TOWER ===
    # One grid is the source of truth for both the loss model
    # (attachment + expected_loss feed the fit) and the contract terms
    # (n_reinstatements + reinstatement_pct feed the cession). Same row
    # = same layer, no join needed upstream.
    tower_seed = new_static_block(
      data       = tower_seed,
      block_name = "Tower seed (Riegel Example1)"
    ),
    tower = blockr.input::new_grid_block(
      state      = list(key_col = "layer_id"),
      block_name = "Tower + treaty terms"
    ),

    # === FIT ===
    # Riegel matching: input attachment + expected_loss, output one
    # alpha per piece, plus the frequency above the lowest threshold.
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

    # === SIMULATE ===
    # One row per (year, layer): year, layer_id, attachment, layer_loss.
    # layer_id propagates so the cession block can join treaty terms on
    # it (attachment is editable and could collide on float equality).
    simulate = new_function_block(
      fn = function(data, nyears = 5000L, seed = 1L) {
        if (is.null(nyears) || !is.finite(nyears) || nyears < 2L) nyears <- 5000L
        if (is.null(seed)) seed <- 1L
        if (is.null(data) || !nrow(data)) {
          return(data.frame(
            year = integer(), layer_id = integer(),
            attachment = numeric(), layer_loss = numeric()
          ))
        }
        nyears <- as.integer(nyears)
        ap  <- as.numeric(data$attachment)
        el  <- as.numeric(data$expected_loss)
        lid <- as.integer(data$layer_id)
        ord <- order(ap)
        ap <- ap[ord]; el <- el[ord]; lid <- lid[ord]
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
          layer_id   = rep(lid, each = nyears),
          attachment = rep(ap, each = nyears),
          layer_loss = as.vector(layer_losses)
        )
      },
      block_name = "Simulated years"
    ),

    # === CESSION (treaty-aware) ===
    # x = simulated long, y = tower (treaty terms). Returns per-year per-
    # layer cession after applying capacity cap + reinstatement accounting.
    #
    # Math (annual-aggregate approximation):
    #   cover_width   = diff(sorted attachments), Inf at top
    #   capacity      = cover_width × (1 + n_reinstatements)
    #   net           = min(gross_to_layer, capacity)
    #   reins_consumed = max(0, net - cover_width) / cover_width
    #   reins_premium = reins_consumed × reinstatement_pct × pure_premium
    #
    # pure_premium is computed as mean(net) per layer within this block —
    # resolves the deposit-premium ↔ reinstatement-premium circularity by
    # using pure as the deposit basis (textbook convention).
    cession = blockr.extra::new_function_xy_block(
      fn = "function(x, y) {
        if (is.null(x) || !nrow(x) || is.null(y) || !nrow(y)) {
          return(data.frame(
            year = integer(), layer_id = integer(), attachment = numeric(),
            cover_width = numeric(), gross_to_layer = numeric(),
            net_to_layer = numeric(),
            reinstatements_consumed = numeric(),
            reinstatement_premium = numeric()
          ))
        }
        ord <- order(y$attachment)
        ys  <- y[ord, , drop = FALSE]
        ap  <- as.numeric(ys$attachment)
        cw  <- c(diff(ap), Inf)
        ys$cover_width <- cw
        ys$capacity <- ys$cover_width *
          (1 + as.numeric(ys$n_reinstatements))

        m <- merge(
          x,
          ys[, c('layer_id', 'cover_width', 'capacity',
                 'n_reinstatements', 'reinstatement_pct')],
          by = 'layer_id'
        )
        m <- m[order(m$layer_id, m$year), , drop = FALSE]

        m$gross_to_layer <- m$layer_loss
        m$net_to_layer   <- pmin(m$gross_to_layer, m$capacity)
        m$reinstatements_consumed <- pmax(
          0, m$net_to_layer - m$cover_width
        ) / m$cover_width
        m$reinstatements_consumed[!is.finite(m$cover_width)] <- NA_real_

        layer_pure <- tapply(m$net_to_layer, m$layer_id, mean, na.rm = TRUE)
        m$layer_pure <- as.numeric(
          layer_pure[as.character(m$layer_id)]
        )
        m$reinstatement_premium <- ifelse(
          is.na(m$reinstatements_consumed), 0,
          m$reinstatements_consumed * m$reinstatement_pct * m$layer_pure
        )

        m[, c('year', 'layer_id', 'attachment', 'cover_width',
              'gross_to_layer', 'net_to_layer',
              'reinstatements_consumed', 'reinstatement_premium')]
      }",
      block_name = "Cession (treaty-aware)"
    ),

    # === QUOTE ===
    # Per-layer pricing summary. lambda is the std-dev loading factor.
    quote = new_function_block(
      fn = function(data, lambda = 0.2) {
        if (is.null(data) || !nrow(data)) {
          return(data.frame(
            layer_id = integer(), attachment = numeric(),
            cover_width = numeric(), pure_premium = numeric(),
            expected_reinstatement_premium = numeric(),
            risk_load = numeric(), technical_premium = numeric(),
            rate_on_line = numeric()
          ))
        }
        if (is.null(lambda) || !is.finite(lambda)) lambda <- 0.2
        ids <- sort(unique(data$layer_id))
        out <- lapply(ids, function(id) {
          d <- data[data$layer_id == id, , drop = FALSE]
          pure <- mean(d$net_to_layer, na.rm = TRUE)
          sd_  <- stats::sd(d$net_to_layer, na.rm = TRUE)
          exp_reinst <- mean(d$reinstatement_premium, na.rm = TRUE)
          risk <- lambda * sd_
          tech <- pure + exp_reinst + risk
          cw <- d$cover_width[1]
          rol <- if (is.finite(cw) && cw > 0) tech / cw else NA_real_
          data.frame(
            layer_id = id,
            attachment = d$attachment[1],
            cover_width = cw,
            pure_premium = round(pure, 1),
            expected_reinstatement_premium = round(exp_reinst, 1),
            risk_load = round(risk, 1),
            technical_premium = round(tech, 1),
            rate_on_line = round(rol, 4)
          )
        })
        do.call(rbind, out)
      },
      block_name = "Quote (per layer)"
    ),

    # === DISTRIBUTIONS ===
    # Per-layer mean reinstatement consumption. One bar per layer, value
    # = expected fraction of reinstatement capacity consumed per year
    # (0 = never used, n_reinstatements = always exhausted). Demo
    # gesture: bump a layer's expected_loss or drop its n_reinstatements,
    # watch its bar grow. The killer chart that only exists because
    # we're simulating with treaty terms.
    reins_chart = new_chart_block(
      chart_type = "bar",
      group      = "layer_id",
      metric     = "reinstatements_consumed",
      agg_fn     = "mean",
      block_name = "Mean reinstatement consumption (per layer)"
    ),

    # Programme-level exceedance curve, computed from treaty-net cession.
    total_by_year = new_summarize_block(
      state = list(
        summaries = list(
          list(type = "simple", name = "total_ceded",
               func = "sum", col = "net_to_layer")
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
    exceedance = new_chart_block(
      chart_type = "line",
      x          = "return_period",
      y          = "total_ceded",
      block_name = "Exceedance curve (programme, treaty-net)"
    )
  ),

  links = links(
    from = c(
      "tower_seed", "tower", "tower",
      "simulate", "tower",
      "cession", "cession", "cession",
      "total_by_year", "exceedance_rank"
    ),
    to = c(
      "tower", "fit_summary", "simulate",
      "cession", "cession",
      "quote", "reins_chart", "total_by_year",
      "exceedance_rank", "exceedance"
    ),
    input = c(
      "data", "data", "data",
      "x", "y",
      "data", "data", "data",
      "data", "data"
    )
  ),

  # Three workspaces. Quote is the everyday view. Distributions shows
  # the two demo-moment charts (reinstatement consumption + exceedance).
  # Diagnostics surfaces the fit and the raw cession for the curious
  # actuary (and for trust — every block is inspectable).
  layout = dock_layouts(
    Quote = dock_view(
      "tower", "quote", active = TRUE
    ),
    Distributions = dock_view(
      "tower", "exceedance", "reins_chart"
    ),
    Diagnostics = dock_view(
      "tower", "fit_summary", "cession"
    )
  )
)

serve(board)
