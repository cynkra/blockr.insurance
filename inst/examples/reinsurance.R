# Reinsurance Group-Risk Demo (Swiss Re conversation).
#
# One board, six workspaces, one synthetic assumed-book cube + 4,000-
# event modelled catalogue (fanned across five scenarios), wired with
# blockr.ai's chat plugin so any block's config can be driven from
# natural language.
#
# Run from workspace root:
#   Rscript blockr.insurance/inst/examples/reinsurance.R
#
# Data: see inst/examples/_reins_data.R (synthetic, seeded).
# AI prompts work once OPENAI_API_KEY is set.
#
# Workspaces:
#   Setup           — data sources, dm, DAG.
#   Portfolio       — KPIs + drilldowns by peril / cedant on the
#                     filtered book.
#   Accumulation    — exceedance curve rebuilt from the filtered
#                     event catalogue, coloured by peril.
#   Stress          — overlay exceedance curves across five
#                     scenarios (Baseline / RCP 4.5 / RCP 8.5 /
#                     Pandemic / Cyber Tail).
#   Event profile   — top events bar; click a bar to drill into a
#                     single event's cedant + treaty breakdown.
#   Tail            — top-25 tail events by cedant x peril.

options(
  blockr.dock_is_locked     = FALSE,
  blockr.eval_parent_env    = asNamespace("stats"),
  blockr.html_table_preview = TRUE,
  blockr.session_url_params = TRUE,
  blockr.lazy_eval          = FALSE
)

pkgload::load_all("blockr.core")
pkgload::load_all("blockr.dock")
pkgload::load_all("blockr.dm")
pkgload::load_all("blockr.dplyr")
pkgload::load_all("blockr.io")
pkgload::load_all("blockr.sandbox")
pkgload::load_all("blockr.extra")
pkgload::load_all("blockr.bi")
pkgload::load_all("blockr.session")
pkgload::load_all("blockr.dag")
pkgload::load_all("blockr.ai")
pkgload::load_all("blockr.insurance")

source(system.file("examples", "_reins_data.R",
  package = "blockr.insurance", mustWork = TRUE))

# Fan exposure + events across scenarios with per-peril shocks.
shocks <- list(
  Baseline = c(
    Windstorm = 1.0, Flood = 1.0, Earthquake = 1.0, Wildfire = 1.0,
    Cyber = 1.0, Pandemic = 1.0, Motor = 1.0, Casualty = 1.0
  ),
  `RCP 4.5 (2050)` = c(
    Windstorm = 1.20, Flood = 1.30, Earthquake = 1.0, Wildfire = 1.6,
    Cyber = 1.0, Pandemic = 1.0, Motor = 1.0, Casualty = 1.0
  ),
  `RCP 8.5 (2050)` = c(
    Windstorm = 1.50, Flood = 1.70, Earthquake = 1.0, Wildfire = 2.5,
    Cyber = 1.0, Pandemic = 1.0, Motor = 1.0, Casualty = 1.0
  ),
  Pandemic = c(
    Windstorm = 1.0, Flood = 1.0, Earthquake = 1.0, Wildfire = 1.0,
    Cyber = 1.0, Pandemic = 3.0, Motor = 0.7, Casualty = 1.0
  ),
  `Cyber Tail` = c(
    Windstorm = 1.0, Flood = 1.0, Earthquake = 1.0, Wildfire = 1.0,
    Cyber = 4.0, Pandemic = 1.0, Motor = 1.0, Casualty = 1.2
  )
)

scenario_events <- do.call(rbind, lapply(names(shocks), function(sc) {
  sh <- shocks[[sc]]
  ev <- treaty_events
  ev$scenario <- sc
  ev$gross_loss_usd <- round(ev$gross_loss_usd * sh[ev$peril])
  ev[, c("scenario", setdiff(names(ev), c("scenario",
    "return_period_years", "exceedance_prob")))]
}))
rownames(scenario_events) <- NULL

scenario_exposure <- do.call(rbind, lapply(names(shocks), function(sc) {
  sh <- shocks[[sc]]
  ex <- treaty_exposure
  ex$scenario <- sc
  ex$expected_loss_usd <- round(ex$expected_loss_usd * sh[ex$peril])
  ex[, c("scenario", setdiff(names(ex), "scenario"))]
}))
rownames(scenario_exposure) <- NULL

board <- new_dock_board(
  blocks = c(

    # === SHARED DATA ===
    exposure_read = new_static_block(
      data       = scenario_exposure,
      block_name = "Exposure"
    ),
    events_read = new_static_block(
      data       = scenario_events,
      block_name = "Events"
    ),
    profile_read = new_static_block(
      data       = event_profile,
      block_name = "Profile"
    ),
    data = new_dm_block(
      infer_keys = FALSE,
      block_name = "DM"
    ),

    # Crossfilter routes each shared dim (peril, region, scenario, LOB)
    # to ONE table only when no FK relationships exist; the LAST table
    # listed wins. We want the global filter to drive the Portfolio
    # workspace (which reads from `exposure`), so events comes first
    # and exposure last. The events-side chains (Accumulation / Stress)
    # rely on their own scenario filter blocks downstream.
    global_filter = new_crossfilter_block(
      active_dims = list(
        events = character(0),
        exposure = c(
          "scenario", "peril", "region", "line_of_business",
          "cedant", "treaty_type", "underwriting_year"
        )
      ),
      block_name = "Filter"
    ),

    # === PORTFOLIO ===
    ov_pull = new_dm_pull_block(table = "exposure",
      block_name = "Pull"),
    ov_mutate = new_mutate_block(
      state = list(
        mutations = list(list(
          name = "loss_ratio",
          expr = "expected_loss_usd / premium_assumed_usd"
        )),
        by = list()
      ),
      block_name = "Loss ratio"
    ),
    ov_kpi = new_tile_block(
      showcase = "number",
      state = list(
        aesthetics = list(value = c(
          "premium_assumed_usd", "exposure_usd", "expected_loss_usd"
        )),
        stats = list(value = "sum"),
        formats = list(measure_labels = c(
          premium_assumed_usd = "Premium",
          exposure_usd        = "Exposure",
          expected_loss_usd   = "Expected loss"
        ))
      ),
      block_name = "KPIs"
    ),
    ov_drill_peril = new_drilldown_chart_block(
      chart_type = "bar",
      group_by   = "peril",
      color_by   = "region",
      metric     = "exposure_usd",
      agg_fn     = "sum",
      block_name = "Peril x Region"
    ),
    ov_drill_cedant = new_drilldown_chart_block(
      chart_type = "bar",
      group_by   = "cedant",
      color_by   = "line_of_business",
      metric     = "expected_loss_usd",
      agg_fn     = "sum",
      block_name = "Cedant x LOB"
    ),

    # === ACCUMULATION (current scenario, filtered events) ===
    acc_pull = new_dm_pull_block(table = "events",
      block_name = "Pull"),
    acc_baseline_filter = new_filter_block(
      state = list(
        conditions = list(list(
          type = "values", column = "scenario",
          values = list("Baseline")
        )),
        operator = "&"
      ),
      block_name = "Scenario"
    ),
    acc_arrange = new_arrange_block(
      state = list(columns = list(
        list(column = "gross_loss_usd", direction = "desc")
      )),
      block_name = "Sort"
    ),
    acc_mutate = new_mutate_block(
      state = list(
        mutations = list(
          list(name = "rank_in_filter",
               expr = "dplyr::row_number()"),
          list(name = "exceedance_prob",
               expr = "rank_in_filter / dplyr::n()"),
          list(name = "return_period_years",
               expr = "round(5 * dplyr::n() / rank_in_filter, 1)"),
          list(name = "gross_loss_musd",
               expr = "round(gross_loss_usd / 1e6, 1)")
        ),
        by = list()
      ),
      block_name = "Exceedance"
    ),
    acc_curve = new_drilldown_chart_block(
      chart_type = "line",
      x_col      = "return_period_years",
      y_col      = "gross_loss_musd",
      series_by  = "peril",
      color_by   = "peril",
      block_name = "Exceedance curve"
    ),

    # === STRESS (all scenarios overlaid) ===
    str_pull = new_dm_pull_block(table = "events",
      block_name = "Pull"),
    str_arrange = new_arrange_block(
      state = list(columns = list(
        list(column = "scenario", direction = "asc"),
        list(column = "gross_loss_usd", direction = "desc")
      )),
      block_name = "Sort"
    ),
    str_mutate = new_mutate_block(
      state = list(
        mutations = list(
          list(name = "rank_in_filter",
               expr = "dplyr::row_number()"),
          list(name = "exceedance_prob",
               expr = "rank_in_filter / dplyr::n()"),
          list(name = "return_period_years",
               expr = "round(5 * dplyr::n() / rank_in_filter, 1)"),
          list(name = "gross_loss_musd",
               expr = "round(gross_loss_usd / 1e6, 1)")
        ),
        by = "scenario"
      ),
      block_name = "Exceedance by scenario"
    ),
    str_curve = new_drilldown_chart_block(
      chart_type = "line",
      x_col      = "return_period_years",
      y_col      = "gross_loss_musd",
      series_by  = "scenario",
      color_by   = "scenario",
      block_name = "Stress overlay"
    ),

    # === EVENT PROFILE (drill to single event) ===
    # Top-events bar chart drills by event_id; downstream blocks see
    # only the clicked event's rows (cedant + treaty breakdown).
    prof_pull = new_dm_pull_block(table = "profile",
      block_name = "Pull"),
    prof_top_slice = new_slice_block(
      state = list(
        type = "max", n = 300L, prop = NULL,
        order_by = "gross_loss_usd", with_ties = TRUE,
        weight_by = "", replace = FALSE,
        rows = "1:300", by = list()
      ),
      block_name = "Top events"
    ),
    prof_drill_events = new_drilldown_chart_block(
      chart_type = "bar",
      group_by   = "event_id",
      color_by   = "peril",
      metric     = "gross_loss_usd",
      agg_fn     = "max",
      block_name = "Pick an event"
    ),
    prof_cedant_filter = new_filter_block(
      state = list(
        conditions = list(list(
          type = "values", column = "breakdown_type",
          values = list("cedant")
        )),
        operator = "&"
      ),
      block_name = "Cedant rows"
    ),
    prof_cedant_bar = new_drilldown_chart_block(
      chart_type = "bar",
      group_by   = "breakdown_key",
      metric     = "breakdown_amount",
      agg_fn     = "sum",
      block_name = "Cedant share"
    ),
    prof_treaty_filter = new_filter_block(
      state = list(
        conditions = list(list(
          type = "values", column = "breakdown_type",
          values = list("treaty")
        )),
        operator = "&"
      ),
      block_name = "Treaty rows"
    ),
    prof_treaty_bar = new_drilldown_chart_block(
      chart_type = "bar",
      group_by   = "breakdown_key",
      color_by   = "peril",
      metric     = "breakdown_amount",
      agg_fn     = "sum",
      block_name = "Treaty layers"
    ),

    # === TAIL ===
    tail_pull = new_dm_pull_block(table = "events",
      block_name = "Pull"),
    tail_slice = new_slice_block(
      state = list(
        type = "max", n = 25L, prop = NULL,
        order_by = "gross_loss_usd", with_ties = TRUE,
        weight_by = "", replace = FALSE,
        rows = "1:25", by = list()
      ),
      block_name = "Top 25"
    ),
    tail_drill = new_drilldown_chart_block(
      chart_type = "bar",
      group_by   = "primary_cedant",
      color_by   = "peril",
      metric     = "gross_loss_usd",
      agg_fn     = "sum",
      block_name = "Cedant x Peril"
    )
  ),

  links = links(
    from = c(
      # data wiring
      "exposure_read", "events_read", "profile_read", "data",
      # portfolio chain
      "global_filter", "ov_pull", "ov_mutate", "ov_mutate", "ov_mutate",
      # accumulation chain
      "global_filter", "acc_pull", "acc_baseline_filter",
      "acc_arrange", "acc_mutate",
      # stress chain
      "global_filter", "str_pull", "str_arrange", "str_mutate",
      # event profile chain (drill emits filtered profile downstream)
      "global_filter", "prof_pull", "prof_top_slice", "prof_drill_events",
      "prof_cedant_filter",
      "prof_drill_events", "prof_treaty_filter",
      # tail chain
      "global_filter", "tail_pull", "tail_slice"
    ),
    to = c(
      "data", "data", "data", "global_filter",
      "ov_pull", "ov_mutate", "ov_kpi",
      "ov_drill_peril", "ov_drill_cedant",
      "acc_pull", "acc_baseline_filter", "acc_arrange",
      "acc_mutate", "acc_curve",
      "str_pull", "str_arrange", "str_mutate", "str_curve",
      "prof_pull", "prof_top_slice", "prof_drill_events",
      "prof_cedant_filter",
      "prof_cedant_bar",
      "prof_treaty_filter", "prof_treaty_bar",
      "tail_pull", "tail_slice", "tail_drill"
    ),
    input = c(
      # data wiring
      "exposure", "events", "profile", "data",
      # portfolio chain
      "data", "data", "data", "data", "data",
      # accumulation chain
      "data", "data", "data", "data", "data",
      # stress chain
      "data", "data", "data", "data",
      # event profile chain
      "data", "data", "data", "data", "data", "data", "data",
      # tail chain
      "data", "data", "data"
    )
  ),

  extensions = list(
    blockr.dag::new_dag_extension()
  ),

  layout = dock_layouts(
    Setup = dock_view(
      "exposure_read", "events_read", "profile_read", "data",
      "dag_extension",
      active = TRUE
    ),
    Portfolio = dock_view(
      "global_filter",
      "ov_pull", "ov_mutate",
      "ov_kpi", "ov_drill_peril", "ov_drill_cedant"
    ),
    Accumulation = dock_view(
      "global_filter",
      "acc_pull", "acc_baseline_filter", "acc_arrange",
      "acc_mutate", "acc_curve"
    ),
    Stress = dock_view(
      "global_filter",
      "str_pull", "str_arrange", "str_mutate", "str_curve"
    ),
    `Event profile` = dock_view(
      "prof_pull", "prof_top_slice", "prof_drill_events",
      "prof_cedant_filter", "prof_cedant_bar",
      "prof_treaty_filter", "prof_treaty_bar"
    ),
    Tail = dock_view(
      "global_filter",
      "tail_pull", "tail_slice", "tail_drill"
    )
  )
)

serve(board, plugins = custom_plugins(c(ai_ctrl_block(), manage_project())))
