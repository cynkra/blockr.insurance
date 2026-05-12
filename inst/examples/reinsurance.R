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
    cedants_read = new_static_block(
      data       = cedants,
      block_name = "Cedants"
    ),
    data = new_dm_block(
      infer_keys = FALSE,
      block_name = "DM"
    ),

    # Declare cedants.cedant as PK and exposure.cedant as FK so
    # dm::dm_filter cascades from a cedant pick down into exposure.
    # Used by the Cedant profile drill.
    keys = new_dm_add_keys_block(
      pk_table  = "cedants",
      pk_column = "cedant",
      fk_tables = "exposure",
      fk_column = "cedant",
      block_name = "Keys"
    ),

    # Crossfilter routes each shared dim (peril, region, scenario, LOB)
    # to ONE table only when no FK relationships exist; the LAST table
    # listed wins. We want the global filter to drive the Portfolio
    # workspace (which reads from `exposure`), so events / cedants come
    # first and exposure last. The events-side chains (Accumulation /
    # Stress) rely on their own scenario filter blocks downstream.
    global_filter = new_crossfilter_block(
      active_dims = list(
        events = character(0),
        cedants = character(0),
        exposure = c(
          "scenario", "peril", "region", "line_of_business",
          "cedant", "treaty_type", "underwriting_year"
        )
      ),
      measure  = "exposure.expected_loss_usd",
      agg_func = "sum",
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

    # === PML (exceedance curves, one line per scenario) ===
    # Cascade the global filter selection (which only touches exposure)
    # onto the events table via three chained semi-filters: events.peril
    # must appear in the filtered exposure, then region, then scenario.
    # The semi-filter blocks are invisible (not in the dock_view) — they
    # execute silently in the graph so the PML chart respects the global
    # filter without showing three extra cards.
    pml_semi_peril = new_dm_semi_filter_block(
      table = "events", key_col = "peril", distinct_only = TRUE,
      block_name = "Cascade peril"
    ),
    pml_semi_region = new_dm_semi_filter_block(
      table = "events", key_col = "region", distinct_only = TRUE,
      block_name = "Cascade region"
    ),
    pml_semi_scenario = new_dm_semi_filter_block(
      table = "events", key_col = "scenario", distinct_only = TRUE,
      block_name = "Cascade scenario"
    ),
    pml_pull = new_dm_pull_block(table = "events",
      block_name = "Pull"),
    pml_arrange = new_arrange_block(
      state = list(columns = list(
        list(column = "scenario", direction = "asc"),
        list(column = "gross_loss_usd", direction = "desc")
      )),
      block_name = "Sort"
    ),
    pml_mutate = new_mutate_block(
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
    pml_curve = new_drilldown_chart_block(
      chart_type = "line",
      x_col      = "return_period_years",
      y_col      = "gross_loss_musd",
      series_by  = "scenario",
      color_by   = "scenario",
      block_name = "PML curve"
    ),

    # === CEDANT PROFILE (CEDX-style drill-into-entity) ===
    # Click a cedant bar -> latest_block captures the click -> semi-filter
    # cascades into the dm via FK -> profile cards see one cedant's slice.
    # Same pattern as the CEDX patient profile in blockr.sandbox.
    cp_drill_pull = new_dm_pull_block(table = "exposure",
      block_name = "Pull"),
    cp_drill = new_drilldown_chart_block(
      chart_type = "bar",
      group_by   = "cedant",
      color_by   = "region",
      metric     = "expected_loss_usd",
      agg_fn     = "sum",
      block_name = "Pick a cedant"
    ),
    cp_latest = new_latest_block(
      block_name = "Active click"
    ),
    cp_semi = new_dm_semi_filter_block(
      table        = "cedants",
      key_col      = "cedant",
      distinct_only = TRUE,
      block_name   = "Restrict dm"
    ),
    cp_profile_pull = new_dm_pull_block(table = "exposure",
      block_name = "Pull (cedant-scoped)"),
    cp_meta_pull = new_dm_pull_block(table = "cedants",
      block_name = "Active (table)"),
    cp_active_tile = new_tile_block(
      showcase = "number",
      state = list(
        aesthetics = list(value = c("cedant", "domicile", "segment")),
        stats = list(value = "first"),
        formats = list(measure_labels = c(
          cedant   = "Active cedant",
          domicile = "Domicile",
          segment  = "Segment"
        ))
      ),
      block_name = "Active"
    ),
    cp_kpi = new_tile_block(
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
    cp_peril_bar = new_drilldown_chart_block(
      chart_type = "bar",
      group_by   = "peril",
      color_by   = "region",
      metric     = "expected_loss_usd",
      agg_fn     = "sum",
      block_name = "Peril mix"
    ),
    cp_treaty_bar = new_drilldown_chart_block(
      chart_type = "bar",
      group_by   = "treaty_type",
      color_by   = "line_of_business",
      metric     = "premium_assumed_usd",
      agg_fn     = "sum",
      block_name = "Treaty mix"
    ),
    cp_year_bar = new_drilldown_chart_block(
      chart_type = "bar",
      group_by   = "underwriting_year",
      color_by   = "peril",
      metric     = "expected_loss_usd",
      agg_fn     = "sum",
      block_name = "Loss over time"
    )
  ),

  links = links(
    from = c(
      # data wiring: 3 statics into dm, dm into keys, keys into filter
      "exposure_read", "events_read", "cedants_read",
      "data", "keys",
      # portfolio chain
      "global_filter", "ov_pull", "ov_mutate", "ov_mutate", "ov_mutate",
      # pml chain (semi-filters cascade global filter onto events)
      "keys", "ov_pull",
      "pml_semi_peril", "ov_pull",
      "pml_semi_region", "ov_pull",
      "pml_semi_scenario",
      "pml_pull", "pml_arrange", "pml_mutate",
      # cedant profile chain
      "global_filter", "cp_drill_pull", "cp_drill",
      "keys", "cp_latest",
      "cp_semi", "cp_semi",
      "cp_meta_pull",
      "cp_profile_pull", "cp_profile_pull",
      "cp_profile_pull", "cp_profile_pull"
    ),
    to = c(
      "data", "data", "data",
      "keys", "global_filter",
      "ov_pull", "ov_mutate", "ov_kpi",
      "ov_drill_peril", "ov_drill_cedant",
      "pml_semi_peril", "pml_semi_peril",
      "pml_semi_region", "pml_semi_region",
      "pml_semi_scenario", "pml_semi_scenario",
      "pml_pull",
      "pml_arrange", "pml_mutate", "pml_curve",
      "cp_drill_pull", "cp_drill", "cp_latest",
      "cp_semi", "cp_semi",
      "cp_profile_pull", "cp_meta_pull",
      "cp_active_tile",
      "cp_kpi", "cp_peril_bar",
      "cp_treaty_bar", "cp_year_bar"
    ),
    input = c(
      # data wiring (3 statics + data->keys + keys->filter)
      "exposure", "events", "cedants",
      "data", "data",
      # portfolio chain
      "data", "data", "data", "data", "data",
      # pml chain (3 semi-filters + pull + sort + mutate + curve = 10 links)
      "dm", "ids",
      "dm", "ids",
      "dm", "ids",
      "data",
      "data", "data", "data",
      # cedant profile chain
      "data", "data", "1",
      "dm", "ids",
      "data", "data",
      "data",
      "data", "data",
      "data", "data"
    )
  ),

  extensions = list(
    blockr.dag::new_dag_extension()
  ),

  layout = dock_layouts(
    Portfolio = dock_view(
      "global_filter", "cp_active_tile",
      "ov_pull", "ov_mutate",
      "ov_kpi", "ov_drill_peril", "ov_drill_cedant",
      active = TRUE
    ),
    PML = dock_view(
      "global_filter",
      "pml_pull", "pml_arrange", "pml_mutate", "pml_curve"
    ),
    `Cedant profile` = dock_view(
      "cp_drill_pull", "cp_drill", "cp_latest", "cp_semi",
      "cp_meta_pull", "cp_active_tile", "cp_profile_pull",
      "cp_kpi", "cp_peril_bar", "cp_treaty_bar", "cp_year_bar"
    )
  )
)

serve(board, plugins = custom_plugins(c(ai_ctrl_block(), manage_project())))
