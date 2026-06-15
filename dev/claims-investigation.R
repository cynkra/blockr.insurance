# Motor claims investigation POC — drill into anomalous claim activity by
# segment, vehicle, year, cohort.
#
# Run from an R session at the workspace root:
#
#   source("blockr.insurance/dev/claims-investigation.R")
#
# Five workspaces (Setup / KPIs / Drill-down / Cohort waterfall / Top claims)
# operate on the bundled `motor_losses` dataset (motor portfolio losses
# developed over DY0..DY15). See `?motor_losses`.
#
# Story: a claims/reinsurance manager sees the loss ratio surprise and uses
# the dashboard to follow the thread — segment severity, frequency over
# time, development pattern, and the largest individual claims.

options(
  blockr.dock_is_locked = FALSE,
  blockr.eval_parent_env = asNamespace("stats"),
  blockr.html_table_preview = TRUE,
  blockr.session_url_params = TRUE,
  blockr.lazy_eval = FALSE
)

if (!interactive()) {
  options(shiny.host = "0.0.0.0", shiny.port = 3838L)
}

pkgload::load_all("blockr.core")
pkgload::load_all("blockr.dock")
pkgload::load_all("blockr.dm")
pkgload::load_all("blockr.dplyr")
pkgload::load_all("blockr.io")
pkgload::load_all("blockr.sandbox")
pkgload::load_all("blockr.extra")
pkgload::load_all("blockr.viz")
pkgload::load_all("blockr.session")
pkgload::load_all("blockr.dag")

board <- new_dock_board(
  blocks = c(

    # === SHARED DATA ===
    loss_read = new_dataset_block(
      dataset    = "motor_losses",
      package    = "blockr.insurance",
      block_name = "motor_losses"
    ),
    data = new_dm_block(
      infer_keys = FALSE,
      block_name = "Loss dm (single table: loss)"
    ),
    global_filter = new_crossfilter_block(
      active_dims = list(loss = c(
        "Year", "Insurance_Company", "Fleet", "Vehicle_type",
        "Cover", "Age_Class", "Gender"
      )),
      block_name = "Global filter (claim segmentation)"
    ),

    # === KPIs ===
    kpi_pull = new_dm_pull_block(table = "loss",
      block_name = "Pull loss"),
    kpi_sum = new_summarize_block(
      summaries = list(
        list(type = "expr", name = "Num_Claims",
             expr = "sum(Num_Claims, na.rm = TRUE)"),
        list(type = "expr", name = "Total_Incurred",
             expr = "sum(Total_Incurred, na.rm = TRUE)")
      ),
      by = list(),
      block_name = "Grand totals over filtered cube"
    ),
    kpi_mutate = new_mutate_block(
      mutations = list(
        list(name = "Avg_Severity",
             expr = "Total_Incurred / Num_Claims")
      ),
      by = list(),
      block_name = "Avg_Severity = Total_Incurred / Num_Claims"
    ),
    # The tile block is a pure renderer (it no longer aggregates), so the
    # KPI measures are collapsed upstream into a one-row frame whose column
    # names are the card labels; the tile then renders them as KPI cards.
    # The feeder (kpi_mutate) is already a single row, so sum == first == the
    # value for Num_Claims / Total_Incurred. Avg_Severity is an average, so it
    # is collapsed with mean() (sum of an average would be wrong); on a one-row
    # frame mean is the identity, matching the old behaviour.
    kpi_tile_sum = new_summarize_block(
      summaries = list(
        list(type = "expr", name = "Total claims",
             expr = "sum(Num_Claims, na.rm = TRUE)"),
        list(type = "expr", name = "Total incurred",
             expr = "sum(Total_Incurred, na.rm = TRUE)"),
        list(type = "expr", name = "Avg severity",
             expr = "mean(Avg_Severity, na.rm = TRUE)")
      ),
      by = character(),
      block_name = "Claims KPI totals"
    ),
    kpi = new_tile_block(
      value = c("Total claims", "Total incurred", "Avg severity"),
      format = "compact",
      block_name = "Claims headline KPIs"
    ),

    # === DRILL-DOWN: severity by vehicle type & cover ===
    sev_pull = new_dm_pull_block(table = "loss",
      block_name = "Pull loss"),
    sev_sum = new_summarize_block(
      summaries = list(
        list(type = "expr", name = "Num_Claims",
             expr = "sum(Num_Claims, na.rm = TRUE)"),
        list(type = "expr", name = "Total_Incurred",
             expr = "sum(Total_Incurred, na.rm = TRUE)")
      ),
      by = c("Vehicle_type", "Cover"),
      block_name = "Sum by Vehicle x Cover"
    ),
    sev_drill = new_chart_block(
      chart_type = "bar",
      group   = "Vehicle_type",
      color   = "Cover",
      metric     = "Total_Incurred",
      agg_fn     = "sum",
      block_name = "Total incurred by Vehicle x Cover"
    ),

    # === DRILL-DOWN: frequency / severity over time, per insurer ===
    trend_pull = new_dm_pull_block(table = "loss",
      block_name = "Pull loss"),
    trend_sum = new_summarize_block(
      summaries = list(
        list(type = "expr", name = "Num_Claims",
             expr = "sum(Num_Claims, na.rm = TRUE)"),
        list(type = "expr", name = "Total_Incurred",
             expr = "sum(Total_Incurred, na.rm = TRUE)")
      ),
      by = c("Year", "Insurance_Company"),
      block_name = "Sum by Year x Company"
    ),
    trend_year_chr = new_mutate_block(
      mutations = list(list(name = "Year",
                            expr = "as.character(Year)")),
      by = list(),
      block_name = "Year -> character (categorical X axis)"
    ),
    trend_drill = new_chart_block(
      chart_type = "line",
      x      = "Year",
      y      = "Total_Incurred",
      series  = "Insurance_Company",
      block_name = "Total incurred over time, per insurer"
    ),

    # === COHORT WATERFALL: claim emergence over development years ===
    # Build a small bridge: paid at DY0 -> Δ to DY3 -> Δ to DY6 -> Δ to DY9
    # -> Δ to DY15. The waterfall block expects measures that sum to a final
    # total; we reshape the row to columns Paid_at_DY0, Plus_DY3, Plus_DY6,
    # Plus_DY9, Plus_DY15.
    wf_pull = new_dm_pull_block(table = "loss",
      block_name = "Pull loss"),
    wf_sum = new_summarize_block(
      summaries = list(
        list(type = "expr", name = "DY0",
             expr = "sum(DY0, na.rm = TRUE)"),
        list(type = "expr", name = "DY3",
             expr = "sum(DY3, na.rm = TRUE)"),
        list(type = "expr", name = "DY6",
             expr = "sum(DY6, na.rm = TRUE)"),
        list(type = "expr", name = "DY9",
             expr = "sum(DY9, na.rm = TRUE)"),
        list(type = "expr", name = "DY15",
             expr = "sum(DY15, na.rm = TRUE)")
      ),
      by = list(),
      block_name = "Cumulative paid at DY 0/3/6/9/15"
    ),
    wf_mutate = new_mutate_block(
      mutations = list(
        list(name = "Paid_at_DY0",  expr = "DY0"),
        list(name = "Plus_DY0_to_DY3",  expr = "DY3 - DY0"),
        list(name = "Plus_DY3_to_DY6",  expr = "DY6 - DY3"),
        list(name = "Plus_DY6_to_DY9",  expr = "DY9 - DY6"),
        list(name = "Plus_DY9_to_DY15", expr = "DY15 - DY9")
      ),
      by = list(),
      block_name = "Bridge components"
    ),
    # The waterfall is now a chart_type on new_chart_block, which consumes a
    # LONG (step, value) bridge — one row per step, each value a delta. Pivot
    # the wide bridge-component columns into that shape; pivot_longer preserves
    # the listed column order, so the step axis keeps DY0 -> DY15 order.
    wf_long = new_pivot_longer_block(
      cols = list("Paid_at_DY0", "Plus_DY0_to_DY3", "Plus_DY3_to_DY6",
                  "Plus_DY6_to_DY9", "Plus_DY9_to_DY15"),
      names_to = "step",
      values_to = "value",
      values_drop_na = FALSE,
      names_prefix = "",
      block_name = "Bridge components (long form)"
    ),
    wf_chart = new_chart_block(
      chart_type = "waterfall",
      group      = "step",
      metric     = "value",
      agg_fn     = "sum",
      block_name = "Loss emergence (DY0 -> DY15)"
    ),

    # === TOP CLAIMS ===
    top_pull = new_dm_pull_block(table = "loss",
      block_name = "Pull loss"),
    top_filter = new_filter_block(
      conditions = list(list(type = "expr",
                              expr = "Total_Incurred > 50000")),
      operator = "&",
      block_name = "Large claims (Total_Incurred > 50K)"
    ),
    top_summary = new_summary_table_block(
      state = list(
        vars = c("Year", "Insurance_Company", "Vehicle_type", "Cover",
                 "Num_Claims", "Total_Incurred"),
        by = character()
      ),
      block_name = "Top claims (filtered list)"
    )
  ),

  links = links(
    from = c(
      "loss_read", "data",
      "global_filter", "kpi_pull", "kpi_sum", "kpi_mutate", "kpi_tile_sum",
      "global_filter", "sev_pull", "sev_sum",
      "global_filter", "trend_pull", "trend_sum", "trend_year_chr",
      "global_filter", "wf_pull", "wf_sum", "wf_mutate", "wf_long",
      "global_filter", "top_pull", "top_filter"
    ),
    to = c(
      "data", "global_filter",
      "kpi_pull", "kpi_sum", "kpi_mutate", "kpi_tile_sum", "kpi",
      "sev_pull", "sev_sum", "sev_drill",
      "trend_pull", "trend_sum", "trend_year_chr", "trend_drill",
      "wf_pull", "wf_sum", "wf_mutate", "wf_long", "wf_chart",
      "top_pull", "top_filter", "top_summary"
    ),
    input = c(
      "loss", "data",
      "data", "data", "data", "data", "data",
      "data", "data", "data",
      "data", "data", "data", "data",
      "data", "data", "data", "data", "data",
      "data", "data", "data"
    )
  ),

  extensions = list(
    blockr.dag::new_dag_extension()
  ),

  layouts = list(
    Setup = dock_layout(
      "loss_read", "data", "dag_extension",
      name = "Setup"
    ),
    KPIs = dock_layout(
      "global_filter",
      "kpi_pull", "kpi_sum", "kpi_mutate", "kpi_tile_sum", "kpi",
      name = "KPIs"
    ),
    `Drill-down` = dock_layout(
      "global_filter",
      "sev_pull", "sev_sum", "sev_drill",
      "trend_pull", "trend_sum", "trend_year_chr", "trend_drill",
      name = "Drill-down"
    ),
    Waterfall = dock_layout(
      "global_filter",
      "wf_pull", "wf_sum", "wf_mutate", "wf_long", "wf_chart",
      name = "Waterfall"
    ),
    Top_claims = dock_layout(
      "global_filter",
      "top_pull", "top_filter", "top_summary",
      name = "Top claims"
    )
  ),
  active = "Setup"
)

serve(board, plugins = custom_plugins(manage_project()))
