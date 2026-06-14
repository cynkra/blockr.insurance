# Motor claims investigation POC — drill into anomalous claim activity by
# segment, vehicle, year, cohort.
#
# Run from an R session:
#
#   pkgload::load_all("blockr.insurance")
#   source(system.file("examples", "claims-investigation.R", package = "blockr.insurance"))
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
      state = list(
        summaries = list(
          list(type = "expr", name = "Num_Claims",
               expr = "sum(Num_Claims, na.rm = TRUE)"),
          list(type = "expr", name = "Total_Incurred",
               expr = "sum(Total_Incurred, na.rm = TRUE)")
        ),
        by = list()
      ),
      block_name = "Grand totals over filtered cube"
    ),
    kpi_mutate = new_mutate_block(
      state = list(
        mutations = list(
          list(name = "Avg_Severity",
               expr = "Total_Incurred / Num_Claims")
        ),
        by = list()
      ),
      block_name = "Avg_Severity = Total_Incurred / Num_Claims"
    ),
    kpi = new_tile_block(
      showcase = "number",
      state = list(
        aesthetics = list(value = c("Num_Claims", "Total_Incurred",
                                    "Avg_Severity")),
        stats = list(value = "sum"),
        formats = list(measure_labels = c(
          Num_Claims     = "Total claims",
          Total_Incurred = "Total incurred",
          Avg_Severity   = "Avg severity"
        ))
      ),
      block_name = "Claims headline KPIs"
    ),

    # === DRILL-DOWN: severity by vehicle type & cover ===
    sev_pull = new_dm_pull_block(table = "loss",
      block_name = "Pull loss"),
    sev_sum = new_summarize_block(
      state = list(
        summaries = list(
          list(type = "expr", name = "Num_Claims",
               expr = "sum(Num_Claims, na.rm = TRUE)"),
          list(type = "expr", name = "Total_Incurred",
               expr = "sum(Total_Incurred, na.rm = TRUE)")
        ),
        by = c("Vehicle_type", "Cover")
      ),
      block_name = "Sum by Vehicle x Cover"
    ),
    sev_drill = new_chart_block(
      chart_type = "bar",
      group_by   = "Vehicle_type",
      color_by   = "Cover",
      metric     = "Total_Incurred",
      agg_fn     = "sum",
      block_name = "Total incurred by Vehicle x Cover"
    ),

    # === DRILL-DOWN: frequency / severity over time, per insurer ===
    trend_pull = new_dm_pull_block(table = "loss",
      block_name = "Pull loss"),
    trend_sum = new_summarize_block(
      state = list(
        summaries = list(
          list(type = "expr", name = "Num_Claims",
               expr = "sum(Num_Claims, na.rm = TRUE)"),
          list(type = "expr", name = "Total_Incurred",
               expr = "sum(Total_Incurred, na.rm = TRUE)")
        ),
        by = c("Year", "Insurance_Company")
      ),
      block_name = "Sum by Year x Company"
    ),
    trend_year_chr = new_mutate_block(
      state = list(
        mutations = list(list(name = "Year",
                              expr = "as.character(Year)")),
        by = list()
      ),
      block_name = "Year -> character (categorical X axis)"
    ),
    trend_drill = new_chart_block(
      chart_type = "line",
      x_col      = "Year",
      y_col      = "Total_Incurred",
      series_by  = "Insurance_Company",
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
      state = list(
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
        by = list()
      ),
      block_name = "Cumulative paid at DY 0/3/6/9/15"
    ),
    wf_mutate = new_mutate_block(
      state = list(
        mutations = list(
          list(name = "Paid_at_DY0",  expr = "DY0"),
          list(name = "Plus_DY0_to_DY3",  expr = "DY3 - DY0"),
          list(name = "Plus_DY3_to_DY6",  expr = "DY6 - DY3"),
          list(name = "Plus_DY6_to_DY9",  expr = "DY9 - DY6"),
          list(name = "Plus_DY9_to_DY15", expr = "DY15 - DY9")
        ),
        by = list()
      ),
      block_name = "Bridge components"
    ),
    wf_chart = new_waterfall_block(
      measures = c("Paid_at_DY0", "Plus_DY0_to_DY3", "Plus_DY3_to_DY6",
                   "Plus_DY6_to_DY9", "Plus_DY9_to_DY15"),
      block_name = "Loss emergence (DY0 -> DY15)"
    ),

    # === TOP CLAIMS ===
    top_pull = new_dm_pull_block(table = "loss",
      block_name = "Pull loss"),
    top_filter = new_filter_block(
      state = list(
        conditions = list(list(type = "expr",
                                expr = "Total_Incurred > 50000")),
        operator = "&"
      ),
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
      "global_filter", "kpi_pull", "kpi_sum", "kpi_mutate",
      "global_filter", "sev_pull", "sev_sum",
      "global_filter", "trend_pull", "trend_sum", "trend_year_chr",
      "global_filter", "wf_pull", "wf_sum", "wf_mutate",
      "global_filter", "top_pull", "top_filter"
    ),
    to = c(
      "data", "global_filter",
      "kpi_pull", "kpi_sum", "kpi_mutate", "kpi",
      "sev_pull", "sev_sum", "sev_drill",
      "trend_pull", "trend_sum", "trend_year_chr", "trend_drill",
      "wf_pull", "wf_sum", "wf_mutate", "wf_chart",
      "top_pull", "top_filter", "top_summary"
    ),
    input = c(
      "loss", "data",
      "data", "data", "data", "data",
      "data", "data", "data",
      "data", "data", "data", "data",
      "data", "data", "data", "data",
      "data", "data", "data"
    )
  ),

  extensions = list(
    blockr.dag::new_dag_extension()
  ),

  layout = dock_layouts(
    Setup = dock_view(
      "loss_read", "data", "dag_extension",
      active = TRUE
    ),
    KPIs = dock_view(
      "global_filter",
      "kpi_pull", "kpi_sum", "kpi_mutate", "kpi"
    ),
    `Drill-down` = dock_view(
      "global_filter",
      "sev_pull", "sev_sum", "sev_drill",
      "trend_pull", "trend_sum", "trend_year_chr", "trend_drill"
    ),
    Waterfall = dock_view(
      "global_filter",
      "wf_pull", "wf_sum", "wf_mutate", "wf_chart"
    ),
    `Top claims` = dock_view(
      "global_filter",
      "top_pull", "top_filter", "top_summary"
    )
  )
)

serve(board, plugins = custom_plugins(manage_project()))
