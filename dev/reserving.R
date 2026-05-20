# Motor reserving review POC — visualize the development triangle, derive
# age-to-age factors, project ultimate.
#
# Run from an R session:
#
#   pkgload::load_all("blockr.insurance")
#   source(system.file("examples", "reserving.R", package = "blockr.insurance"))
#
# Four workspaces (Setup / Triangle / Development factors / Ultimate)
# operate on the bundled `motor_losses` dataset, where each row has fully
# developed cumulative paid columns DY0..DY15. See `?motor_losses`.
#
# Story: an actuary reviewing the quarterly reserves wants to see the
# triangle, eyeball the development pattern, and confirm the chain-ladder
# ultimate. NOTE: this demo computes a *simple paid chain-ladder* (age-to-age
# factors via volume-weighted ratios). Mack, Bornhuetter-Ferguson, and tail
# fitting are out of scope here — the goal is the visual structure of the
# review, not full actuarial credibility.

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
pkgload::load_all("blockr.bi")
pkgload::load_all("blockr.echarts")
pkgload::load_all("blockr.session")
pkgload::load_all("blockr.dag")

# Helper to summarize sum(DY0..DY15) — used in both Triangle and Ultimate
# workspaces. Year goes in `by` for triangle (one row per origin year);
# omit `by` for the development-factor workspace (single grand-total row).
sum_dy <- function(by = "Year",
                   block_name = "Sum DY0..DY15") {
  new_summarize_block(
    state = list(
      summaries = lapply(0:15, function(i) {
        list(type = "expr",
             name = paste0("DY", i),
             expr = sprintf("sum(DY%d, na.rm = TRUE)", i))
      }),
      by = by
    ),
    block_name = block_name
  )
}

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
      block_name = "Loss dm"
    ),
    global_filter = new_crossfilter_block(
      active_dims = list(loss = c(
        "Insurance_Company", "Fleet", "Vehicle_type", "Cover",
        "Age_Class", "Gender"
      )),
      block_name = "Global filter (segment selection)"
    ),

    # === TRIANGLE VIEW ===
    tri_pull = new_dm_pull_block(table = "loss",
      block_name = "Pull loss"),
    tri_sum = sum_dy(by = "Year",
      block_name = "Triangle: sum DY0..DY15 by origin Year"),
    tri_long = new_pivot_longer_block(
      state = list(
        cols = paste0("DY", 0:15),
        names_to = "dev_period",
        values_to = "cumulative_paid",
        values_drop_na = FALSE,
        names_prefix = ""
      ),
      block_name = "Reshape wide -> long (Year x DY)"
    ),
    tri_mutate = new_mutate_block(
      state = list(
        mutations = list(
          list(name = "dev_year",
               expr = "as.integer(sub('^DY', '', dev_period))"),
          list(name = "origin_year",
               expr = "as.character(Year)")
        ),
        by = list()
      ),
      block_name = "Add dev_year (int) + origin_year (chr)"
    ),
    tri_heatmap = new_echart_heatmap_block(
      x = "dev_year",
      y = "origin_year",
      value = "cumulative_paid",
      title = "Loss development triangle",
      block_name = "Triangle heatmap"
    ),

    # === DEVELOPMENT FACTORS ===
    df_pull = new_dm_pull_block(table = "loss",
      block_name = "Pull loss"),
    df_sum = sum_dy(by = list(),
      block_name = "Grand-total triangle (single row)"),
    df_factors = new_mutate_block(
      state = list(
        mutations = lapply(0:14, function(k) {
          list(
            name = sprintf("f_%02d_to_%02d", k, k + 1),
            expr = sprintf("DY%d / DY%d", k + 1, k)
          )
        }),
        by = list()
      ),
      block_name = "Age-to-age factors (volume weighted)"
    ),
    df_select = new_select_block(
      state = list(
        columns = sprintf("f_%02d_to_%02d", 0:14, 1:15),
        exclude = FALSE,
        distinct = FALSE
      ),
      block_name = "Keep factor columns only"
    ),
    df_long = new_pivot_longer_block(
      state = list(
        cols = sprintf("f_%02d_to_%02d", 0:14, 1:15),
        names_to = "dev_step",
        values_to = "factor",
        values_drop_na = FALSE,
        names_prefix = ""
      ),
      block_name = "Reshape factors -> long"
    ),
    df_chart = new_drilldown_chart_block(
      chart_type = "bar",
      group_by   = "dev_step",
      metric     = "factor",
      agg_fn     = "sum",
      block_name = "Age-to-age factors chart"
    ),

    # === ULTIMATE / IBNR ===
    ult_pull = new_dm_pull_block(table = "loss",
      block_name = "Pull loss"),
    ult_sum = sum_dy(by = "Year",
      block_name = "Sum DY0..DY15 by origin Year"),
    ult_mutate = new_mutate_block(
      state = list(
        mutations = list(
          list(name = "Paid_DY0",         expr = "DY0"),
          list(name = "Paid_to_date",     expr = "DY15"),
          list(name = "Development_factor",
               expr = "DY15 / DY0"),
          # Implied IBNR if we'd only seen DY0 (synthetic — for demo).
          list(name = "Implied_IBNR_from_DY0",
               expr = "DY15 - DY0")
        ),
        by = list()
      ),
      block_name = "Per-year development metrics"
    ),
    ult_kpi = new_tile_block(
      showcase = "number",
      state = list(
        aesthetics = list(value = c("Paid_to_date",
                                    "Implied_IBNR_from_DY0")),
        stats = list(value = "sum"),
        formats = list(measure_labels = c(
          Paid_to_date          = "Total paid to date",
          Implied_IBNR_from_DY0 = "Loss emergence post-DY0"
        ))
      ),
      block_name = "Reserving KPIs"
    ),
    ult_table = new_summary_table_block(
      state = list(
        vars = c("Year", "Paid_DY0", "Paid_to_date",
                 "Development_factor", "Implied_IBNR_from_DY0"),
        by = character()
      ),
      block_name = "Per-origin-year reserve table"
    )
  ),

  links = links(
    from = c(
      "loss_read", "data",
      "global_filter", "tri_pull", "tri_sum", "tri_long", "tri_mutate",
      "global_filter", "df_pull", "df_sum", "df_factors", "df_select", "df_long",
      "global_filter", "ult_pull", "ult_sum", "ult_mutate", "ult_mutate"
    ),
    to = c(
      "data", "global_filter",
      "tri_pull", "tri_sum", "tri_long", "tri_mutate", "tri_heatmap",
      "df_pull", "df_sum", "df_factors", "df_select", "df_long", "df_chart",
      "ult_pull", "ult_sum", "ult_mutate", "ult_kpi", "ult_table"
    ),
    input = c(
      "loss", "data",
      "data", "data", "data", "data", "data",
      "data", "data", "data", "data", "data", "data",
      "data", "data", "data", "data", "data"
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
    Triangle = dock_view(
      "global_filter",
      "tri_pull", "tri_sum", "tri_long", "tri_mutate", "tri_heatmap"
    ),
    `Dev factors` = dock_view(
      "global_filter",
      "df_pull", "df_sum", "df_factors", "df_select", "df_long", "df_chart"
    ),
    Ultimate = dock_view(
      "global_filter",
      "ult_pull", "ult_sum", "ult_mutate", "ult_kpi", "ult_table"
    )
  )
)

serve(board, plugins = custom_plugins(manage_project()))
