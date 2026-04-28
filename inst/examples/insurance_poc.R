# Insurance POC — multi-page motor-insurance dashboard.
#
# Run from an R session after installing blockr.insurance:
#
#   library(blockr.insurance)
#   source(system.file("examples", "insurance_poc.R", package = "blockr.insurance"))
#
# Five workspaces (Setup / Portfolio / Profitability / Claims / Reserving)
# operate on the bundled `motor_portfolio` and `motor_losses` datasets. The
# data is built from `insuranceData::dataCar` and `ChainLadder::MW2014` by
# `data-raw/build.R`. See `?motor_portfolio`, `?motor_losses`.

options(
  blockr.dock_is_locked = FALSE,
  blockr.eval_parent_env = asNamespace("stats"),
  blockr.html_table_preview = TRUE,
  blockr.session_url_params = TRUE
)

library(blockr.core)
library(blockr.dock)
library(blockr.dm)
library(blockr.dplyr)
library(blockr.io)
library(blockr.sandbox)
library(blockr.extra)
library(blockr.bi)
library(blockr.session)
library(blockr.dag)

board <- new_dock_board(
  blocks = c(

    # === SHARED DATA ===
    profiles_read = new_dataset_block(
      dataset  = "motor_portfolio",
      package  = "blockr.insurance",
      block_name = "motor_portfolio"
    ),
    loss_read = new_dataset_block(
      dataset  = "motor_losses",
      package  = "blockr.insurance",
      block_name = "motor_losses"
    ),
    data = new_dm_block(
      infer_keys = FALSE,
      block_name = "Insurance dm (portfolio + losses)"
    ),

    global_filter = new_crossfilter_block(
      active_dims = list(profiles = c(
        "Insurance_Company", "Fleet", "Vehicle_type",
        "Cover", "Age_Class", "Gender"
      )),
      block_name = "Global filter (segmentation)"
    ),

    # === PORTFOLIO OVERVIEW ===
    ov_pull = new_dm_pull_block(table = "profiles",
      block_name = "Pull profiles"),
    ov_nonzero = new_filter_block(
      state = list(
        conditions = list(list(type = "expr", expr = "Vehicles > 0")),
        operator = "&"
      ),
      block_name = "Drop empty cells"
    ),
    ov_avg = new_mutate_block(
      state = list(
        mutations = list(list(name = "Avg_Premium", expr = "Premium / Vehicles")),
        by = list()
      ),
      block_name = "Avg_Premium = Premium / Vehicles"
    ),
    ov_kpi = new_kpi_block(
      measures = c("Premium", "Vehicles"),
      agg_fun = "sum",
      titles = c(Premium = "Total Premium", Vehicles = "Total Vehicles"),
      block_name = "Portfolio KPIs"
    ),
    ov_drill = new_drilldown_chart_block(
      chart_type = "bar",
      group_by = "Vehicle_type",
      color_by = "Cover",
      metric = "Premium",
      agg_fn = "sum",
      block_name = "Premium by Vehicle Type x Cover"
    ),

    # === PROFITABILITY (Premium evolution per insurer) ===
    # NOTE: a full Loss Ratio chart would join premium + loss tables.
    # join_block's JS UI clobbers the constructor `keys` state on first
    # render, so the join silently passes through `x`. v1 ships with the
    # premium-only narrative; loss-ratio is deferred until either a custom
    # multi-input transform is wired up, or join_block is patched to
    # honour constructor state on init.
    prof_premium_pull = new_dm_pull_block(table = "profiles",
      block_name = "Pull profiles"),
    prof_premium_sum = new_summarize_block(
      state = list(
        summaries = list(
          list(type = "expr", name = "Total_Premium",
               expr = "sum(Premium, na.rm = TRUE)"),
          list(type = "expr", name = "Total_Vehicles",
               expr = "sum(Vehicles, na.rm = TRUE)")
        ),
        by = c("Year", "Insurance_Company")
      ),
      block_name = "Premium by Year x Company"
    ),
    prof_nonzero = new_filter_block(
      state = list(
        conditions = list(list(type = "expr",
                                expr = "Total_Premium > 0")),
        operator = "&"
      ),
      block_name = "Drop empty insurer-years"
    ),
    prof_year_chr = new_mutate_block(
      state = list(
        mutations = list(list(name = "Year",
                              expr = "as.character(Year)")),
        by = list()
      ),
      block_name = "Year -> character (categorical X axis)"
    ),
    prof_drill = new_drilldown_chart_block(
      chart_type = "line",
      x_col = "Year",
      y_col = "Total_Premium",
      series_by = "Insurance_Company",
      block_name = "Premium over time, per insurer"
    ),

    # === LARGE CLAIMS ===
    claims_pull = new_dm_pull_block(table = "loss",
      block_name = "Pull loss"),
    claims_filter = new_filter_block(
      state = list(
        conditions = list(list(type = "expr",
                                expr = "Latest > 10000")),
        operator = "&"
      ),
      block_name = "Large claims (Latest > 10K)"
    ),
    claims_drill = new_drilldown_chart_block(
      chart_type = "bar",
      group_by = "Year",
      color_by = "Vehicle_type",
      metric = "Latest",
      agg_fn = "sum",
      block_name = "Large claims by Year x Vehicle Type"
    ),

    # === RESERVING — Development triangle ===
    tri_pull = new_dm_pull_block(table = "loss",
      block_name = "Pull loss"),
    tri_select = new_select_block(
      state = list(
        columns = c("Year", paste0("DY", 0:15)),
        exclude = FALSE,
        distinct = FALSE
      ),
      block_name = "Select triangle columns"
    ),
    tri_summary = new_summarize_block(
      state = list(
        summaries = lapply(0:15, function(i) {
          list(type = "expr",
               name = paste0("DY", i),
               expr = sprintf("sum(DY%d, na.rm = TRUE)", i))
        }),
        by = "Year"
      ),
      block_name = "Development triangle (Year x DY)"
    )
    # NOTE: a fancier renderer (gt with triangle styling) is a v1.5 task —
    # gt_table_block expects summary_table_block columns (label, depth);
    # the summarize preview is the triangle for now.
  ),

  links = links(
    from = c(
      "profiles_read", "loss_read", "data",
      "global_filter", "ov_pull", "ov_nonzero", "ov_avg", "ov_avg",
      "global_filter", "prof_premium_pull", "prof_premium_sum",
      "prof_nonzero", "prof_year_chr",
      "global_filter", "claims_pull", "claims_filter",
      "global_filter", "tri_pull", "tri_select"
    ),
    to = c(
      "data", "data", "global_filter",
      "ov_pull", "ov_nonzero", "ov_avg", "ov_kpi", "ov_drill",
      "prof_premium_pull", "prof_premium_sum", "prof_nonzero",
      "prof_year_chr", "prof_drill",
      "claims_pull", "claims_filter", "claims_drill",
      "tri_pull", "tri_select", "tri_summary"
    ),
    input = c(
      "profiles", "loss", "data",
      "data", "data", "data", "data", "data",
      "data", "data", "data", "data", "data",
      "data", "data", "data",
      "data", "data", "data"
    )
  ),

  extensions = list(
    blockr.dag::new_dag_extension()
  ),

  layout = dock_workspaces(
    Setup = dock_workspace(
      layout = list("profiles_read", "loss_read", "data", "dag_extension")
    ),
    Portfolio = dock_workspace(
      layout = list(
        "global_filter", "ov_pull", "ov_nonzero", "ov_avg",
        "ov_kpi", "ov_drill"
      )
    ),
    Profitability = dock_workspace(
      layout = list(
        "global_filter",
        "prof_premium_pull", "prof_premium_sum", "prof_nonzero",
        "prof_year_chr", "prof_drill"
      )
    ),
    Claims = dock_workspace(
      layout = list(
        "global_filter", "claims_pull", "claims_filter", "claims_drill"
      )
    ),
    Reserving = dock_workspace(
      layout = list(
        "global_filter", "tri_pull", "tri_select", "tri_summary"
      )
    )
  )
)

serve(board, plugins = custom_plugins(manage_project()))
