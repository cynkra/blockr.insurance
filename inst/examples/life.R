# Life-insurance POC — multi-page mortality A/E dashboard.
#
# Run from an R session after installing blockr.insurance:
#
#   library(blockr.insurance)
#   source(system.file("examples", "life.R", package = "blockr.insurance"))
#
# Five workspaces (Setup / Mortality / Trend / Underwriting / Face_Amount)
# operate on the bundled `ilec_mortality` cube — SOA ILEC 2013-2017
# individual life experience, with `amount_actual` / `amount_2015vbt` and
# `policy_actual` / `policy_2015vbt` (VBT 2015 expected). The headline
# actuarial KPI is the actual-to-expected ratio. See `?ilec_mortality`.

options(
  blockr.dock_is_locked = FALSE,
  blockr.eval_parent_env = asNamespace("stats"),
  blockr.html_table_preview = TRUE,
  blockr.session_url_params = TRUE,
  blockr.lazy_eval = FALSE
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

# Convenience: identical mutate that adds the two A/E ratios from the four
# summary columns. Used downstream of every summarize block in this demo.
ae_mutate <- function(name = "Compute A/E ratios") {
  new_mutate_block(
    state = list(
      mutations = list(
        list(name = "AE_Amount",
             expr = "amount_actual / amount_2015vbt"),
        list(name = "AE_Count",
             expr = "policy_actual / policy_2015vbt")
      ),
      by = list()
    ),
    block_name = name
  )
}

board <- new_dock_board(
  blocks = c(

    # === SHARED DATA ===
    ilec_read = new_dataset_block(
      dataset    = "ilec_mortality",
      package    = "blockr.insurance",
      block_name = "ilec_mortality"
    ),
    data = new_dm_block(
      infer_keys = FALSE,
      block_name = "Mortality dm (single table: ilec)"
    ),

    global_filter = new_crossfilter_block(
      active_dims = list(ilec = c(
        "uw", "gender", "insurance_plan", "ltp",
        "ia_band1", "dur_band1", "iy_band1", "face_amount_band"
      )),
      block_name = "Global filter (mortality dimensions)"
    ),

    # === MORTALITY OVERVIEW (grand-total A/E + age-band breakdown) ===
    mort_pull = new_dm_pull_block(table = "ilec",
      block_name = "Pull ilec"),
    mort_total = new_summarize_block(
      state = list(
        summaries = list(
          list(type = "expr", name = "amount_actual",
               expr = "sum(amount_actual, na.rm = TRUE)"),
          list(type = "expr", name = "amount_2015vbt",
               expr = "sum(amount_2015vbt, na.rm = TRUE)"),
          list(type = "expr", name = "policy_actual",
               expr = "sum(policy_actual, na.rm = TRUE)"),
          list(type = "expr", name = "policy_2015vbt",
               expr = "sum(policy_2015vbt, na.rm = TRUE)")
        ),
        by = list()
      ),
      block_name = "Grand totals over filtered cube"
    ),
    mort_ae = ae_mutate("AE_Amount, AE_Count (one row)"),
    mort_kpi = new_kpi_block(
      measures = c("AE_Amount", "AE_Count"),
      agg_fun  = "sum",
      titles   = c(AE_Amount = "A/E by Amount",
                   AE_Count  = "A/E by Count"),
      block_name = "Mortality A/E"
    ),

    # === MORTALITY BY AGE BAND ===
    age_pull = new_dm_pull_block(table = "ilec",
      block_name = "Pull ilec"),
    age_sum = new_summarize_block(
      state = list(
        summaries = list(
          list(type = "expr", name = "amount_actual",
               expr = "sum(amount_actual, na.rm = TRUE)"),
          list(type = "expr", name = "amount_2015vbt",
               expr = "sum(amount_2015vbt, na.rm = TRUE)"),
          list(type = "expr", name = "policy_actual",
               expr = "sum(policy_actual, na.rm = TRUE)"),
          list(type = "expr", name = "policy_2015vbt",
               expr = "sum(policy_2015vbt, na.rm = TRUE)")
        ),
        by = "ia_band1"
      ),
      block_name = "Sum by issue-age band"
    ),
    age_ae   = ae_mutate("Add A/E columns"),
    age_drill = new_drilldown_chart_block(
      chart_type = "bar",
      group_by = "ia_band1",
      metric   = "AE_Amount",
      agg_fn   = "sum",
      block_name = "A/E by issue-age band"
    ),

    # === TREND (A/E over observation year, per insurance plan) ===
    trend_pull = new_dm_pull_block(table = "ilec",
      block_name = "Pull ilec"),
    trend_sum = new_summarize_block(
      state = list(
        summaries = list(
          list(type = "expr", name = "amount_actual",
               expr = "sum(amount_actual, na.rm = TRUE)"),
          list(type = "expr", name = "amount_2015vbt",
               expr = "sum(amount_2015vbt, na.rm = TRUE)"),
          list(type = "expr", name = "policy_actual",
               expr = "sum(policy_actual, na.rm = TRUE)"),
          list(type = "expr", name = "policy_2015vbt",
               expr = "sum(policy_2015vbt, na.rm = TRUE)")
        ),
        by = c("observation_year", "insurance_plan")
      ),
      block_name = "Sum by Year x Plan"
    ),
    trend_ae   = ae_mutate("Add A/E columns"),
    trend_year_chr = new_mutate_block(
      state = list(
        mutations = list(list(name = "observation_year",
                              expr = "as.character(observation_year)")),
        by = list()
      ),
      block_name = "Year -> character (categorical X axis)"
    ),
    trend_drill = new_drilldown_chart_block(
      chart_type = "line",
      x_col     = "observation_year",
      y_col     = "AE_Amount",
      series_by = "insurance_plan",
      block_name = "A/E by Amount over time, per plan"
    ),

    # === UNDERWRITING (preferred-class wear-off — A/E by uw class) ===
    uw_pull = new_dm_pull_block(table = "ilec",
      block_name = "Pull ilec"),
    uw_sum = new_summarize_block(
      state = list(
        summaries = list(
          list(type = "expr", name = "amount_actual",
               expr = "sum(amount_actual, na.rm = TRUE)"),
          list(type = "expr", name = "amount_2015vbt",
               expr = "sum(amount_2015vbt, na.rm = TRUE)"),
          list(type = "expr", name = "policy_actual",
               expr = "sum(policy_actual, na.rm = TRUE)"),
          list(type = "expr", name = "policy_2015vbt",
               expr = "sum(policy_2015vbt, na.rm = TRUE)")
        ),
        by = c("uw", "dur_band1")
      ),
      block_name = "Sum by UW class x Duration band"
    ),
    uw_ae   = ae_mutate("Add A/E columns"),
    uw_drill = new_drilldown_chart_block(
      chart_type = "bar",
      group_by  = "uw",
      color_by  = "dur_band1",
      metric    = "AE_Amount",
      agg_fn    = "sum",
      block_name = "A/E by UW class, split by duration"
    ),

    # === FACE AMOUNT (anti-selection — A/E by face_amount_band) ===
    fa_pull = new_dm_pull_block(table = "ilec",
      block_name = "Pull ilec"),
    fa_sum = new_summarize_block(
      state = list(
        summaries = list(
          list(type = "expr", name = "amount_actual",
               expr = "sum(amount_actual, na.rm = TRUE)"),
          list(type = "expr", name = "amount_2015vbt",
               expr = "sum(amount_2015vbt, na.rm = TRUE)"),
          list(type = "expr", name = "policy_actual",
               expr = "sum(policy_actual, na.rm = TRUE)"),
          list(type = "expr", name = "policy_2015vbt",
               expr = "sum(policy_2015vbt, na.rm = TRUE)")
        ),
        by = c("face_amount_band", "gender")
      ),
      block_name = "Sum by Face-amount x Gender"
    ),
    fa_ae   = ae_mutate("Add A/E columns"),
    fa_drill = new_drilldown_chart_block(
      chart_type = "bar",
      group_by  = "face_amount_band",
      color_by  = "gender",
      metric    = "AE_Amount",
      agg_fn    = "sum",
      block_name = "A/E by Face Amount, split by Gender"
    )
  ),

  links = links(
    from = c(
      "ilec_read", "data",
      "global_filter", "mort_pull",  "mort_total", "mort_ae",
      "global_filter", "age_pull",   "age_sum",    "age_ae",
      "global_filter", "trend_pull", "trend_sum",  "trend_ae", "trend_year_chr",
      "global_filter", "uw_pull",    "uw_sum",     "uw_ae",
      "global_filter", "fa_pull",    "fa_sum",     "fa_ae"
    ),
    to = c(
      "data", "global_filter",
      "mort_pull",  "mort_total", "mort_ae",   "mort_kpi",
      "age_pull",   "age_sum",    "age_ae",    "age_drill",
      "trend_pull", "trend_sum",  "trend_ae",  "trend_year_chr", "trend_drill",
      "uw_pull",    "uw_sum",     "uw_ae",     "uw_drill",
      "fa_pull",    "fa_sum",     "fa_ae",     "fa_drill"
    ),
    input = c(
      "ilec", "data",
      "data", "data", "data", "data",
      "data", "data", "data", "data",
      "data", "data", "data", "data", "data",
      "data", "data", "data", "data",
      "data", "data", "data", "data"
    )
  ),

  extensions = list(
    blockr.dag::new_dag_extension()
  ),

  layout = dock_layouts(
    Setup = dock_view(
      "ilec_read", "data", "dag_extension",
      active = TRUE
    ),
    Mortality = dock_view(
      "global_filter",
      "mort_pull", "mort_total", "mort_ae", "mort_kpi",
      "age_pull",  "age_sum",    "age_ae",  "age_drill"
    ),
    Trend = dock_view(
      "global_filter",
      "trend_pull", "trend_sum", "trend_ae",
      "trend_year_chr", "trend_drill"
    ),
    Underwriting = dock_view(
      "global_filter", "uw_pull", "uw_sum", "uw_ae", "uw_drill"
    ),
    Face_Amount = dock_view(
      "global_filter", "fa_pull", "fa_sum", "fa_ae", "fa_drill"
    )
  )
)

serve(board, plugins = custom_plugins(manage_project()))
