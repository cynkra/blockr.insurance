# Life-underwriting demo, workspace 3: per-coverage expected-claims
# pipeline + UWR price-driver dashboard.
#
# Mirrors Antoine's coverage workflow with the URW pricing flow: the team
# prices ONE policy at a time. The census upload is every employee covered
# by that single submission, broken into three coverages (Death,
# Disability, CriticalIllness). UW factors live per coverage_type — just
# three rows in the worksheet, one per coverage.
#
#   coverages -> join uw_factors -> mutate age
#             -> join incidence_by_coverage
#             -> join country_adjustment -> join annuity_2pct
#             -> formula chain -> mutate age_band
#             -> dm + crossfilter (Drivers tab)
#             -> KPI tiles + 4 drilldown charts (price drivers)
#
# The UWR's worksheet (grid block) edits `uw_factors` — 3 rows (one per
# coverage_type), 3 editable columns. Demographics, sums at risk and
# individual additive_rate are inputs, not UWR-edited here.
#
# From /workspace:
#   Rscript blockr.insurance/dev/life-underwriting-ws3.R
# then open http://127.0.0.1:3838/

options(
  blockr.dock_is_locked       = FALSE,
  blockr.eval_parent_env      = asNamespace("stats"),
  blockr.html_table_preview   = TRUE,
  blockr.session_url_params   = TRUE,
  blockr.lazy_eval            = FALSE
)

pkgload::load_all("blockr.core")
pkgload::load_all("blockr.dock")
pkgload::load_all("blockr.dplyr")
pkgload::load_all("blockr.dm")
pkgload::load_all("blockr.bi")
pkgload::load_all("blockr.dag")
pkgload::load_all("blockr.input")
pkgload::load_all("blockr.insurance")

# Valuation date drives the age calculation. Edit via the cogwheel of the
# `age_calc` block in the Setup tab.
valuation_date <- "2026-05-11"

# Same summarize three-measure shape, reused across every chart path.
sum_by <- function(by, block_name = paste("Sum by", paste(by, collapse = " x "))) {
  new_summarize_block(
    state = list(
      summaries = list(
        list(type = "expr", name = "Expected_Claims",
             expr = "sum(Expected_Claims, na.rm = TRUE)"),
        list(type = "expr", name = "Expected_Claims_Raw",
             expr = "sum(Expected_Claims_Raw, na.rm = TRUE)"),
        list(type = "expr", name = "Sum_at_Risk",
             expr = "sum(sum_at_risk, na.rm = TRUE)")
      ),
      by = by
    ),
    block_name = block_name
  )
}

board <- new_dock_board(
  blocks = c(

    # === INPUTS ===
    coverages_src = new_dataset_block(
      dataset    = "coverages",
      package    = "blockr.insurance",
      block_name = "Coverages (person x coverage)"
    ),
    uw_factors_src = new_dataset_block(
      dataset    = "uw_factors",
      package    = "blockr.insurance",
      block_name = "UW factors (policy x coverage)"
    ),
    incidence_src = new_dataset_block(
      dataset    = "incidence_by_coverage",
      package    = "blockr.insurance",
      block_name = "Incidence by coverage"
    ),
    country_src = new_dataset_block(
      dataset    = "country_adjustment",
      package    = "blockr.insurance",
      block_name = "Country adjustment"
    ),
    annuity_src = new_dataset_block(
      dataset    = "annuity_2pct",
      package    = "blockr.insurance",
      block_name = "Annuity (capitalization factor)"
    ),

    # === UWR EDITING ===
    uwr_edit = new_grid_block(
      state = list(key_col = "coverage_type"),
      block_name = "Underwriter worksheet (per coverage)"
    ),

    # === PIPELINE: JOIN UW + AGE + JOINS + FORMULA ===
    j_uw = new_join_block(
      state = list(
        type = "left_join",
        keys = list(
          list(xCol = "coverage_type", op = "==", yCol = "coverage_type")
        ),
        exprs = list(), suffix_x = ".x", suffix_y = ".y"
      ),
      block_name = "Join UW factors"
    ),
    age_calc = new_mutate_block(
      state = list(
        mutations = list(
          list(name = "age",
               expr = sprintf(
                 "as.integer(as.numeric(as.Date('%s') - dob) / 365.25)",
                 valuation_date
               ))
        ),
        by = list()
      ),
      block_name = "Age at valuation date"
    ),
    j_incidence = new_join_block(
      state = list(
        type = "left_join",
        keys = list(
          list(xCol = "coverage_type", op = "==", yCol = "coverage_type"),
          list(xCol = "age",           op = "==", yCol = "age"),
          list(xCol = "sex",           op = "==", yCol = "sex"),
          list(xCol = "smoker",        op = "==", yCol = "smoker")
        ),
        exprs = list(), suffix_x = ".x", suffix_y = ".y"
      ),
      block_name = "Join incidence (coverage x age x sex x smoker)"
    ),
    j_country = new_join_block(
      state = list(
        type = "left_join",
        keys = list(list(xCol = "country", op = "==", yCol = "country")),
        exprs = list(), suffix_x = ".x", suffix_y = ".y"
      ),
      block_name = "Join country adjustment"
    ),
    j_annuity = new_join_block(
      state = list(
        type = "left_join",
        keys = list(
          list(xCol = "age", op = "==", yCol = "age"),
          list(xCol = "sex", op = "==", yCol = "sex")
        ),
        exprs = list(), suffix_x = ".x", suffix_y = ".y"
      ),
      block_name = "Join annuity (capitalization factor)"
    ),
    formula = new_mutate_block(
      state = list(
        mutations = list(
          list(name = "SI_Capitalized",
               expr = "sum_at_risk * capitalization_factor"),
          list(name = "Prob_Event_Raw",
               expr = "incidence_rate + additive_rate"),
          list(name = "UW_adjustment1",
               expr = "uw_health * uw_occupation * uw_hobby"),
          list(name = "Probability_Event",
               expr = "Prob_Event_Raw * adjustment * UW_adjustment1"),
          list(name = "Expected_Claims",
               expr = "Probability_Event * SI_Capitalized"),
          list(name = "Expected_Claims_Raw",
               expr = "Prob_Event_Raw * SI_Capitalized")
        ),
        by = list()
      ),
      block_name = "Expected claims (per person x coverage)"
    ),
    age_band = new_mutate_block(
      state = list(
        mutations = list(
          list(name = "age_band",
               expr = paste(
                 "cut(age,",
                 "breaks = c(-Inf, 29, 44, 59, 74, Inf),",
                 "labels = c('18-29', '30-44', '45-59', '60-74', '75+'),",
                 "ordered_result = TRUE)"
               ))
        ),
        by = list()
      ),
      block_name = "Age band"
    ),

    # === DM + CROSSFILTER ===
    data = new_dm_block(
      infer_keys = FALSE,
      block_name = "Claims dm (single table)"
    ),
    global_filter = new_crossfilter_block(
      active_dims = list(claims = c(
        "coverage_type", "sex", "smoker", "country", "age_band"
      )),
      block_name = "Global filter (UWR-controlled)"
    ),

    # === KPI: per-coverage totals (filter-reactive) ===
    kpi_pull = new_dm_pull_block(table = "claims",
                                 block_name = "Pull claims"),
    kpi_sum  = sum_by("coverage_type", block_name = "Sum by coverage"),
    kpi = new_tile_block(
      showcase = "number",
      state = list(
        aesthetics = list(
          value = c("Expected_Claims", "Expected_Claims_Raw", "Sum_at_Risk"),
          rows  = "coverage_type"
        ),
        stats   = list(value = "sum"),
        formats = list(measure_labels = c(
          Expected_Claims     = "Expected claims (full UW)",
          Expected_Claims_Raw = "Expected claims (raw)",
          Sum_at_Risk         = "Sum at risk"
        ))
      ),
      block_name = "Coverage KPIs"
    ),

    # === DRIVER CHARTS ===
    # 1. Expected claims by age band, split by coverage.
    age_pull = new_dm_pull_block(table = "claims",
                                 block_name = "Pull claims"),
    age_sum  = sum_by(c("age_band", "coverage_type")),
    age_chart = new_drilldown_chart_block(
      chart_type = "bar",
      group_by = "age_band",
      color_by = "coverage_type",
      metric   = "Expected_Claims",
      agg_fn   = "sum",
      block_name = "Expected claims by age band x coverage"
    ),

    # 2. Expected claims by sex, color by smoker.
    ssm_pull = new_dm_pull_block(table = "claims",
                                 block_name = "Pull claims"),
    ssm_sum  = sum_by(c("sex", "smoker")),
    ssm_chart = new_drilldown_chart_block(
      chart_type = "bar",
      group_by = "sex",
      color_by = "smoker",
      metric   = "Expected_Claims",
      agg_fn   = "sum",
      block_name = "Expected claims by sex x smoker"
    ),

    # 3. Expected claims by country, split by coverage.
    cty_pull = new_dm_pull_block(table = "claims",
                                 block_name = "Pull claims"),
    cty_sum  = sum_by(c("country", "coverage_type")),
    cty_chart = new_drilldown_chart_block(
      chart_type = "bar",
      group_by = "country",
      color_by = "coverage_type",
      metric   = "Expected_Claims",
      agg_fn   = "sum",
      block_name = "Expected claims by country x coverage"
    ),

    # 4. Top lives by expected claims — concentration analysis. With one
    # policy, the UWR's "where is the cost?" lens is per-employee, not
    # per-policy. These are the 15 individuals driving the most claim cost.
    pol_pull = new_dm_pull_block(table = "claims",
                                 block_name = "Pull claims"),
    pol_sum  = sum_by("person_id"),
    pol_top  = new_arrange_block(
      state = list(columns = list(
        list(column = "Expected_Claims", direction = "desc")
      )),
      block_name = "Sort by expected claims desc"
    ),
    pol_head = new_head_block(n = 15L,
                              block_name = "Top 15 lives"),
    pol_chart = new_drilldown_chart_block(
      chart_type = "bar",
      group_by = "person_id",
      metric   = "Expected_Claims",
      agg_fn   = "sum",
      block_name = "Top lives by expected claims"
    ),

    # 5. UW impact: full-UW vs raw (shows what UW assessment is changing).
    uw_pull = new_dm_pull_block(table = "claims",
                                block_name = "Pull claims"),
    uw_sum  = sum_by("coverage_type"),
    uw_chart = new_drilldown_chart_block(
      chart_type = "bar",
      group_by = "coverage_type",
      metric   = "Expected_Claims",
      agg_fn   = "sum",
      block_name = "Full UW vs raw — per coverage"
    ),

    # Per-row preview of the post-formula data.
    preview_pull = new_dm_pull_block(table = "claims",
                                     block_name = "Pull claims"),
    preview = new_head_block(n = 12L, block_name = "Per-row preview"),

    # === PRICING: Expected claims -> Gross premium build-up ===
    # The UWR's bottom line. Surcharges shown here are demo loadings;
    # edit them via the cogwheel of the `premium_calc` block.
    #   Safety margin  : 5%  of Expected_Claims  (uncertainty load)
    #   Expense        : 12% of Net_Premium      (admin / IT / overhead)
    #   Commission     : 8%  of Net_Premium      (broker / distribution)
    #   Profit margin  : 5%  of Net_Premium      (target return)
    #
    # Cumulative columns (Net_Premium, After_Expense, After_Commission,
    # Gross_Premium) feed the waterfall block, which renders the build-up
    # as floating bars from Expected_Claims to Gross_Premium.
    pricing_pull = new_dm_pull_block(table = "claims",
                                     block_name = "Pull claims"),
    pricing_sum = new_summarize_block(
      state = list(
        summaries = list(
          list(type = "expr", name = "Expected_Claims",
               expr = "sum(Expected_Claims, na.rm = TRUE)")
        ),
        by = list()
      ),
      block_name = "Total expected claims"
    ),
    premium_calc = new_mutate_block(
      state = list(
        mutations = list(
          list(name = "Net_Premium",
               expr = "Expected_Claims * 1.05"),
          list(name = "After_Expense",
               expr = "Net_Premium + Net_Premium * 0.12"),
          list(name = "After_Commission",
               expr = "After_Expense + Net_Premium * 0.08"),
          list(name = "Gross_Premium",
               expr = "After_Commission + Net_Premium * 0.05")
        ),
        by = list()
      ),
      block_name = "Premium build-up (surcharges)"
    ),
    premium_kpi = new_tile_block(
      showcase = "number",
      state = list(
        aesthetics = list(
          value = c("Expected_Claims", "Net_Premium", "Gross_Premium")
        ),
        stats   = list(value = "sum"),
        formats = list(measure_labels = c(
          Expected_Claims = "Expected claims (full UW)",
          Net_Premium     = "Net premium (+ safety margin)",
          Gross_Premium   = "Gross premium (quoted price)"
        ))
      ),
      block_name = "Premium headline"
    ),
    premium_waterfall = new_waterfall_block(
      measures = c("Expected_Claims", "Net_Premium",
                   "After_Expense", "After_Commission", "Gross_Premium"),
      block_name = "Premium build-up — Expected claims to Gross premium"
    )
  ),

  links = c(
    # UWR edits uw_factors via the grid block.
    new_link("uw_factors_src", "uwr_edit", "data"),

    # coverages x edited uw_factors -> aged -> joins -> formula -> age_band
    new_link("coverages_src", "j_uw",        "x"),
    new_link("uwr_edit",      "j_uw",        "y"),
    new_link("j_uw",          "age_calc",    "data"),
    new_link("age_calc",      "j_incidence", "x"),
    new_link("incidence_src", "j_incidence", "y"),
    new_link("j_incidence",   "j_country",   "x"),
    new_link("country_src",   "j_country",   "y"),
    new_link("j_country",     "j_annuity",   "x"),
    new_link("annuity_src",   "j_annuity",   "y"),
    new_link("j_annuity",     "formula",     "data"),
    new_link("formula",       "age_band",    "data"),

    # age_band -> dm -> crossfilter
    new_link("age_band",      "data",          "claims"),
    new_link("data",          "global_filter", "data"),

    # Chart paths (each starts with its own pull from the filtered dm).
    new_link("global_filter", "kpi_pull",      "data"),
    new_link("kpi_pull",      "kpi_sum",       "data"),
    new_link("kpi_sum",       "kpi",           "data"),

    new_link("global_filter", "age_pull",      "data"),
    new_link("age_pull",      "age_sum",       "data"),
    new_link("age_sum",       "age_chart",     "data"),

    new_link("global_filter", "ssm_pull",      "data"),
    new_link("ssm_pull",      "ssm_sum",       "data"),
    new_link("ssm_sum",       "ssm_chart",     "data"),

    new_link("global_filter", "cty_pull",      "data"),
    new_link("cty_pull",      "cty_sum",       "data"),
    new_link("cty_sum",       "cty_chart",     "data"),

    new_link("global_filter", "pol_pull",      "data"),
    new_link("pol_pull",      "pol_sum",       "data"),
    new_link("pol_sum",       "pol_top",       "data"),
    new_link("pol_top",       "pol_head",      "data"),
    new_link("pol_head",      "pol_chart",     "data"),

    new_link("global_filter", "uw_pull",       "data"),
    new_link("uw_pull",       "uw_sum",        "data"),
    new_link("uw_sum",        "uw_chart",      "data"),

    new_link("global_filter", "preview_pull",  "data"),
    new_link("preview_pull",  "preview",       "data"),

    # Pricing path: filtered claims -> total -> premium build-up -> KPI + waterfall
    new_link("global_filter", "pricing_pull",      "data"),
    new_link("pricing_pull",  "pricing_sum",       "data"),
    new_link("pricing_sum",   "premium_calc",      "data"),
    new_link("premium_calc",  "premium_kpi",       "data"),
    new_link("premium_calc",  "premium_waterfall", "data")
  ),

  extensions = list(
    blockr.dag::new_dag_extension()
  ),

  # Six tabs:
  #   Underwrite  — UWR's worksheet + global filter + KPI per coverage.
  #   Pricing     — premium build-up waterfall + headline KPI.
  #   Drivers     — drilldown charts to inspect what makes the price.
  #   Top         — concentration: top lives + UW-impact comparison.
  #   Pipeline    — the joins, formula chain, age band, dm.
  #   Setup       — data sources, age calc, DAG.
  layout = dock_layouts(
    Underwrite = dock_view(
      "uwr_edit", "global_filter", "kpi",
      active = TRUE
    ),
    Pricing = dock_view(
      "global_filter", "premium_kpi", "premium_waterfall"
    ),
    Drivers = dock_view(
      "global_filter",
      "age_chart", "ssm_chart", "cty_chart"
    ),
    Top = dock_view(
      "global_filter",
      "pol_chart", "uw_chart", "preview"
    ),
    Pipeline = dock_view(
      "j_uw", "j_incidence", "j_country", "j_annuity",
      "formula", "age_band", "data", "premium_calc"
    ),
    Setup = dock_view(
      "coverages_src", "uw_factors_src", "incidence_src",
      "country_src", "annuity_src",
      "age_calc", "dag_extension"
    )
  )
)


shiny::runApp(serve(board))
