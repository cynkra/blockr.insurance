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
  blockr.lazy_eval            = FALSE,
  blockr.ai_model             = "gpt-4o-mini"
)

pkgload::load_all("blockr.core")
pkgload::load_all("blockr.dock")
pkgload::load_all("blockr.dplyr")
pkgload::load_all("blockr.dm")
pkgload::load_all("blockr.viz")
pkgload::load_all("blockr.dag")
pkgload::load_all("blockr.input")
pkgload::load_all("blockr.io")
pkgload::load_all("blockr.session")
# blockr.extra overrides block_output for data/transform blocks so the
# html_table_preview option actually fires on initial load. Without this,
# tile / waterfall outputs stay suspended until you nudge a block.
pkgload::load_all("blockr.extra")
pkgload::load_all("blockr.ai")
pkgload::load_all("blockr.code")
pkgload::load_all("blockr.insurance")

# CSV paths shipped with the package — `new_read_block(source = "path")`
# defaults to these so the workspace runs end-to-end on first launch. The
# UWR can swap to upload-mode via the read block's cogwheel.
employees_csv <- system.file("extdata", "employees.csv",
                             package = "blockr.insurance")
claims_csv    <- system.file("extdata", "life_claims.csv",
                             package = "blockr.insurance")

# Valuation date drives the age calculation. Edit via the cogwheel of the
# `age_calc` block in the Setup tab.
valuation_date <- "2026-05-11"

# Same summarize three-measure shape, reused across every chart path.
sum_by <- function(by, block_name = paste("Sum by", paste(by, collapse = " x "))) {
  new_summarize_block(
    summaries = list(
      list(type = "expr", name = "Expected_Claims",
           expr = "sum(Expected_Claims, na.rm = TRUE)"),
      list(type = "expr", name = "Expected_Claims_Raw",
           expr = "sum(Expected_Claims_Raw, na.rm = TRUE)"),
      list(type = "expr", name = "Sum_at_Risk",
           expr = "sum(sum_at_risk, na.rm = TRUE)")
    ),
    by = by,
    block_name = block_name
  )
}

board <- new_dock_board(
  blocks = c(

    # === INPUTS ===
    # Employees census: pre-configured read from the bundled CSV; the UWR
    # can swap to upload-mode via the cogwheel. The grid block downstream
    # is the "tweak" layer — same UX pattern as the claims experience.
    employees_read = new_read_block(
      path       = employees_csv,
      source     = "path",
      block_name = "Employees census (upload)"
    ),
    employees_edit = new_grid_entry_block(
      state = list(key_col = "person_id"),
      block_name = "Tweak employees"
    ),
    # Expand 1,500 employee rows -> 4,500 (person x coverage) by pivoting
    # the three `sum_*` columns. Downstream pipeline operates on long form.
    coverage_pivot = new_pivot_longer_block(
      cols           = c("sum_Death", "sum_Disability", "sum_CriticalIllness"),
      names_to       = "coverage_type",
      values_to      = "sum_at_risk",
      names_prefix   = "sum_",
      values_drop_na = FALSE,
      block_name = "Expand employees to per-coverage rows"
    ),
    # Historical claims experience for this company: same upload+tweak pair.
    claims_read = new_read_block(
      path       = claims_csv,
      source     = "path",
      block_name = "Claims experience (upload)"
    ),
    claims_edit = new_grid_entry_block(
      state = list(key_col = "claim_id"),
      block_name = "Tweak claims"
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
    uwr_edit = new_grid_entry_block(
      state = list(key_col = "coverage_type"),
      block_name = "Underwriter worksheet (per coverage)"
    ),

    # === PIPELINE: JOIN UW + AGE + JOINS + FORMULA ===
    j_uw = new_join_block(
      type = "left_join",
      keys = list(
        list(xCol = "coverage_type", op = "==", yCol = "coverage_type")
      ),
      exprs = list(), suffix_x = ".x", suffix_y = ".y",
      block_name = "Join UW factors"
    ),
    age_calc = new_mutate_block(
      mutations = list(
        list(name = "age",
             expr = sprintf(
               "as.integer(as.numeric(as.Date('%s') - dob) / 365.25)",
               valuation_date
             ))
      ),
      by = list(),
      block_name = "Age at valuation date"
    ),
    j_incidence = new_join_block(
      type = "left_join",
      keys = list(
        list(xCol = "coverage_type", op = "==", yCol = "coverage_type"),
        list(xCol = "age",           op = "==", yCol = "age"),
        list(xCol = "sex",           op = "==", yCol = "sex"),
        list(xCol = "smoker",        op = "==", yCol = "smoker")
      ),
      exprs = list(), suffix_x = ".x", suffix_y = ".y",
      block_name = "Join incidence (coverage x age x sex x smoker)"
    ),
    j_country = new_join_block(
      type = "left_join",
      keys = list(list(xCol = "country", op = "==", yCol = "country")),
      exprs = list(), suffix_x = ".x", suffix_y = ".y",
      block_name = "Join country adjustment"
    ),
    j_annuity = new_join_block(
      type = "left_join",
      keys = list(
        list(xCol = "age", op = "==", yCol = "age"),
        list(xCol = "sex", op = "==", yCol = "sex")
      ),
      exprs = list(), suffix_x = ".x", suffix_y = ".y",
      block_name = "Join annuity (capitalization factor)"
    ),
    formula = new_mutate_block(
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
             expr = "Prob_Event_Raw * SI_Capitalized"),
        # Per-row Net + Gross so pivot_table can aggregate downstream
        # without an intermediate summarize+mutate.
        list(name = "Net_Premium",
             expr = "Expected_Claims * 1.05"),
        list(name = "Gross_Premium",
             expr = "Net_Premium * 1.25")
      ),
      by = list(),
      block_name = "Expected claims + premium (per person x coverage)"
    ),
    age_band = new_mutate_block(
      mutations = list(
        list(name = "age_band",
             expr = paste(
               "cut(age,",
               "breaks = c(-Inf, 29, 44, 59, 74, Inf),",
               "labels = c('18-29', '30-44', '45-59', '60-74', '75+'),",
               "ordered_result = TRUE)"
             ))
      ),
      by = list(),
      block_name = "Age band"
    ),

    # === CROSSFILTER ===
    # Crossfilter takes the post-formula data.frame directly (no dm wrapper).
    # Wrapping a single-table dm before crossfilter triggers an "unzoomed
    # <dm>" error from internal dplyr-on-dm calls; passing a data.frame
    # sidesteps it — crossfilter wraps internally and uses dplyr::filter.
    global_filter = new_crossfilter_block(
      active_dims = list(.tbl = c(
        "coverage_type", "sex", "smoker", "country", "age_band"
      )),
      block_name = "Global filter (UWR-controlled)"
    ),

    # Per-coverage breakdown removed — waterfall + driver charts already
    # convey the structure. Just one headline number on Underwrite.

    # === DRIVER CHARTS ===
    # 1. Expected claims by age band, split by coverage.
    age_sum  = sum_by(c("age_band", "coverage_type")),
    age_chart = new_chart_block(
      chart_type = "bar",
      group = "age_band",
      color = "coverage_type",
      metric   = "Expected_Claims",
      agg_fn   = "sum",
      block_name = "Expected claims by age band x coverage"
    ),

    # 2. Expected claims by sex, color by smoker.
    ssm_sum  = sum_by(c("sex", "smoker")),
    ssm_chart = new_chart_block(
      chart_type = "bar",
      group = "sex",
      color = "smoker",
      metric   = "Expected_Claims",
      agg_fn   = "sum",
      block_name = "Expected claims by sex x smoker"
    ),

    # 3. Expected claims by country, split by coverage.
    cty_sum  = sum_by(c("country", "coverage_type")),
    cty_chart = new_chart_block(
      chart_type = "bar",
      group = "country",
      color = "coverage_type",
      metric   = "Expected_Claims",
      agg_fn   = "sum",
      block_name = "Expected claims by country x coverage"
    ),

    # 4. Top lives by expected claims — where the cost is concentrated.
    pol_sum  = sum_by("person_id"),
    pol_top  = new_arrange_block(
      columns = list(
        list(column = "Expected_Claims", direction = "desc")
      ),
      block_name = "Sort by expected claims desc"
    ),
    pol_head = new_head_block(n = 15L,
                              block_name = "Top 15 lives"),
    pol_chart = new_chart_block(
      chart_type = "bar",
      group = "person_id",
      metric   = "Expected_Claims",
      agg_fn   = "sum",
      block_name = "Top lives by expected claims"
    ),

    # 5. UW impact: full-UW vs raw (shows what UW assessment is changing).
    uw_sum  = sum_by("coverage_type"),
    uw_chart = new_chart_block(
      chart_type = "bar",
      group = "coverage_type",
      metric   = "Expected_Claims",
      agg_fn   = "sum",
      block_name = "Full UW vs raw — per coverage"
    ),

    # Per-row preview of the post-formula data.
    preview = new_head_block(n = 12L, block_name = "Per-row preview"),

    # === INPUTS: overview chart — insured value by country x coverage ===
    # Branched off coverage_pivot so it reacts to the UWR's grid edits. Sums
    # `sum_at_risk` and counts unique employees per (country, coverage_type).
    employees_sum = new_summarize_block(
      summaries = list(
        list(type = "expr", name = "Sum_at_Risk",
             expr = "sum(sum_at_risk, na.rm = TRUE)"),
        list(type = "expr", name = "Headcount",
             expr = "dplyr::n_distinct(person_id)")
      ),
      by = c("country", "coverage_type"),
      block_name = "Insured value by country x coverage"
    ),
    employees_chart = new_chart_block(
      chart_type = "bar",
      group = "country",
      color = "coverage_type",
      metric   = "Sum_at_Risk",
      agg_fn   = "sum",
      block_name = "Insured value by country x coverage"
    ),

    # === EXPERIENCE: historical claims summary + chart ===
    # The UWR uses experience to sanity-check the forward-looking Expected
    # Claims. Bar chart: year x coverage_type x claim amount totals.
    claims_sum = new_summarize_block(
      summaries = list(
        list(type = "expr", name = "Total_Claims",
             expr = "sum(claim_amount, na.rm = TRUE)"),
        list(type = "expr", name = "Num_Claims",
             expr = "dplyr::n()")
      ),
      by = c("year", "coverage_type"),
      block_name = "Claims by year x coverage"
    ),
    claims_chart = new_chart_block(
      chart_type = "bar",
      group = "year",
      color = "coverage_type",
      metric   = "Total_Claims",
      agg_fn   = "sum",
      block_name = "Historical claims by year x coverage"
    ),

    # === WATERFALL: Expected claims -> Gross premium build-up (portfolio) ===
    # Surcharges below are demo loadings, edit via cogwheel of premium_calc.
    #   Safety margin  : 5%  of Expected_Claims
    #   Expense        : 12% of Net_Premium  (admin / IT / overhead)
    #   Commission     : 8%  of Net_Premium  (broker / distribution)
    #   Profit margin  : 5%  of Net_Premium  (target return)
    # Portfolio-level — per-coverage Net + Gross live in the kpi tile.
    pricing_sum = new_summarize_block(
      summaries = list(
        list(type = "expr", name = "Expected_Claims",
             expr = "sum(Expected_Claims, na.rm = TRUE)")
      ),
      by = list(),
      block_name = "Total expected claims"
    ),
    premium_calc = new_mutate_block(
      mutations = list(
        list(name = "Net_Premium",
             expr = "Expected_Claims * 1.05"),
        list(name = "After_Expense",
             expr = "Net_Premium + Net_Premium * 0.12"),
        list(name = "After_Commission",
             expr = "After_Expense + Net_Premium * 0.08"),
        list(name = "Gross_Premium",
             expr = "After_Commission + Net_Premium * 0.05"),
        # Per-step DELTAS for the (long-form) waterfall chart. The new
        # waterfall chart floats each bar from the running cumulative, so
        # each step's metric is the increment, not the running total.
        list(name = "d_Expected_Claims",
             expr = "Expected_Claims"),
        list(name = "d_Safety_Margin",
             expr = "Net_Premium - Expected_Claims"),
        list(name = "d_Expense",
             expr = "After_Expense - Net_Premium"),
        list(name = "d_Commission",
             expr = "After_Commission - After_Expense"),
        list(name = "d_Profit_Margin",
             expr = "Gross_Premium - After_Commission")
      ),
      by = list(),
      block_name = "Premium build-up (surcharges)"
    ),
    # Pivot the per-step deltas to long (step, value) form, then order the
    # step factor so the waterfall renders Expected claims -> Gross premium.
    premium_long = new_pivot_longer_block(
      cols           = c("d_Expected_Claims", "d_Safety_Margin",
                         "d_Expense", "d_Commission", "d_Profit_Margin"),
      names_to       = "step",
      values_to      = "amount",
      names_prefix   = "d_",
      values_drop_na = FALSE,
      block_name = "Waterfall steps (long form)"
    ),
    premium_steps = new_mutate_block(
      mutations = list(
        list(name = "step",
             expr = paste(
               "factor(step, levels = c('Expected_Claims',",
               "'Safety_Margin', 'Expense', 'Commission',",
               "'Profit_Margin'), ordered = TRUE)"
             ))
      ),
      by = list(),
      block_name = "Order waterfall steps"
    ),
    premium_waterfall = new_chart_block(
      chart_type = "waterfall",
      group  = "step",
      metric = "amount",
      agg_fn = "sum",
      block_name = "Premium build-up — Expected claims to Gross premium"
    ),
    # Headline tiles — three numbers: Expected claims, Net premium, Gross
    # premium. The tile is now a pure renderer (no aggregation), so the
    # premium_calc one-row frame is summarized into card-labelled columns
    # via dplyr::first() (the old stats = "first"), then rendered as tiles.
    kpi_totals_sum = new_summarize_block(
      summaries = list(
        list(type = "expr", name = "Expected claims (EUR)",
             expr = "dplyr::first(Expected_Claims)"),
        list(type = "expr", name = "Net premium (EUR)",
             expr = "dplyr::first(Net_Premium)"),
        list(type = "expr", name = "Gross premium (EUR)",
             expr = "dplyr::first(Gross_Premium)")
      ),
      by = character(),
      block_name = "Portfolio totals"
    ),
    kpi_totals = new_tile_block(
      value = c("Expected claims (EUR)", "Net premium (EUR)",
                "Gross premium (EUR)"),
      format = "compact",
      block_name = "Portfolio totals"
    )
  ),

  links = c(
    # UWR edits uw_factors via the grid block.
    new_link("uw_factors_src", "uwr_edit", "data"),

    # Employees: read CSV -> tweak grid -> pivot to per-coverage rows.
    new_link("employees_read", "employees_edit",  "data"),
    new_link("employees_edit", "coverage_pivot",  "data"),

    # Overview chart on the Inputs tab — tee off coverage_pivot.
    new_link("coverage_pivot", "employees_sum",   "data"),
    new_link("employees_sum",  "employees_chart", "data"),

    # Claims experience: read CSV -> tweak grid -> summarize -> chart.
    new_link("claims_read", "claims_edit", "data"),
    new_link("claims_edit", "claims_sum",  "data"),
    new_link("claims_sum",  "claims_chart","data"),

    # pivoted (4,500) x edited uw_factors -> aged -> joins -> formula -> age_band
    new_link("coverage_pivot", "j_uw",       "x"),
    new_link("uwr_edit",       "j_uw",       "y"),
    new_link("j_uw",       "age_calc",    "data"),
    new_link("age_calc",   "j_incidence", "x"),
    new_link("incidence_src", "j_incidence", "y"),
    new_link("j_incidence",   "j_country",   "x"),
    new_link("country_src",   "j_country",   "y"),
    new_link("j_country",     "j_annuity",   "x"),
    new_link("annuity_src",   "j_annuity",   "y"),
    new_link("j_annuity",     "formula",     "data"),
    new_link("formula",       "age_band",    "data"),

    # age_band feeds crossfilter directly as a data.frame.
    new_link("age_band", "global_filter", "data"),

    # Chart paths: filtered data.frame -> summarize -> chart.

    new_link("global_filter", "age_sum",   "data"),
    new_link("age_sum",       "age_chart", "data"),

    new_link("global_filter", "ssm_sum",   "data"),
    new_link("ssm_sum",       "ssm_chart", "data"),

    new_link("global_filter", "cty_sum",   "data"),
    new_link("cty_sum",       "cty_chart", "data"),

    new_link("global_filter", "pol_sum",   "data"),
    new_link("pol_sum",       "pol_top",   "data"),
    new_link("pol_top",       "pol_head",  "data"),
    new_link("pol_head",      "pol_chart", "data"),

    new_link("global_filter", "uw_sum",    "data"),
    new_link("uw_sum",        "uw_chart",  "data"),

    new_link("global_filter", "preview",   "data"),

    # Pricing path: filtered df -> portfolio total -> build-up -> waterfall + headline.
    new_link("global_filter", "pricing_sum",       "data"),
    new_link("pricing_sum",   "premium_calc",      "data"),
    new_link("premium_calc",  "premium_long",      "data"),
    new_link("premium_long",  "premium_steps",     "data"),
    new_link("premium_steps", "premium_waterfall", "data"),
    new_link("premium_calc",  "kpi_totals_sum",    "data"),
    new_link("kpi_totals_sum", "kpi_totals",       "data")
  ),

  extensions = list(
    blockr.dag::new_dag_extension()
  ),

  # Final-app tabs:
  #   Inputs      — employees upload-and-tweak.
  #   Claims      — claims experience upload-and-tweak + historical chart.
  #   Underwrite  — the main page (the whole price story in one place):
  #                 worksheet, filter, portfolio totals tile, premium build-up
  #                 waterfall (the star), per-coverage premium table, driver
  #                 charts (age / sex-smoker / country).
  #   Analysis    — top lives, UW impact, per-row preview (use-once deep dives).
  #
  # Actuarial    — the pipeline guts. Keep visible during dev; in production
  #                drop this `dock_view()` and set blockr.dock_is_locked=TRUE.
  layouts = list(
    Inputs = dock_layout(
      "employees_read", "employees_edit", "employees_chart",
      name = "Inputs"
    ),
    Claims = dock_layout(
      "claims_read", "claims_edit", "claims_chart",
      name = "Claims"
    ),
    Underwrite = dock_layout(
      "uwr_edit", "global_filter",
      "kpi_totals_sum", "kpi_totals", "premium_waterfall",
      name = "Underwrite"
    ),
    Analysis = dock_layout(
      "global_filter",
      "age_chart", "ssm_chart", "cty_chart",
      "pol_chart", "uw_chart", "preview",
      name = "Analysis"
    ),
    Actuarial = dock_layout(
      "coverage_pivot",
      "j_uw", "j_incidence", "j_country", "j_annuity",
      "formula", "age_band", "premium_calc",
      "premium_long", "premium_steps",
      "uw_factors_src", "incidence_src",
      "country_src", "annuity_src",
      "age_calc", "dag_extension",
      name = "Actuarial"
    )
  ),
  active = "Inputs"
)


serve(
  board,
  plugins = custom_plugins(c(
    ai_ctrl_block(),
    manage_project(),
    generate_flat_code()
  ))
)
