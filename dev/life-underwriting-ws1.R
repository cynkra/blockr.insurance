# Life-underwriting demo, workspace 1: single-applicant entry.
#
# Underwriter receives an application, types the applicant's attributes
# into the table block, hits Apply. The engine (a stub mutate that
# applies an age-band x smoker x tier loading on top of sum_assured)
# computes a quoted premium. KPI shows the quote.
#
# From /workspace:
#   Rscript blockr.insurance/dev/life-underwriting-ws1.R
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
pkgload::load_all("blockr.bi")
pkgload::load_all("blockr.dag")
pkgload::load_all("blockr.input")
pkgload::load_all("blockr.insurance")

# 0-row schema declaring the applicant attributes the underwriter fills in.
# The table block hydrates with an empty row from this schema.
applicant_schema <- tibble::tibble(
  id          = integer(),
  name        = character(),
  age         = integer(),
  gender      = factor(character(), levels = c("F", "M")),
  smoker      = factor(character(), levels = c("no", "yes")),
  sum_assured = numeric(),
  risk_class  = factor(
    character(),
    levels = c("preferred", "standard", "substandard")
  )
)

# Stub life-underwriting engine: premium is sum_assured times an
# age-band rate, scaled by smoker load and risk-tier multiplier.
# Replace with a real ilec_mortality lookup once the data flow proves out.
life_premium_expr <- paste(
  "sum_assured * ",
  "dplyr::case_when(age < 30 ~ 0.0005, age < 50 ~ 0.0010,",
  "                 age < 65 ~ 0.0030, TRUE ~ 0.0080) * ",
  "dplyr::if_else(smoker == 'yes', 1.5, 1.0) * ",
  "dplyr::case_when(",
  "  risk_class == 'preferred'   ~ 0.85,",
  "  risk_class == 'standard'    ~ 1.00,",
  "  risk_class == 'substandard' ~ 1.50,",
  "  TRUE ~ 1.00)"
)

board <- new_dock_board(
  blocks = c(
    schema = new_static_block(data = applicant_schema, block_name = "Applicant schema"),
    entry  = new_table_block(block_name = "Applicant entry"),
    price  = new_mutate_block(
      state = list(
        mutations = list(
          list(name = "premium", expr = life_premium_expr)
        ),
        by = list()
      ),
      block_name = "Stub life engine"
    ),
    kpi    = new_kpi_block(
      measures   = c("premium"),
      agg_fun    = "sum",
      titles     = c(premium = "Quoted premium"),
      block_name = "Quoted premium"
    ),
    head   = new_head_block(n = 5L, block_name = "Output preview")
  ),
  links = c(
    new_link("schema", "entry", "data"),
    new_link("entry",  "price", "data"),
    new_link("price",  "kpi",   "data"),
    new_link("price",  "head",  "data")
  ),
  extensions = list(
    blockr.dag::new_dag_extension()
  ),
  # Two tabs: Underwrite (entry + quote), Setup (schema + engine wiring).
  layout = dock_layouts(
    Underwrite = dock_view(
      "entry", "kpi", "head",
      active = TRUE
    ),
    Setup = dock_view(
      "schema", "price", "dag_extension"
    )
  )
)

shiny::runApp(serve(board))
