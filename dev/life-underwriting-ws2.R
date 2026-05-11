# Life-underwriting demo, workspace 2: batch triage.
#
# Broker submits a 50-applicant tibble. The underwriter sorts, searches,
# overrides risk classes, deletes declines, and Apply — the engine
# re-rates the changed rows and the KPI shows the portfolio-level
# premium total.
#
# From /workspace:
#   Rscript blockr.insurance/dev/life-underwriting-ws2.R
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

# Synthetic broker submission: 50 applicants.
set.seed(2026)
n <- 50L
broker_submission <- tibble::tibble(
  id          = seq_len(n),
  name        = sprintf("Applicant_%03d", seq_len(n)),
  age         = sample(22:72, n, replace = TRUE),
  gender      = factor(sample(c("F", "M"), n, replace = TRUE), levels = c("F", "M")),
  smoker      = factor(sample(c("no", "yes"), n, replace = TRUE, prob = c(0.78, 0.22)),
                       levels = c("no", "yes")),
  sum_assured = round(runif(n, 50000, 750000), -3),
  risk_class  = factor(
    sample(c("preferred", "standard", "substandard"),
           n, replace = TRUE, prob = c(0.45, 0.45, 0.10)),
    levels = c("preferred", "standard", "substandard")
  )
)

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
    submission = new_static_block(data = broker_submission,
                                  block_name = "Broker submission"),
    review     = new_table_block(block_name = "Underwriter review"),
    price      = new_mutate_block(
      state = list(
        mutations = list(list(name = "premium", expr = life_premium_expr)),
        by = list()
      ),
      block_name = "Stub life engine"
    ),
    kpi        = new_tile_block(
      showcase = "number",
      state = list(
        aesthetics = list(value = c("premium", "sum_assured")),
        stats = list(value = "sum"),
        formats = list(measure_labels = c(
          premium     = "Total premium",
          sum_assured = "Total sum assured"
        ))
      ),
      block_name = "Portfolio KPIs"
    ),
    head       = new_head_block(n = 10L, block_name = "Output preview")
  ),
  links = c(
    new_link("submission", "review",  "data"),
    new_link("review",     "price",   "data"),
    new_link("price",      "kpi",     "data"),
    new_link("price",      "head",    "data")
  ),
  extensions = list(
    blockr.dag::new_dag_extension()
  ),
  layout = dock_layouts(
    Triage = dock_view(
      "review", "kpi", "head",
      active = TRUE
    ),
    Setup = dock_view(
      "submission", "price", "dag_extension"
    )
  )
)

shiny::runApp(serve(board))
