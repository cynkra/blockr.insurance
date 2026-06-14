# Solvency II / IFRS 17 review POC — pre-submission walkthrough of capital,
# MCR/SCR, technical provisions, and module breakdown across LOBs.
#
# Run from an R session:
#
#   pkgload::load_all("blockr.insurance")
#   source(system.file("examples", "solvency.R", package = "blockr.insurance"))
#
# Four workspaces (Setup / Capital position / SCR modules / Technical provisions)
# operate on three small *synthetic* tables defined inline below — they're
# sized like a small/mid European insurer (5 LOB x 8 quarters).
#
# IMPORTANT: the numbers are illustrative, not actuarial. Real Solvency II QRTs
# follow EIOPA templates and would slot in here once a sponsor provides them.
# This file is the *frame* of the review, not the content.

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

# ============================================================================
# SYNTHETIC SOLVENCY II DATA
# ============================================================================
set.seed(42)
periods <- c("2024-Q1", "2024-Q2", "2024-Q3", "2024-Q4",
             "2025-Q1", "2025-Q2", "2025-Q3", "2025-Q4")
lobs <- c("Motor", "Property", "Liability", "Life", "Health")

# Capital aggregates: BSCR -> +OpRisk +Adj -> SCR; eligible own funds split
# across tiers; MCR is a fraction of SCR.
sii_capital <- expand.grid(period = periods, lob = lobs,
                           stringsAsFactors = FALSE)
n <- nrow(sii_capital)
base_bscr <- c(Motor = 28, Property = 22, Liability = 18, Life = 36, Health = 12)
trend <- as.numeric(factor(sii_capital$period, levels = periods)) / 8
lob_factor <- base_bscr[sii_capital$lob]
noise <- rnorm(n, 0, 1.0)
sii_capital$bscr     <- round(lob_factor * (1 + 0.04 * trend) + noise, 1)
sii_capital$op_risk  <- round(0.12 * sii_capital$bscr +
                              rnorm(n, 0, 0.3), 1)
sii_capital$adj      <- round(-0.04 * sii_capital$bscr +
                              rnorm(n, 0, 0.2), 1)
sii_capital$scr      <- round(sii_capital$bscr + sii_capital$op_risk +
                              sii_capital$adj, 1)
sii_capital$mcr      <- round(0.32 * sii_capital$scr, 1)
sii_capital$eof_tier1 <- round(1.55 * sii_capital$scr +
                               rnorm(n, 0, 1.0), 1)
sii_capital$eof_tier2 <- round(0.18 * sii_capital$scr +
                               rnorm(n, 0, 0.4), 1)
sii_capital$eof_tier3 <- round(0.04 * sii_capital$scr +
                               rnorm(n, 0, 0.1), 1)
sii_capital$eof_total <- sii_capital$eof_tier1 + sii_capital$eof_tier2 +
                        sii_capital$eof_tier3
sii_capital$scr_ratio <- round(sii_capital$eof_total / sii_capital$scr, 3)

# SCR module breakdown (gross / net, before/after diversification benefits).
modules <- c("Market", "Counterparty", "Life", "Non-Life",
             "Health", "Intangible")
sii_modules <- expand.grid(period = periods, lob = lobs, module = modules,
                           stringsAsFactors = FALSE)
sii_modules$gross <- round(
  runif(nrow(sii_modules), 1, 14) *
    base_bscr[sii_modules$lob] / 28 *
    c(Market = 1.4, Counterparty = 0.5, Life = 0.9, `Non-Life` = 1.6,
      Health = 0.4, Intangible = 0.05)[sii_modules$module],
  1)
sii_modules$net <- round(sii_modules$gross * 0.78 +
                         rnorm(nrow(sii_modules), 0, 0.3), 1)

# Technical provisions: Best Estimate Liability + Risk Margin.
sii_provisions <- expand.grid(period = periods, lob = lobs,
                              stringsAsFactors = FALSE)
np <- nrow(sii_provisions)
base_bel <- c(Motor = 180, Property = 140, Liability = 95,
              Life = 320, Health = 60)
sii_provisions$bel <-
  round(base_bel[sii_provisions$lob] *
        (1 + 0.03 * trend[match(sii_provisions$period, periods)]) +
        rnorm(np, 0, 4), 0)
sii_provisions$risk_margin <- round(0.08 * sii_provisions$bel +
                                    rnorm(np, 0, 1.5), 1)
sii_provisions$tp_total <- sii_provisions$bel + sii_provisions$risk_margin

board <- new_dock_board(
  blocks = c(

    # === SHARED DATA ===
    cap_read = new_static_block(
      data = sii_capital,
      block_name = "sii_capital (synthetic)"
    ),
    mod_read = new_static_block(
      data = sii_modules,
      block_name = "sii_modules (synthetic)"
    ),
    tp_read = new_static_block(
      data = sii_provisions,
      block_name = "sii_provisions (synthetic)"
    ),
    data = new_dm_block(
      infer_keys = FALSE,
      block_name = "Solvency II dm"
    ),
    global_filter = new_crossfilter_block(
      active_dims = list(
        capital = c("period", "lob"),
        modules = c("period", "lob", "module"),
        provisions = c("period", "lob")
      ),
      block_name = "Global filter (Period x LOB)"
    ),

    # === CAPITAL POSITION ===
    cap_pull = new_dm_pull_block(table = "capital",
      block_name = "Pull capital"),
    cap_sum = new_summarize_block(
      state = list(
        summaries = list(
          list(type = "expr", name = "BSCR",
               expr = "sum(bscr, na.rm = TRUE)"),
          list(type = "expr", name = "Operational_Risk",
               expr = "sum(op_risk, na.rm = TRUE)"),
          list(type = "expr", name = "Adjustments",
               expr = "sum(adj, na.rm = TRUE)"),
          list(type = "expr", name = "SCR",
               expr = "sum(scr, na.rm = TRUE)"),
          list(type = "expr", name = "MCR",
               expr = "sum(mcr, na.rm = TRUE)"),
          list(type = "expr", name = "EOF_Total",
               expr = "sum(eof_total, na.rm = TRUE)")
        ),
        by = list()
      ),
      block_name = "Aggregate capital position"
    ),
    cap_ratio = new_mutate_block(
      state = list(
        mutations = list(
          list(name = "SCR_Ratio_pct",
               expr = "round(100 * EOF_Total / SCR, 1)"),
          list(name = "MCR_Ratio_pct",
               expr = "round(100 * EOF_Total / MCR, 1)")
        ),
        by = list()
      ),
      block_name = "SCR / MCR ratios"
    ),
    cap_kpi = new_tile_block(
      showcase = "number",
      state = list(
        aesthetics = list(value = c("SCR", "MCR", "EOF_Total",
                                    "SCR_Ratio_pct", "MCR_Ratio_pct")),
        stats = list(value = "sum"),
        formats = list(measure_labels = c(
          SCR           = "SCR (CHFm)",
          MCR           = "MCR (CHFm)",
          EOF_Total     = "Eligible Own Funds (CHFm)",
          SCR_Ratio_pct = "SCR coverage (%)",
          MCR_Ratio_pct = "MCR coverage (%)"
        ))
      ),
      block_name = "Capital headline KPIs"
    ),
    cap_wf = new_waterfall_block(
      measures = c("BSCR", "Operational_Risk", "Adjustments", "SCR"),
      block_name = "BSCR -> SCR bridge"
    ),

    # === SCR MODULE BREAKDOWN ===
    mod_pull = new_dm_pull_block(table = "modules",
      block_name = "Pull modules"),
    mod_sum = new_summarize_block(
      state = list(
        summaries = list(
          list(type = "expr", name = "Gross",
               expr = "sum(gross, na.rm = TRUE)"),
          list(type = "expr", name = "Net",
               expr = "sum(net, na.rm = TRUE)")
        ),
        by = c("module", "lob")
      ),
      block_name = "Sum by Module x LOB"
    ),
    mod_drill = new_chart_block(
      chart_type = "bar",
      group_by   = "module",
      color_by   = "lob",
      metric     = "Gross",
      agg_fn     = "sum",
      block_name = "SCR module gross capital, by LOB"
    ),

    # === TECHNICAL PROVISIONS ===
    tp_pull = new_dm_pull_block(table = "provisions",
      block_name = "Pull provisions"),
    tp_sum = new_summarize_block(
      state = list(
        summaries = list(
          list(type = "expr", name = "BEL",
               expr = "sum(bel, na.rm = TRUE)"),
          list(type = "expr", name = "Risk_Margin",
               expr = "sum(risk_margin, na.rm = TRUE)"),
          list(type = "expr", name = "TP_Total",
               expr = "sum(tp_total, na.rm = TRUE)")
        ),
        by = c("lob")
      ),
      block_name = "Sum by LOB"
    ),
    tp_drill = new_chart_block(
      chart_type = "bar",
      group_by   = "lob",
      color_by   = "lob",
      metric     = "TP_Total",
      agg_fn     = "sum",
      block_name = "Technical provisions by LOB"
    ),
    tp_trend_pull = new_dm_pull_block(table = "provisions",
      block_name = "Pull provisions"),
    tp_trend_sum = new_summarize_block(
      state = list(
        summaries = list(
          list(type = "expr", name = "TP_Total",
               expr = "sum(tp_total, na.rm = TRUE)")
        ),
        by = c("period", "lob")
      ),
      block_name = "TP by period x LOB"
    ),
    tp_trend_drill = new_chart_block(
      chart_type = "line",
      x_col      = "period",
      y_col      = "TP_Total",
      series_by  = "lob",
      block_name = "TP trend per LOB"
    )
  ),

  links = links(
    from = c(
      "cap_read", "mod_read", "tp_read", "data",
      "global_filter", "cap_pull", "cap_sum", "cap_ratio", "cap_ratio",
      "global_filter", "mod_pull", "mod_sum",
      "global_filter", "tp_pull", "tp_sum",
      "global_filter", "tp_trend_pull", "tp_trend_sum"
    ),
    to = c(
      "data", "data", "data", "global_filter",
      "cap_pull", "cap_sum", "cap_ratio", "cap_kpi", "cap_wf",
      "mod_pull", "mod_sum", "mod_drill",
      "tp_pull", "tp_sum", "tp_drill",
      "tp_trend_pull", "tp_trend_sum", "tp_trend_drill"
    ),
    input = c(
      "capital", "modules", "provisions", "data",
      "data", "data", "data", "data", "data",
      "data", "data", "data",
      "data", "data", "data",
      "data", "data", "data"
    )
  ),

  extensions = list(
    blockr.dag::new_dag_extension()
  ),

  layout = dock_layouts(
    Setup = dock_view(
      "cap_read", "mod_read", "tp_read", "data", "dag_extension",
      active = TRUE
    ),
    `Capital position` = dock_view(
      "global_filter",
      "cap_pull", "cap_sum", "cap_ratio", "cap_kpi", "cap_wf"
    ),
    `SCR modules` = dock_view(
      "global_filter",
      "mod_pull", "mod_sum", "mod_drill"
    ),
    `Technical provisions` = dock_view(
      "global_filter",
      "tp_pull", "tp_sum", "tp_drill",
      "tp_trend_pull", "tp_trend_sum", "tp_trend_drill"
    )
  )
)

serve(board, plugins = custom_plugins(manage_project()))
