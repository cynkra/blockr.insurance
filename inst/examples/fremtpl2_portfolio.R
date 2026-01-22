# freMTPL2 Portfolio Analysis
#
# Interactive exploration of the French Motor Third Party Liability dataset
# (~678K policies). Demonstrates visual filtering, KPIs, pivot tables,
# aggregation, ggplot charts, and waterfall visualization using blockr.
#
# Features:
# - Visual filter with Area, VehGas, driver age bands, vehicle power bands
# - KPIs showing total exposure and claims
# - Pivot table for Area x VehGas breakdown
# - Aggregate block for claims by segment
# - ggplot bar chart of exposure by area
# - Waterfall chart showing portfolio metrics progression
#
# Blocks used:
# - blockr: new_static_block
# - blockr.dplyr: new_mutate_expr_block
# - blockr.bi: new_visual_filter_block, new_kpi_block, new_pivot_table_block,
#              new_aggregate_block, new_waterfall_block
# - blockr.ggplot: new_ggplot_block
#
# Data source: CASdatasets package (freMTPL2freq)

library(blockr)
library(blockr.dag)
library(blockr.bi)
library(blockr.dplyr)
library(blockr.ggplot)

run_app(
  blocks = c(
    # =========================================================================
    # DATA LOAD
    # =========================================================================

    data = new_static_block(
      # Use 10% sample for faster demo - set sample = FALSE for full data
      blockr.insurance::fremtpl2_freq(sample = TRUE)
    ),

    # =========================================================================
    # TRANSFORM: Create age and power bands for filtering
    # =========================================================================

    transform = new_mutate_expr_block(
      exprs = list(
        DrivAgeBand = "cut(DrivAge, breaks = c(17, 25, 35, 45, 55, 65, Inf), labels = c('18-25', '26-35', '36-45', '46-55', '56-65', '65+'), right = TRUE)",
        VehPowerBand = "cut(VehPower, breaks = c(0, 5, 7, 9, 11, Inf), labels = c('4-5', '6-7', '8-9', '10-11', '12+'), right = TRUE)"
      )
    ),

    # =========================================================================
    # VISUAL FILTER: Interactive crossfilter
    # =========================================================================

    filter = new_visual_filter_block(
      dimensions = c("Area", "VehGas", "DrivAgeBand", "VehPowerBand"),
      measure = "Exposure"
    ),

    # =========================================================================
    # ANALYSIS BLOCKS
    # =========================================================================

    # KPIs: Total exposure and claims
    kpis = new_kpi_block(
      measures = c("Exposure", "ClaimNb"),
      agg_fun = "sum",
      digits = "0",
      titles = c(Exposure = "Total Exposure", ClaimNb = "Total Claims"),
      subtitles = c(Exposure = "Policy-years at risk", ClaimNb = "Reported claims")
    ),

    # Pivot table: Area x VehGas breakdown
    pivot = new_pivot_table_block(
      rows = "Area",
      cols = "VehGas",
      measures = c("Exposure", "ClaimNb"),
      agg_fun = "sum",
      digits = "0"
    ),

    # Aggregate: Claims by segment
    frequency = new_aggregate_block(
      drill_down = c("Area", "DrivAgeBand"),
      values = c("Exposure", "ClaimNb"),
      agg_fun = "sum"
    ),

    # =========================================================================
    # VISUALIZATION: Exposure distribution by Area
    # =========================================================================

    # Aggregate for chart
    chart_data = new_aggregate_block(
      drill_down = "Area",
      values = "Exposure",
      agg_fun = "sum"
    ),

    # Bar chart of exposure by area
    exposure_chart = new_ggplot_block(
      type = "bar",
      x = "Area",
      y = "Exposure",
      fill = "Area"
    ),

    # =========================================================================
    # WATERFALL: Portfolio metrics progression
    # =========================================================================

    # Calculate portfolio metrics for waterfall
    waterfall_data = new_mutate_expr_block(
      exprs = list(
        # Portfolio frequency (claims per exposure)
        Frequency = "sum(ClaimNb) / sum(Exposure)",
        # Scale exposure for visualization (in thousands)
        Exposure_k = "sum(Exposure) / 1000",
        # Expected claims based on portfolio frequency
        Expected_Claims = "sum(Exposure) * (sum(ClaimNb) / sum(Exposure))",
        # Actual claims
        Actual_Claims = "sum(ClaimNb)"
      )
    ),

    # Waterfall showing exposure -> expected -> actual
    waterfall = new_waterfall_block(
      measures = c("Exposure_k", "Expected_Claims", "Actual_Claims")
    )
  ),
  links = c(
    # Data pipeline
    new_link("data", "transform", "data"),
    new_link("transform", "filter", "data"),

    # Filter feeds all analysis blocks
    new_link("filter", "kpis", "data"),
    new_link("filter", "pivot", "data"),
    new_link("filter", "frequency", "data"),

    # Chart pipeline
    new_link("filter", "chart_data", "data"),
    new_link("chart_data", "exposure_chart", "data"),

    # Waterfall pipeline
    new_link("filter", "waterfall_data", "data"),
    new_link("waterfall_data", "waterfall", "data")
  ),
  extensions = list(new_dag_extension())
)
