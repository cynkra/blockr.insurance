# freMTPL2 Portfolio Analysis - Deployed App
#
# Interactive exploration of the French Motor Third Party Liability dataset

library(blockr)
library(blockr.dag)
library(blockr.bi)
library(blockr.dplyr)
library(blockr.ggplot)
library(blockr.insurance)

run_app(
  blocks = c(
    # DATA LOAD
    data = new_static_block(
      blockr.insurance::fremtpl2_freq(sample = TRUE)
    ),

    # TRANSFORM: Create age and power bands
    transform = new_mutate_expr_block(
      exprs = list(
        DrivAgeBand = "cut(DrivAge, breaks = c(17, 25, 35, 45, 55, 65, Inf), labels = c('18-25', '26-35', '36-45', '46-55', '56-65', '65+'), right = TRUE)",
        VehPowerBand = "cut(VehPower, breaks = c(0, 5, 7, 9, 11, Inf), labels = c('4-5', '6-7', '8-9', '10-11', '12+'), right = TRUE)"
      )
    ),

    # VISUAL FILTER
    filter = new_visual_filter_block(
      dimensions = c("Area", "VehGas", "DrivAgeBand", "VehPowerBand"),
      measure = "Exposure"
    ),

    # KPIs
    kpis = new_kpi_block(
      measures = c("Exposure", "ClaimNb"),
      agg_fun = "sum",
      digits = "0",
      titles = c(Exposure = "Total Exposure", ClaimNb = "Total Claims"),
      subtitles = c(Exposure = "Policy-years at risk", ClaimNb = "Reported claims")
    ),

    # Pivot table
    pivot = new_pivot_table_block(
      rows = "Area",
      cols = "VehGas",
      measures = c("Exposure", "ClaimNb"),
      agg_fun = "sum",
      digits = "0"
    ),

    # Aggregate
    frequency = new_aggregate_block(
      drill_down = c("Area", "DrivAgeBand"),
      values = c("Exposure", "ClaimNb"),
      agg_fun = "sum"
    ),

    # Chart data
    chart_data = new_aggregate_block(
      drill_down = "Area",
      values = "Exposure",
      agg_fun = "sum"
    ),

    # ggplot chart
    exposure_chart = new_ggplot_block(
      type = "bar",
      x = "Area",
      y = "Exposure",
      fill = "Area"
    ),

    # Waterfall data
    waterfall_data = new_mutate_expr_block(
      exprs = list(
        Frequency = "sum(ClaimNb) / sum(Exposure)",
        Exposure_k = "sum(Exposure) / 1000",
        Expected_Claims = "sum(Exposure) * (sum(ClaimNb) / sum(Exposure))",
        Actual_Claims = "sum(ClaimNb)"
      )
    ),

    # Waterfall
    waterfall = new_waterfall_block(
      measures = c("Exposure_k", "Expected_Claims", "Actual_Claims")
    )
  ),
  links = c(
    new_link("data", "transform", "data"),
    new_link("transform", "filter", "data"),
    new_link("filter", "kpis", "data"),
    new_link("filter", "pivot", "data"),
    new_link("filter", "frequency", "data"),
    new_link("filter", "chart_data", "data"),
    new_link("chart_data", "exposure_chart", "data"),
    new_link("filter", "waterfall_data", "data"),
    new_link("waterfall_data", "waterfall", "data")
  ),
  extensions = list(new_dag_extension())
)
