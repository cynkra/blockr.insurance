# Waterfall Chart Prototype
#
# Testing echarts4r waterfall visualization for pricing walks
# Run this script to see the output in RStudio viewer

library(echarts4r)
library(dplyr)

# =============================================================================
# Demo Data - Insurance Pricing Walk with Increases AND Decreases
# =============================================================================

# This simulates what a real pricing walk might look like:
# - Base Premium: Starting point
# - Cat Loading: Increase for catastrophe risk
# - Experience Credit: DECREASE for good loss history
# - Expense Loading: Increase for admin costs
# - Model Price: Final result

pricing_walk_data <- data.frame(
  Country = rep(c("Germany", "France", "Italy", "Spain"), each = 100),
  # Simulate location-level data
  TIV = runif(400, 50000, 500000)
) |>
  mutate(
    # Base premium as % of TIV
    Base_Premium = TIV * runif(n(), 0.008, 0.012),

    # Cat loading: +20-40%
    Cat_Loading_Pct = runif(n(), 0.20, 0.40),
    After_Cat = Base_Premium * (1 + Cat_Loading_Pct),

    # Experience credit: -5 to -15% (good loss history = discount)
    Experience_Credit_Pct = runif(n(), -0.15, -0.05),
    After_Experience = After_Cat * (1 + Experience_Credit_Pct),

    # Expense loading: +8-12%
    Expense_Loading_Pct = runif(n(), 0.08, 0.12),
    Model_Price = After_Experience * (1 + Expense_Loading_Pct)
  )

# Aggregate by country for the waterfall
pricing_summary <- pricing_walk_data |>
  group_by(Country) |>
  summarise(
    Base_Premium = sum(Base_Premium),
    After_Cat = sum(After_Cat),
    After_Experience = sum(After_Experience),
    Model_Price = sum(Model_Price),
    .groups = "drop"
  )

print(pricing_summary)

# =============================================================================
# Build Waterfall Structure
# =============================================================================

build_waterfall <- function(steps, values) {
  n <- length(values)
  deltas <- c(values[1], diff(values))

  helper <- numeric(n)
  positive <- numeric(n)
  negative <- numeric(n)

  cumsum_val <- 0

  for (i in seq_len(n)) {
    if (i == 1) {
      # First bar: starts from 0
      helper[i] <- 0
      positive[i] <- values[i]
      negative[i] <- 0
      cumsum_val <- values[i]
    } else if (i == n) {
      # Last bar: show as total (full bar from 0)
      helper[i] <- 0
      positive[i] <- values[i]
      negative[i] <- 0
    } else {
      # Middle bars: show delta (floating)
      delta <- deltas[i]
      if (delta >= 0) {
        helper[i] <- cumsum_val
        positive[i] <- delta
        negative[i] <- 0
      } else {
        helper[i] <- cumsum_val + delta
        positive[i] <- 0
        negative[i] <- abs(delta)
      }
      cumsum_val <- cumsum_val + delta
    }
  }

  data.frame(
    step = factor(steps, levels = steps),
    value = values,
    delta = deltas,
    helper = helper,
    positive = positive,
    negative = negative
  )
}

# =============================================================================
# Single Country Waterfall (Germany)
# =============================================================================

germany <- pricing_summary |> filter(Country == "Germany")
steps <- c("Base Premium", "Cat Loading", "Experience Credit", "Model Price")
values <- c(germany$Base_Premium, germany$After_Cat, germany$After_Experience, germany$Model_Price)

wf_germany <- build_waterfall(steps, values)
print(wf_germany)

# Nice waterfall with trend line
chart_germany <- wf_germany |>
  e_charts(step) |>
  e_bar(helper, stack = "waterfall",
        itemStyle = list(color = "transparent", borderColor = "transparent"),
        emphasis = list(disabled = TRUE)) |>
  e_bar(positive, stack = "waterfall", name = "Increase",
        itemStyle = list(
          color = "#22c55e",
          borderRadius = c(4, 4, 0, 0)
        ),
        label = list(
          show = TRUE,
          position = "top",
          formatter = htmlwidgets::JS("function(p) {
            if (p.value > 0) return '+' + (p.value/1000).toFixed(0) + 'k';
            return '';
          }")
        )) |>
  e_bar(negative, stack = "waterfall", name = "Decrease",
        itemStyle = list(
          color = "#ef4444",
          borderRadius = c(4, 4, 0, 0)
        ),
        label = list(
          show = TRUE,
          position = "bottom",
          formatter = htmlwidgets::JS("function(p) {
            if (p.value > 0) return '-' + (p.value/1000).toFixed(0) + 'k';
            return '';
          }")
        )) |>
  e_line(value, name = "Total",
         symbol = "circle", symbolSize = 8,
         lineStyle = list(color = "#6366f1", width = 2, type = "dashed"),
         itemStyle = list(color = "#6366f1"),
         label = list(
           show = TRUE,
           position = "top",
           formatter = htmlwidgets::JS("function(p) {
             return (p.value/1000).toFixed(0) + 'k';
           }")
         )) |>
  e_legend(show = FALSE) |>
  e_tooltip(
    trigger = "axis",
    formatter = htmlwidgets::JS("function(params) {
      var step = params[0].axisValue;
      var total = params[params.length-1].value;
      var delta = '';
      params.forEach(function(p) {
        if (p.seriesName === 'Increase' && p.value > 0) {
          delta = '<br/><span style=\"color:#22c55e\">+' + (p.value/1000).toFixed(1) + 'k</span>';
        }
        if (p.seriesName === 'Decrease' && p.value > 0) {
          delta = '<br/><span style=\"color:#ef4444\">-' + (p.value/1000).toFixed(1) + 'k</span>';
        }
      });
      return '<b>' + step + '</b>' + delta + '<br/>Total: ' + (total/1000).toFixed(1) + 'k';
    }")
  ) |>
  e_y_axis(
    name = "Premium",
    nameLocation = "middle",
    nameGap = 50,
    axisLabel = list(
      formatter = htmlwidgets::JS("function(value) {
        if (value >= 1000000) return (value/1000000).toFixed(1) + 'M';
        if (value >= 1000) return (value/1000).toFixed(0) + 'k';
        return value;
      }")
    )
  ) |>
  e_x_axis(
    axisLabel = list(
      rotate = 15,
      fontSize = 11
    )
  ) |>
  e_grid(left = "12%", right = "5%", bottom = "15%", top = "10%") |>
  e_title(
    text = "Germany - Pricing Walk",
    subtext = "From Base Premium to Model Price",
    left = "center"
  )

print(chart_germany)

# =============================================================================
# All Countries - Timeline Animation
# =============================================================================

# Build waterfall for each country
build_country_waterfall <- function(country_data) {
  steps <- c("Base Premium", "Cat Loading", "Experience Credit", "Model Price")

  country_data |>
    rowwise() |>
    mutate(
      wf = list(build_waterfall(
        steps,
        c(Base_Premium, After_Cat, After_Experience, Model_Price)
      ))
    ) |>
    select(Country, wf) |>
    tidyr::unnest(wf)
}

wf_all <- build_country_waterfall(pricing_summary)
print(wf_all)

chart_timeline <- wf_all |>
  group_by(Country) |>
  e_charts(step, timeline = TRUE) |>
  e_bar(helper, stack = "waterfall",
        itemStyle = list(color = "transparent", borderColor = "transparent"),
        emphasis = list(disabled = TRUE)) |>
  e_bar(positive, stack = "waterfall", name = "Increase",
        itemStyle = list(color = "#22c55e", borderRadius = c(4, 4, 0, 0))) |>
  e_bar(negative, stack = "waterfall", name = "Decrease",
        itemStyle = list(color = "#ef4444", borderRadius = c(4, 4, 0, 0))) |>
  e_line(value, name = "Total",
         symbol = "circle", symbolSize = 6,
         lineStyle = list(color = "#6366f1", type = "dashed"),
         itemStyle = list(color = "#6366f1")) |>
  e_legend(show = FALSE) |>
  e_timeline_opts(
    autoPlay = FALSE,
    playInterval = 2000,
    bottom = 0,
    currentIndex = 0
  ) |>
  e_y_axis(
    axisLabel = list(
      formatter = htmlwidgets::JS("function(value) {
        if (value >= 1000000) return (value/1000000).toFixed(1) + 'M';
        if (value >= 1000) return (value/1000).toFixed(0) + 'k';
        return value;
      }")
    )
  ) |>
  e_tooltip(trigger = "axis") |>
  e_title(
    text = "Pricing Walk by Country",
    subtext = "Click countries below to compare",
    left = "center"
  )

print(chart_timeline)

# =============================================================================
# Horizontal Version (cleaner for presentations)
# =============================================================================

chart_horizontal <- wf_germany |>
  e_charts(step) |>
  e_bar(helper, stack = "waterfall",
        itemStyle = list(color = "transparent", borderColor = "transparent"),
        emphasis = list(disabled = TRUE)) |>
  e_bar(positive, stack = "waterfall", name = "Increase",
        itemStyle = list(
          color = list(
            type = "linear", x = 0, y = 0, x2 = 1, y2 = 0,
            colorStops = list(
              list(offset = 0, color = "#22c55e"),
              list(offset = 1, color = "#16a34a")
            )
          ),
          borderRadius = c(0, 4, 4, 0)
        )) |>
  e_bar(negative, stack = "waterfall", name = "Decrease",
        itemStyle = list(
          color = list(
            type = "linear", x = 0, y = 0, x2 = 1, y2 = 0,
            colorStops = list(
              list(offset = 0, color = "#fca5a5"),
              list(offset = 1, color = "#ef4444")
            )
          ),
          borderRadius = c(0, 4, 4, 0)
        )) |>
  e_flip_coords() |>
  e_legend(show = FALSE) |>
  e_x_axis(
    axisLabel = list(
      formatter = htmlwidgets::JS("function(value) {
        if (value >= 1000000) return (value/1000000).toFixed(1) + 'M';
        if (value >= 1000) return (value/1000).toFixed(0) + 'k';
        return value;
      }")
    )
  ) |>
  e_grid(left = "25%", right = "10%") |>
  e_tooltip(trigger = "axis") |>
  e_title(
    text = "Germany - Pricing Walk (Horizontal)",
    left = "center"
  )

print(chart_horizontal)

# =============================================================================
# Summary
# =============================================================================

cat("\n")
cat("==============================================\n")
cat("WATERFALL CHART DEMOS\n")
cat("==============================================\n")
cat("\n")
cat("Created charts with SYNTHETIC pricing data:\n")
cat("- Base Premium -> Cat Loading (+) -> Experience Credit (-) -> Model Price\n")
cat("\n")
cat("Charts available:\n")
cat("  chart_germany    - Single country with labels & trend line\n
")
cat("  chart_timeline   - All countries with timeline selector\n")
cat("  chart_horizontal - Horizontal layout for presentations\n")
cat("\n")
cat("Run: print(chart_germany) to view\n")
cat("==============================================\n")
