# blockr.insurance

Bundled motor-insurance datasets and a multi-page [blockr](https://github.com/BristolMyersSquibb/blockr) example workflow. Designed as a demo / starting point for property-actuary conversations.

## Installation

```r
# install.packages("remotes")
remotes::install_github("cynkra/blockr.insurance")
```

## Datasets

Two synthesised cubes are lazy-loaded:

| Dataset | Rows | What |
|---|---|---|
| [`motor_portfolio`](`?motor_portfolio`) | 20,921 | Multi-insurer, multi-year motor-insurance portfolio cube. Year (2019–2024) × ten synthetic insurers × Fleet × Vehicle_type × Cover × Age_Class × Gender, with `Vehicles` and `Premium` per segment. |
| [`motor_losses`](`?motor_losses`) | 8,958 | Loss-development cube at the same grain. `Total_Incurred` plus 16 development years (`DY0`..`DY15`), shaped by `ChainLadder::MW2014`. |

```r
library(blockr.insurance)
data(motor_portfolio)
data(motor_losses)
```

Both datasets are derived from plain-CRAN sources (`insuranceData::dataCar` for the policy base, `ChainLadder::MW2014` for the development pattern); a multi-insurer × multi-year dimension is synthesised on top. See `data-raw/README.md` for the full schema and synthesis steps, and the inline help (`?motor_portfolio`, `?motor_losses`).

## Example workflow

A five-workspace [blockr](https://github.com/BristolMyersSquibb/blockr) dashboard ships in `inst/examples/insurance_poc.R`:

| Workspace | Shows |
|---|---|
| Setup | Data sources, `dm` build, DAG view |
| Portfolio | Global crossfilter on segmentation; KPI cards; premium-by-vehicle-type drilldown |
| Profitability | Premium-over-time per insurer (line chart with categorical year axis) |
| Claims | Large-claims drilldown (segment-level threshold filter) |
| Reserving | Development triangle, sum across segments by year × DY |

### Run

```r
library(blockr.insurance)
source(system.file("examples", "insurance_poc.R", package = "blockr.insurance"))
```

### Block stack

The example uses generic blockr blocks:

- `blockr.core` (read, transform), `blockr.dock` (panel layout), `blockr.dag` (DAG extension)
- `blockr.dm` (dm + crossfilter + pull), `blockr.dplyr` (filter / mutate / summarize)
- `blockr.bi` (KPI block), `blockr.sandbox` (drilldown chart)

No insurance-specific blocks. Custom blocks (e.g. waterfall, loss-triangle renderer, per-policy view) are roadmap items.

## Property pricing

`inst/examples/property_pricing.R` demonstrates the **rating-engine** pattern: a pure R function `engine(inputs, params) -> outputs` (here `engine_property()` or `engine_property_v2()`) wrapped in `new_price_block()`. The engine version is selectable at runtime via a dropdown in the block UI — toggle between v1 and the CAT-loaded v2 without rewiring. The engine returns a single wide `premium` table at the location grain — one row per insured location, with all price components (`base_premium`, `layer_share`, `exposure_premium`, `risk_premium`, `model_price`, …) as columns. Aggregate quantities are broadcast onto every row so a downstream crossfilter slice keeps the full schema.

```r
source(system.file("examples", "property_pricing.R", package = "blockr.insurance"))
```

## Portfolio explorer

A *portfolio* is a folder of policies, each with `inputs/{locations,claims}.csv`. `run_portfolio(dir)` calls the engine for every policy, writes per-policy `outputs/premium.csv`, and binds them all into a portfolio-root `premium.csv` with an added `policy_id` column.

`inst/examples/portfolio_explorer.R` compares **two pricing scenarios on the same book** (bundled as `portfolio-property/` run with `property_params`, and `portfolio-property-comparison/` run with `property_params_comparison` — same inputs, Italian `base_rate` ×1.30) across four workspaces:

| Workspace | Shows |
|---|---|
| Setup | Two `portfolio_premium` blocks + their dms |
| Base | Crossfilter on the base portfolio (`country / policy_id / peril`) → drill-down by policy |
| Alternative | Independent crossfilter on the comparison portfolio → drill-down by policy |
| Comparison | Per-location `compare_block` on `(policy_id, location_id)`, diff drill-down, and a waterfall summing the diffs across base / exposure / risk / model price |

```r
source(system.file("examples", "portfolio_explorer.R", package = "blockr.insurance"))
```

Helpers:

- `run_portfolio(dir, engine, params)` — walks policies, writes outputs and the aggregate.
- `read_portfolio_premium(dir)` / `read_portfolio_overview(dir)` — read the bound table / one-row-per-policy summary.
- `new_portfolio_premium_block(dir)`, `new_portfolio_overview_block(dir)`, `new_policy_loader_block(dir)` — blockr roots.
- `default_portfolio_dir()`, `default_comparison_portfolio_dir()` — paths to the bundled fixtures.

## License

GPL (>= 3). Underlying data:

- `dataCar` from [insuranceData](https://cran.r-project.org/package=insuranceData) — accompanies de Jong & Heller, *Generalised Linear Models for Insurance Data* (Cambridge, 2008).
- `MW2014` from [ChainLadder](https://cran.r-project.org/package=ChainLadder) — accompanies Wüthrich & Merz, *Stochastic Claims Reserving Manual* (2014).

The `motor_portfolio` and `motor_losses` cubes are synthesised on top of these. The synthesis script (`data-raw/build.R`) is seeded and reproducible.
