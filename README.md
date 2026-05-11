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
| [`ilec_mortality`](`?ilec_mortality`) | 311,462 | US individual life insurance mortality experience (SOA ILEC 2013–2017). One row per `(uw × face_amount_band × year × duration_band × age_band × gender × plan × ltp × issue_year_band)` cell with actual and VBT-2015-expected deaths, by count and by face amount. |

```r
library(blockr.insurance)
data(motor_portfolio)
data(motor_losses)
```

The motor cubes are derived from plain-CRAN sources (`insuranceData::dataCar` for the policy base, `ChainLadder::MW2014` for the development pattern); a multi-insurer × multi-year dimension is synthesised on top. `ilec_mortality` is the SOA Research Institute's pre-summarised "lean" version of the ILEC 2013–2017 study (Apache-2.0, [RILEC repo](https://github.com/Society-of-actuaries-research-institute/RILEC)) — real industry data, no synthesis. See `data-raw/README.md` for the full schemas and `?motor_portfolio`, `?motor_losses`, `?ilec_mortality` for inline help.

## Example workflow

A five-workspace [blockr](https://github.com/BristolMyersSquibb/blockr) dashboard ships in `inst/examples/motor.R`:

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
source(system.file("examples", "motor.R", package = "blockr.insurance"))
```

### Block stack

The example uses generic blockr blocks:

- `blockr.core` (read, transform), `blockr.dock` (panel layout), `blockr.dag` (DAG extension)
- `blockr.dm` (dm + crossfilter + pull), `blockr.dplyr` (filter / mutate / summarize)
- `blockr.bi` (KPI block), `blockr.sandbox` (drilldown chart)

No insurance-specific blocks. Custom blocks (e.g. waterfall, loss-triangle renderer, per-policy view) are roadmap items.

## Life mortality example

A five-workspace life-insurance dashboard ships in `inst/examples/life.R`, parallel in structure to `motor.R` but built around the actual-to-expected (A/E) ratio against VBT 2015:

| Workspace | Shows |
|---|---|
| Setup | `ilec_mortality` dataset, dm wrap, DAG view |
| Mortality | Global crossfilter on UW class / gender / plan / level-term / age-band / duration / issue-year / face-amount; A/E KPIs (by amount and by count) and A/E by issue-age band |
| Trend | A/E by amount over `observation_year`, line per `insurance_plan` |
| Underwriting | A/E by `uw` class split by `dur_band1` — preferred-class wear-off curve |
| Face_Amount | A/E by `face_amount_band` split by gender — anti-selection at jumbo bands |

```r
source(system.file("examples", "life.R", package = "blockr.insurance"))
```

## Property workbench

`inst/examples/property-workbench.R` is the SAA-targeted Homer-Actuarial-UI replica: same property book, two engine versions (`engine_property()` and `engine_property_v2()` with CAT loading), engine version toggleable live via a dropdown on `new_price_block()`, drill into a policy via a scatter click, side-by-side compare at portfolio grain. Five workspaces (Setup / Portfolio / Policy / Compare-Portfolio).

```r
source(system.file("examples", "property-workbench.R", package = "blockr.insurance"))
```

## Portfolio explorer (static)

A *portfolio* is a folder of policies, each with `inputs/{locations,claims}.csv`. `run_portfolio(dir)` calls the engine for every policy, writes per-policy `outputs/premium.csv`, and binds them all into a portfolio-root `premium.csv` with an added `policy_id` column.

`inst/examples/portfolio-explorer-static.R` compares **two pricing scenarios on the same book** read from those pre-baked CSVs (bundled as `portfolio-property/` run with `property_params`, and `portfolio-property-comparison/` run with `property_params_comparison` — same inputs, Italian `base_rate` ×1.30) across four workspaces. Read-only, no live engine — superseded by `property-workbench.R` for live work; kept as a static reference.

| Workspace | Shows |
|---|---|
| Setup | Two `portfolio_premium` blocks + their dms |
| Base | Crossfilter on the base portfolio (`country / policy_id / peril`) → drill-down by policy |
| Alternative | Independent crossfilter on the comparison portfolio → drill-down by policy |
| Comparison | Per-location `compare_block` on `(policy_id, location_id)`, diff drill-down, and a waterfall summing the diffs across base / exposure / risk / model price |

```r
source(system.file("examples", "portfolio-explorer-static.R", package = "blockr.insurance"))
```

Helpers:

- `run_portfolio(dir, engine, params)` — walks policies, writes outputs and the aggregate.
- `read_portfolio_premium(dir)` / `read_portfolio_overview(dir)` — read the bound table / one-row-per-policy summary.
- `new_portfolio_premium_block(dir)`, `new_portfolio_overview_block(dir)`, `new_policy_loader_block(dir)` — blockr roots.
- `default_portfolio_dir()`, `default_comparison_portfolio_dir()` — paths to the bundled fixtures.

## Reinsurance demo (Swiss Re conversation)

`inst/examples/reinsurance.R` is one board for a group-risk audience
at a reinsurer. The data is a synthetic assumed book (one row per
treaty × cedant × peril × region × LOB × year), a 4,000-event
modelled catalogue fanned across five scenarios, and a per-event
breakdown table (cedant shares + treaty layer impacts), built
deterministically by `inst/examples/_reins_data.R`. Not a real CAT
model — sized to make the live crossfilter + exceedance-curve
story land.

| Workspace | Shows |
|---|---|
| Setup | Three data sources (exposure / events / event_profile), the dm, and the DAG. |
| Portfolio | Global crossfilter on scenario × peril × region × cedant × LOB; KPIs (premium / exposure / expected loss); drilldowns by peril × region and cedant × LOB. |
| Accumulation | Exceedance curve rebuilt from the filtered event catalogue (one line per peril, log-shaped). |
| Stress | Same curve mechanic, but rows are coloured by scenario so all five overlay on one chart. |
| Event profile | Top-events bar (click a bar to drill into one event); downstream blocks show that event's cedant breakdown and treaty layer impacts. |
| Tail | Top-25 tail events by cedant × peril. |

```r
source(system.file("examples", "reinsurance.R", package = "blockr.insurance"))
```

`ai_ctrl_block()` is enabled board-wide, so filter / arrange /
mutate / slice blocks expose a sparkle button that opens a chat
panel — natural-language config for any of them. Requires
`OPENAI_API_KEY` (or set `options(blockr.chat_function = ...)`).

This is a conversation seed for the May 2026 Swiss Re meeting,
not the final demo. Real treaty / cedant data plugs into the same
shape by swapping the three `new_static_block(...)` roots.

Implementation note: the crossfilter routes each shared dim to one
table only when no FK relationships are declared in the dm (the
last table listed in `active_dims` wins). The demo puts the
portfolio dims on `exposure`, and the events-side chains
(Accumulation / Stress) carry their own `filter_block` for scenario
selection.

## License

GPL (>= 3). Underlying data:

- `dataCar` from [insuranceData](https://cran.r-project.org/package=insuranceData) — accompanies de Jong & Heller, *Generalised Linear Models for Insurance Data* (Cambridge, 2008).
- `MW2014` from [ChainLadder](https://cran.r-project.org/package=ChainLadder) — accompanies Wüthrich & Merz, *Stochastic Claims Reserving Manual* (2014).

The `motor_portfolio` and `motor_losses` cubes are synthesised on top of these. The synthesis script (`data-raw/build.R`) is seeded and reproducible.
