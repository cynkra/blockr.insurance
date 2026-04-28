# Demo data — sources, synthesis, schemas

The package ships two datasets in `../data/`. They are generated from public
CRAN sources by `build.R` and lazy-loaded via `data(motor_portfolio)` /
`data(motor_losses)`. **Do not edit the `.rda` files directly** — re-run
`build.R`.

## Sources

| Source | Package (CRAN) | Role | Reference |
|---|---|---|---|
| `dataCar` | `insuranceData` | Policy-level base: 67,856 motor policies, 11 columns | de Jong & Heller, *Generalised Linear Models for Insurance Data* (Cambridge, 2008) |
| `MW2014` | `ChainLadder` | Loss-development pattern: 17×17 cumulative paid triangle | Wüthrich & Merz, *Stochastic Claims Reserving Manual* (2014) |

Both install with plain `install.packages()` — no custom repos. Both are
textbook-canonical, recognisable to anyone who has worked through standard
pricing and reserving courses.

## Synthesis

`dataCar` is single-year and single-insurer; `MW2014` is an aggregated
triangle with no segmentation. We need a multi-insurer, multi-year,
segmented portfolio plus a per-segment triangle. The build script adds the
missing dimensions on top.

| Dimension | How it's added |
|---|---|
| `Year` (2019–2024) | Replicate `dataCar` across 6 years, with year-specific exposure drift (~±10%) |
| `Insurance_Company` (`Company_01` … `Company_10`) | Random partition of policies, with per-company premium and loss multipliers |
| `Fleet` (Fleet / Non-Fleet) | Random binary, ~10/90 split |
| `Cover` (Comprehensive / Third-Party) | Random binary, ~60/40 split; Third-Party priced ~40% lower |
| `DY0` … `DY15` | Each segment's `Total_Incurred` is spread across 16 development years using `MW2014`'s average paid-to-ultimate proportions |

`Vehicle_type`, `Age_Class`, `Gender` come straight from `dataCar` (renamed
from `veh_body`, `agecat`, `gender`).

The script is seeded (`set.seed(42)`) and idempotent.

## Output schemas

Documented inline via roxygen — see `R/data.R` and the rendered help pages
(`?motor_portfolio`, `?motor_losses`).

## Reproduction

```r
# from package root
source("data-raw/build.R")
```

## Why these sources, not CASdatasets

`CASdatasets` contains `freMTPL2`, the actuarial canon for non-life pricing.
It's freely available but **not on CRAN** (size constraints — see
[CASdatasets repository](https://github.com/dutangc/CASdatasets)). To keep
the install path frictionless (`install.packages(c("insuranceData",
"ChainLadder"))`), we use CRAN-only sources. `dataCar` is the closest
plain-CRAN analogue with broad recognition; `MW2014` is the textbook-
canonical reserving triangle.

If you'd rather build on top of `freMTPL2`, the swap is a script-level
change to this file (replace the source loads, keep the synthesis steps for
Year / Company / Fleet / Cover).
