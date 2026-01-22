# blockr.insurance

Insurance datasets and example workflows for [blockr](https://github.com/BristolMyersSquibb/blockr).

## Installation

```r
# install.packages("remotes")
remotes::install_github("cynkra/blockr.insurance")
```

## Data

### freMTPL2 Frequency Dataset

The package includes the French Motor Third Party Liability (freMTPL2) frequency dataset from the [CASdatasets](http://cas.uqam.ca/) R package. This is a standard actuarial benchmark dataset with ~678K motor insurance policies.

```r
library(blockr.insurance)

# Load full dataset
data <- fremtpl2_freq()

# Load 10% sample for faster demos
sample_data <- fremtpl2_freq(sample = TRUE)
```

**Variables:**

| Variable | Description |
|----------|-------------|
| IDpol | Policy ID |
| ClaimNb | Number of claims |
| Exposure | Exposure in policy-years (0-1) |
| VehPower | Vehicle power category (4-15) |
| VehAge | Vehicle age in years |
| DrivAge | Driver age in years |
| BonusMalus | Bonus-malus coefficient (50-230) |
| VehBrand | Vehicle brand (B1-B14) |
| VehGas | Fuel type (Diesel, Regular) |
| Area | Area code (A=rural to F=urban) |
| Density | Population density |
| Region | French region code (R11-R94) |

## Example App

The package includes a portfolio analysis example demonstrating:

- Visual filter with interactive crossfiltering
- KPIs for total exposure and claims
- Pivot tables for segment breakdowns
- ggplot visualization
- Waterfall chart for portfolio metrics

### Run the Example

```r
library(blockr.insurance)
source(system.file("examples", "fremtpl2_portfolio.R", package = "blockr.insurance"))
```

### Live Demo

Visit [blockr.cloud](https://blockr.cloud/) and select "Insurance Portfolio".

## Dependencies

The example app uses:

- [blockr](https://github.com/BristolMyersSquibb/blockr) - Core framework
- [blockr.dplyr](https://github.com/BristolMyersSquibb/blockr.dplyr) - Data transformation blocks
- [blockr.bi](https://github.com/cynkra/blockr.bi) - BI blocks (visual filter, KPI, pivot table, waterfall)
- [blockr.ggplot](https://github.com/BristolMyersSquibb/blockr.ggplot) - ggplot2 visualization blocks
- [blockr.dag](https://github.com/BristolMyersSquibb/blockr.dag) - DAG visualization extension
- [blockr.session](https://github.com/BristolMyersSquibb/blockr.session) - Workflow save/load

## License
GPL (>= 3)
