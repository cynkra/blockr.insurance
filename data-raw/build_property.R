# Build property_locations + property_claims + property_params +
# property_params_comparison package data.
#
# Run from the package root:
#   Rscript data-raw/build_property.R
#
# No external sources — small, deterministic synthetic data so the example
# can demonstrate the property rating engine end-to-end.
#
# Outputs:
#   data/property_locations.rda
#   data/property_claims.rda
#   data/property_params.rda
#   data/property_params_comparison.rda
#   data/property_param_country_factor.rda
#   data/property_param_base_rate.rda
#   data/property_param_cat_factor.rda
#   data/property_param_expenses.rda

set.seed(1)

countries <- c("Italy", "Spain", "United Kingdom", "France", "Germany")
perils    <- c("fire", "flood", "theft")

n_loc <- 20L
property_locations <- data.frame(
  location_id = sprintf("LOC%03d", seq_len(n_loc)),
  country     = sample(countries, n_loc, replace = TRUE),
  peril       = sample(perils,    n_loc, replace = TRUE),
  postcode    = sample(1000:99999, n_loc),
  tiv         = round(10^runif(n_loc, 5, 7) / 1e3) * 1e3,
  sra_adj     = sample(c(0.9, 1.0, 1.0, 1.0, 1.1, 1.2),
                       n_loc, replace = TRUE),
  deductible  = sample(c(0, 500, 1000, 5000, 10000),
                       n_loc, replace = TRUE)
)
property_locations$limit <- pmax(
  property_locations$deductible + 50000,
  round(property_locations$tiv * runif(n_loc, 0.3, 1.0) / 1e3) * 1e3
)

n_clm <- 15L
property_claims <- data.frame(
  claim_id     = sprintf("CL%04d", seq_len(n_clm)),
  date_of_loss = as.Date("2020-01-01") +
                 sort(sample(0:1825, n_clm)),
  country      = sample(countries, n_clm, replace = TRUE),
  paid         = round(runif(n_clm, 1000, 200000)),
  outstanding  = round(runif(n_clm, 0, 100000))
)

property_params <- list(
  country_factor = data.frame(
    country        = countries,
    country_factor = c(1.10, 0.95, 1.00, 1.05, 0.90)
  ),
  base_rate = data.frame(
    country   = countries,
    base_rate = c(0.0015, 0.0012, 0.0010, 0.0013, 0.0009)
  ),
  cat_factor = data.frame(
    country    = countries,
    cat_factor = c(1.30, 1.05, 1.15, 1.10, 1.00)
  ),
  expenses = data.frame(
    producing_country = "Germany",
    expenses_factor   = 1.25
  )
)

# Comparison-scenario params: Italian base_rate +30%, leaves the rest alone.
# This is the alternative rate set the portfolio-explorer demo compares the
# base run against.
property_params_comparison <- property_params
property_params_comparison$base_rate$base_rate[
  property_params_comparison$base_rate$country == "Italy"
] <- property_params$base_rate$base_rate[
  property_params$base_rate$country == "Italy"
] * 1.30

dir.create("data", showWarnings = FALSE)
save(property_locations,
     file = "data/property_locations.rda",
     compress = "xz")
save(property_claims,
     file = "data/property_claims.rda",
     compress = "xz")
save(property_params,
     file = "data/property_params.rda",
     compress = "xz")
save(property_params_comparison,
     file = "data/property_params_comparison.rda",
     compress = "xz")

# Individual param tables — exposed as standalone datasets so the property
# workbench can load each via new_dataset_block() rather than new_static_block().
# Dataset blocks store the dataset name, not the data, so they round-trip
# through save/load cleanly.
property_param_country_factor <- property_params$country_factor
property_param_base_rate      <- property_params$base_rate
property_param_cat_factor     <- property_params$cat_factor
property_param_expenses       <- property_params$expenses
save(property_param_country_factor,
     file = "data/property_param_country_factor.rda", compress = "xz")
save(property_param_base_rate,
     file = "data/property_param_base_rate.rda",      compress = "xz")
save(property_param_cat_factor,
     file = "data/property_param_cat_factor.rda",     compress = "xz")
save(property_param_expenses,
     file = "data/property_param_expenses.rda",       compress = "xz")

cat("Wrote ", nrow(property_locations),
    " rows to data/property_locations.rda\n", sep = "")
cat("Wrote ", nrow(property_claims),
    " rows to data/property_claims.rda\n", sep = "")
cat("Wrote params list with ", length(property_params),
    " tables to data/property_params.rda\n", sep = "")
cat("Wrote comparison params (Italian base_rate +30%) to ",
    "data/property_params_comparison.rda\n", sep = "")
