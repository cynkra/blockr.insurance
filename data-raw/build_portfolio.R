# Build the bundled fixture portfolios under inst/extdata/.
#
# Run from the package root:
#   Rscript data-raw/build_portfolio.R
#
# The two fixtures share inputs verbatim — same locations, same claims —
# and differ only by the parameter set the engine runs with. This is the
# "rate-change study" comparison shape: identical book, two pricing
# scenarios, so per-location keys match exactly and the per-location diff
# is meaningful.
#
#   inst/extdata/portfolio-property/             # base       (property_params)
#   inst/extdata/portfolio-property-comparison/  # comparison (property_params_comparison)

pkgload::load_all(".")

set.seed(42)

countries <- c("Italy", "Spain", "United Kingdom", "France", "Germany")
perils    <- c("fire", "flood", "theft")

recipes <- list(
  list(id = "policy-001", n_loc = 8L,  n_clm =  3L,
       countries = c("United Kingdom"),
       tiv_log10 = c(5.0, 5.8), claim_paid = c(2e3,  80e3)),
  list(id = "policy-002", n_loc = 18L, n_clm =  9L,
       countries = c("Italy", "Spain", "France"),
       tiv_log10 = c(5.5, 6.5), claim_paid = c(5e3, 120e3)),
  list(id = "policy-003", n_loc = 6L,  n_clm =  5L,
       countries = c("Italy"),
       tiv_log10 = c(6.5, 7.2), claim_paid = c(50e3, 400e3)),
  list(id = "policy-004", n_loc = 25L, n_clm = 12L,
       countries = c("France"),
       tiv_log10 = c(5.0, 6.0), claim_paid = c(1e3,  60e3)),
  list(id = "policy-005", n_loc = 12L, n_clm = 18L,
       countries = c("Germany", "France"),
       tiv_log10 = c(6.0, 6.8), claim_paid = c(20e3, 250e3))
)

draw_locations <- function(rcp) {
  n   <- rcp$n_loc
  tiv <- round(10^runif(n, rcp$tiv_log10[1L], rcp$tiv_log10[2L]) / 1e3) * 1e3
  ded <- sample(c(0, 500, 1000, 5000, 10000), n, replace = TRUE)
  data.frame(
    location_id = sprintf("LOC%03d", seq_len(n)),
    country     = sample(rcp$countries, n, replace = TRUE),
    peril       = sample(perils,        n, replace = TRUE),
    postcode    = sample(1000:99999, n),
    tiv         = tiv,
    sra_adj     = sample(c(0.9, 1.0, 1.0, 1.0, 1.1, 1.2),
                         n, replace = TRUE),
    deductible  = ded,
    limit       = pmax(ded + 50000,
                       round(tiv * runif(n, 0.3, 1.0) / 1e3) * 1e3),
    stringsAsFactors = FALSE
  )
}

draw_claims <- function(rcp) {
  n <- rcp$n_clm
  if (!n) {
    return(data.frame(
      claim_id     = character(),
      date_of_loss = as.Date(character()),
      country      = character(),
      paid         = numeric(),
      outstanding  = numeric(),
      stringsAsFactors = FALSE
    ))
  }
  data.frame(
    claim_id     = sprintf("CL%04d", seq_len(n)),
    date_of_loss = as.Date("2020-01-01") + sort(sample(0:1825, n)),
    country      = sample(rcp$countries, n, replace = TRUE),
    paid         = round(runif(n, rcp$claim_paid[1L], rcp$claim_paid[2L])),
    outstanding  = round(runif(n, 0, rcp$claim_paid[2L] / 3)),
    stringsAsFactors = FALSE
  )
}

# Step 1: draw inputs once, write into the BASE portfolio folder. ----------
base_root       <- file.path("inst", "extdata", "portfolio-property")
comparison_root <- file.path("inst", "extdata", "portfolio-property-comparison")

unlink(base_root,       recursive = TRUE)
unlink(comparison_root, recursive = TRUE)
dir.create(base_root,       recursive = TRUE, showWarnings = FALSE)
dir.create(comparison_root, recursive = TRUE, showWarnings = FALSE)

for (rcp in recipes) {
  in_dir <- file.path(base_root, rcp$id, "inputs")
  dir.create(in_dir, recursive = TRUE, showWarnings = FALSE)
  utils::write.csv(draw_locations(rcp),
                   file.path(in_dir, "locations.csv"),
                   row.names = FALSE)
  utils::write.csv(draw_claims(rcp),
                   file.path(in_dir, "claims.csv"),
                   row.names = FALSE)
}

# Step 2: copy the base inputs verbatim into the comparison portfolio. ------
file.copy(
  list.files(base_root, full.names = TRUE),
  comparison_root,
  recursive = TRUE
)
# Wipe any outputs we may have copied (shouldn't be any yet, but be safe).
for (pid in list.files(comparison_root)) {
  out_dir <- file.path(comparison_root, pid, "outputs")
  if (dir.exists(out_dir)) unlink(out_dir, recursive = TRUE)
}

# Step 3: run engine on each side with the appropriate params. --------------
done_base <- run_portfolio(base_root,
                           engine    = engine_property,
                           params    = property_params,
                           overwrite = TRUE)
cat("Wrote ", length(done_base), " policies under ", base_root, "\n", sep = "")

done_comp <- run_portfolio(comparison_root,
                           engine    = engine_property,
                           params    = property_params_comparison,
                           overwrite = TRUE)
cat("Wrote ", length(done_comp), " policies under ", comparison_root,
    "\n", sep = "")

cat("\n--- base overview ---\n")
print(read_portfolio_overview(base_root), row.names = FALSE)
cat("\n--- comparison overview ---\n")
print(read_portfolio_overview(comparison_root), row.names = FALSE)

# Diff sanity-check: model_price diff should be zero outside Italy and
# positive on Italian locations.
b <- read_portfolio_premium(base_root)
c <- read_portfolio_premium(comparison_root)
diff_by_country <- merge(
  aggregate(model_price ~ country, data = b, FUN = sum),
  aggregate(model_price ~ country, data = c, FUN = sum),
  by = "country", suffixes = c("_base", "_comp")
)
diff_by_country$diff <- diff_by_country$model_price_comp -
                       diff_by_country$model_price_base
cat("\n--- model_price diff (comparison - base) by country ---\n")
print(diff_by_country, row.names = FALSE)
