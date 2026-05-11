# Build the life-underwriting demo datasets (see
# `dev/life-underwriting-ws3.R`):
#
#   data/uw_factors.rda            — 3 rows (one per coverage_type);
#                                     UWR-editable underwriting factors
#   data/employees.rda             — 1,500 rows (one per insured employee)
#                                     with three sum_at_risk columns, one
#                                     per coverage. Pivoted to long format
#                                     in the pipeline to give the 4,500-row
#                                     per-(person x coverage) frame.
#   data/life_claims.rda           — ~80 rows of historical claims for the
#                                     company being underwritten (experience)
#   data/incidence_by_coverage.rda — VBT 2015 extended with per-coverage
#                                     multipliers (Death / Disability / CI)
#   data/country_adjustment.rda    — 5 EU countries x adjustment factor
#   data/annuity_2pct.rda          — capitalization factors derived from
#                                     vbt_2015 at 2% interest
#
# Also writes CSV copies of the two "uploadable" tables under inst/extdata/
# so the workspace's `new_read_block(source = "path")` defaults to a real
# file. The UWR can swap to upload-mode via the cogwheel.
#
# Note on policies: the workspace prices ONE policy at a time (think one
# employer submission, one group-life treaty). The census upload is the
# employees covered by that single policy, so we don't carry a policy_id —
# everything in `coverages` belongs to "the" policy.
#
# Run from the package root after data-raw/build_vbt_2015.R has produced
# data/vbt_2015.rda.

set.seed(42)

load("data/vbt_2015.rda")  # vbt_2015 (age, sex, smoker, qx)

# 1) uw_factors ---------------------------------------------------------
#
# The UWR's worksheet for the one policy being priced. UW factors live
# per coverage_type: the same submission can carry a high uw_occupation
# loading on Disability (roofers, scaffolders) while staying neutral on
# Death and CriticalIllness. Three rows total; the grid block edits them.

coverage_types <- c("Death", "Disability", "CriticalIllness")

uw_factors <- tibble::tibble(
  coverage_type = factor(coverage_types, levels = coverage_types),
  uw_health     = 1,
  uw_occupation = 1,
  uw_hobby      = 1
)

# 2) coverages ----------------------------------------------------------
#
# Main pipeline input. One row per (person, coverage_type). Demographic
# columns (dob, sex, smoker, country) are person-level but replicated
# across that person's coverage rows for join simplicity.

n_lives <- 750
countries <- c("DE", "ES", "FR", "GB", "IT")

dob <- as.Date("2026-05-11") -
  as.integer(stats::rbeta(n_lives, 2.4, 2.1) * (95 - 18) + 18) * 365.25 -
  sample.int(365, n_lives, replace = TRUE)

lives <- tibble::tibble(
  person_id    = sprintf("P%05d", seq_len(n_lives)),
  dob          = dob,
  sex          = factor(sample(c("F", "M"), n_lives, replace = TRUE, prob = c(0.46, 0.54)),
                        levels = c("F", "M")),
  smoker       = factor(sample(c("N", "Y"), n_lives, replace = TRUE, prob = c(0.83, 0.17)),
                        levels = c("N", "Y")),
  country      = factor(sample(countries, n_lives, replace = TRUE,
                               prob = c(0.30, 0.22, 0.18, 0.16, 0.14)),
                        levels = countries),
  base_sum     = round(stats::rlnorm(n_lives, meanlog = log(150000), sdlog = 0.7) / 1000) * 1000
)

# Wide format — one row per employee, three sum_at_risk columns (one per
# coverage). The workspace pivots this to long form (4,500 rows) so the
# grid stays at the human-readable 1,500-row "list of employees" grain.
coverage_mult <- c(Death = 1.00, Disability = 0.80, CriticalIllness = 0.30)

employees <- tibble::tibble(
  person_id        = lives$person_id,
  dob              = lives$dob,
  sex              = lives$sex,
  smoker           = lives$smoker,
  country          = lives$country,
  additive_rate    = 0,             # UWR can flag per-person specific-risk loads
  sum_Death           = lives$base_sum * coverage_mult[["Death"]],
  sum_Disability      = lives$base_sum * coverage_mult[["Disability"]],
  sum_CriticalIllness = lives$base_sum * coverage_mult[["CriticalIllness"]]
)

# 3) incidence_by_coverage ----------------------------------------------
#
# Pipeline-ready incidence rates. Death = VBT 2015 qx unchanged.
# Disability and Critical Illness use VBT-derived proxies (real morbidity
# tables are insurer-proprietary; multipliers below are demo-grade).

cov_mult <- c(Death = 1.0, Disability = 1.5, CriticalIllness = 0.5)

incidence_by_coverage <- do.call(rbind, lapply(names(cov_mult), function(ct) {
  out <- vbt_2015
  out$coverage_type <- factor(ct, levels = coverage_types)
  out$incidence_rate <- out$qx * cov_mult[[ct]]
  out$qx <- NULL
  out
}))
incidence_by_coverage <- tibble::as_tibble(incidence_by_coverage)
incidence_by_coverage <- incidence_by_coverage[, c("coverage_type", "age", "sex", "smoker", "incidence_rate")]

# 4) country_adjustment --------------------------------------------------

country_adjustment <- tibble::tibble(
  country    = factor(countries, levels = countries),
  adjustment = unname(c(DE = 1.00, ES = 1.05, FR = 0.95, GB = 0.92, IT = 1.10))
)

# 5) annuity_2pct --------------------------------------------------------
#
# Capitalization factor at age x = sum over k of (k_px) / (1 + i)^k with
# i = 2% and survival from vbt_2015 ultimate (non-smoker basis).

i <- 0.02
v <- 1 / (1 + i)

build_annuity <- function(sex_in) {
  qx <- subset(vbt_2015, sex == sex_in & smoker == "N")
  qx <- qx[order(qx$age), ]
  ages <- qx$age
  px   <- 1 - qx$qx

  cap <- numeric(length(ages))
  for (j in seq_along(ages)) {
    surv <- cumprod(px[j:length(px)])
    yrs  <- seq_along(surv) - 1L
    cap[j] <- sum(surv * v ^ yrs)
  }

  tibble::tibble(age = ages, sex = factor(sex_in, levels = c("F", "M")),
                 capitalization_factor = cap)
}

annuity_2pct <- rbind(build_annuity("F"), build_annuity("M"))

# 6) life_claims --------------------------------------------------------
#
# Historical claims experience for the company being underwritten.
# Synthetic; ~80 rows across the past 5 years. The UWR uploads this and
# may tweak rows (e.g. to mark a claim as outlier or update an amount).

n_claims <- 80
claim_year <- sample(2021:2025, n_claims, replace = TRUE)
claim_cov  <- sample(coverage_types, n_claims, replace = TRUE,
                     prob = c(0.45, 0.35, 0.20))
claim_persons <- sample(lives$person_id, n_claims, replace = FALSE)

# Claim amount roughly proportional to sum_at_risk for that person/coverage
claim_amount <- numeric(n_claims)
for (k in seq_len(n_claims)) {
  pid  <- claim_persons[k]
  cov  <- claim_cov[k]
  base <- lives$base_sum[lives$person_id == pid]
  mult <- c(Death = 1.0, Disability = 0.80, CriticalIllness = 0.30)[cov]
  partial <- stats::runif(1, 0.4, 1.0)   # partial-loss factor
  claim_amount[k] <- round(base * mult * partial / 1000) * 1000
}

life_claims <- tibble::tibble(
  claim_id      = sprintf("CL%04d", seq_len(n_claims)),
  year          = claim_year,
  coverage_type = factor(claim_cov, levels = coverage_types),
  person_id     = claim_persons,
  claim_amount  = claim_amount
)
life_claims <- life_claims[order(life_claims$year, life_claims$claim_id), ]

# Save -------------------------------------------------------------------

dir.create("data", showWarnings = FALSE)

# Old single-table life_census + per-policy policies table are superseded
# by the uw_factors / coverages split.
for (old in c("data/life_census.rda", "data/policies.rda")) {
  if (file.exists(old)) {
    file.remove(old)
    cat("Removed obsolete ", old, "\n", sep = "")
  }
}

# Old long-format coverages table is superseded by the wide employees table.
if (file.exists("data/coverages.rda")) {
  file.remove("data/coverages.rda")
  cat("Removed obsolete data/coverages.rda\n")
}

save(uw_factors,            file = "data/uw_factors.rda",            compress = "xz")
save(employees,             file = "data/employees.rda",             compress = "xz")
save(life_claims,           file = "data/life_claims.rda",           compress = "xz")
save(incidence_by_coverage, file = "data/incidence_by_coverage.rda", compress = "xz")
save(country_adjustment,    file = "data/country_adjustment.rda",    compress = "xz")
save(annuity_2pct,          file = "data/annuity_2pct.rda",          compress = "xz")

# CSV copies of the uploadable tables — referenced by `new_read_block()`
# in the workspace so the read block defaults to a real path.
dir.create("inst/extdata", showWarnings = FALSE, recursive = TRUE)
if (file.exists("inst/extdata/coverages.csv")) {
  file.remove("inst/extdata/coverages.csv")
}
utils::write.csv(employees,   "inst/extdata/employees.csv",   row.names = FALSE)
utils::write.csv(life_claims, "inst/extdata/life_claims.csv", row.names = FALSE)

cat("uw_factors:            ", nrow(uw_factors),            " rows\n", sep = "")
cat("employees:             ", nrow(employees),             " rows\n", sep = "")
cat("life_claims:           ", nrow(life_claims),           " rows\n", sep = "")
cat("incidence_by_coverage: ", nrow(incidence_by_coverage), " rows\n", sep = "")
cat("country_adjustment:    ", nrow(country_adjustment),    " rows\n", sep = "")
cat("annuity_2pct:          ", nrow(annuity_2pct),          " rows\n", sep = "")
cat("\nCSV exports under inst/extdata/: employees.csv, life_claims.csv\n")
