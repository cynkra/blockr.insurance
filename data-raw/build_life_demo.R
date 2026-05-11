# Build the life-underwriting demo datasets (see
# `dev/life-underwriting-ws3.R`):
#
#   data/uw_factors.rda            — 3 rows (one per coverage_type);
#                                     UWR-editable underwriting factors
#   data/coverages.rda             — ~4,500 rows: one per (person x coverage)
#                                     for the single policy being priced
#   data/incidence_by_coverage.rda — VBT 2015 extended with per-coverage
#                                     multipliers (Death / Disability / CI)
#   data/country_adjustment.rda    — 5 EU countries x adjustment factor
#   data/annuity_2pct.rda          — capitalization factors derived from
#                                     vbt_2015 at 2% interest
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

n_lives <- 1500
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

# Each insured life gets all three coverages. sum_at_risk varies by
# coverage type (death is the headline benefit; disability is similar
# magnitude; CI is a lump sum, smaller).
coverage_mult <- c(Death = 1.00, Disability = 0.80, CriticalIllness = 0.30)

coverages <- merge(lives, data.frame(coverage_type = coverage_types),
                   by = NULL)
coverages$coverage_type <- factor(coverages$coverage_type,
                                  levels = coverage_types)
coverages$sum_at_risk   <- coverages$base_sum *
                          coverage_mult[as.character(coverages$coverage_type)]
coverages$additive_rate <- 0  # UWR can flag specific-risk additions later
coverages$base_sum      <- NULL

coverages <- tibble::as_tibble(coverages[order(coverages$person_id,
                                               coverages$coverage_type), ])

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

save(uw_factors,            file = "data/uw_factors.rda",            compress = "xz")
save(coverages,             file = "data/coverages.rda",             compress = "xz")
save(incidence_by_coverage, file = "data/incidence_by_coverage.rda", compress = "xz")
save(country_adjustment,    file = "data/country_adjustment.rda",    compress = "xz")
save(annuity_2pct,          file = "data/annuity_2pct.rda",          compress = "xz")

cat("uw_factors:            ", nrow(uw_factors),            " rows\n", sep = "")
cat("coverages:             ", nrow(coverages),             " rows\n", sep = "")
cat("incidence_by_coverage: ", nrow(incidence_by_coverage), " rows\n", sep = "")
cat("country_adjustment:    ", nrow(country_adjustment),    " rows\n", sep = "")
cat("annuity_2pct:          ", nrow(annuity_2pct),          " rows\n", sep = "")
