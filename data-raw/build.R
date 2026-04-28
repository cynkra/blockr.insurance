# Build motor_portfolio + motor_losses package data.
#
# Run from the package root:
#   Rscript data-raw/build.R
#
# Sources (plain CRAN):
#   - insuranceData::dataCar  (de Jong & Heller, 2008)
#   - ChainLadder::MW2014     (Wuthrich & Merz, 2014)
#
# Synthesis adds Year (2019-2024), Insurance_Company (Company_01..Company_10),
# Fleet, Cover, and a per-segment development triangle. See README.md for the
# full schema.
#
# Outputs:
#   data/motor_portfolio.rda
#   data/motor_losses.rda

suppressMessages({
  library(insuranceData)
  library(ChainLadder)
})

set.seed(42)

# ---- 1. load sources -------------------------------------------------------

data(dataCar, package = "insuranceData")
data(MW2014,  package = "ChainLadder")

# ---- 2. synthesis parameters -----------------------------------------------

companies <- sprintf("Company_%02d", 1:10)
years     <- 2019:2024

co_premium_mult <- setNames(runif(length(companies), 0.85, 1.15), companies)
co_loss_mult    <- setNames(runif(length(companies), 0.70, 1.40), companies)
year_drift      <- setNames(runif(length(years), 0.95, 1.10),
                            as.character(years))

# ---- 3. expand dataCar across years ----------------------------------------

df_long <- do.call(rbind, lapply(years, function(yr) {
  d <- dataCar
  d$Year     <- yr
  d$exposure <- d$exposure * year_drift[as.character(yr)]
  d
}))

n <- nrow(df_long)

# ---- 4. add synthetic columns ----------------------------------------------

df_long$Insurance_Company <- sample(companies, n, replace = TRUE)
df_long$Fleet             <- ifelse(runif(n) < 0.10, "Fleet", "Non-Fleet")
df_long$Cover             <- ifelse(runif(n) < 0.60, "Comprehensive",
                                                     "Third-Party")
df_long$Vehicle_type      <- as.character(df_long$veh_body)
df_long$Age_Class         <- as.character(df_long$agecat)
df_long$Gender            <- as.character(df_long$gender)

# ---- 5. premium and incurred (with company-specific multipliers) -----------

df_long$Premium <- with(df_long,
  exposure * veh_value * 1500 *
    co_premium_mult[Insurance_Company] *
    ifelse(Cover == "Comprehensive", 1.0, 0.6)
)
df_long$Claim_Incurred <- with(df_long,
  claimcst0 * co_loss_mult[Insurance_Company]
)

# ---- 6. portfolio cube -----------------------------------------------------

dim_cols <- c("Year", "Insurance_Company", "Fleet",
              "Vehicle_type", "Cover", "Age_Class", "Gender")

motor_portfolio <- aggregate(
  cbind(Vehicles = rep(1L, n), Premium = df_long$Premium),
  by = df_long[dim_cols],
  FUN = sum
)
motor_portfolio$Year     <- as.integer(motor_portfolio$Year)
motor_portfolio$Vehicles <- as.integer(motor_portfolio$Vehicles)

# ---- 7. losses cube (claims-only segments) ---------------------------------

claims <- df_long[df_long$numclaims > 0, ]
motor_losses <- aggregate(
  cbind(Num_Claims     = claims$numclaims,
        Total_Incurred = claims$Claim_Incurred),
  by = claims[dim_cols],
  FUN = sum
)
motor_losses$Year       <- as.integer(motor_losses$Year)
motor_losses$Num_Claims <- as.integer(motor_losses$Num_Claims)

# ---- 8. development pattern from MW2014 ------------------------------------
#
# For each development year k (1..K), compute the average ratio
# MW[origin, k] / MW[origin, K_max] across origins where both are observed.
# This gives the proportion of ultimate paid by end of dev year k.

mw <- as.matrix(MW2014)
K_max <- ncol(mw)
proportions <- vapply(seq_len(K_max), function(k) {
  ok <- !is.na(mw[, k]) & !is.na(mw[, K_max])
  mean(mw[ok, k] / mw[ok, K_max], na.rm = TRUE)
}, numeric(1))

DY_n <- 16L
prop16 <- proportions[seq_len(min(DY_n, length(proportions)))]
if (length(prop16) < DY_n) {
  prop16 <- c(prop16, rep(1, DY_n - length(prop16)))
}

# ---- 9. apply development to each segment ---------------------------------

for (k in 0:(DY_n - 1)) {
  motor_losses[[paste0("DY", k)]] <- motor_losses$Total_Incurred * prop16[k + 1]
}
motor_losses$Latest              <- motor_losses[[paste0("DY", DY_n - 1)]]
motor_losses$Reporting_Threshold <- 1e6

# ---- 10. write package data ------------------------------------------------

# usethis::use_data() is the conventional helper but adds a hard dep here;
# saveRDS to data/<name>.rda with compress = "xz" matches what use_data does.
# `data()` lazy-loads .rda files automatically when they live in pkg's data/.

dir.create("data", showWarnings = FALSE)
save(motor_portfolio,
     file = "data/motor_portfolio.rda",
     compress = "xz")
save(motor_losses,
     file = "data/motor_losses.rda",
     compress = "xz")

cat("Wrote ", nrow(motor_portfolio), " rows to data/motor_portfolio.rda\n",
    sep = "")
cat("Wrote ", nrow(motor_losses), " rows to data/motor_losses.rda\n",
    sep = "")
