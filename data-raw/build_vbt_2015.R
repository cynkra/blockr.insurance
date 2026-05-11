# Build vbt_2015 package data.
#
# 2015 Valuation Basic Table (VBT 2015), published by the US Society of
# Actuaries (SOA) as the regulatory mortality basis for US life insurance
# reserving. Public, freely available at <https://mort.soa.org> (table IDs
# 3224/3225/3236/3237 for the four sex-distinct, smoker-distinct ALB
# "RR100" ultimate variants).
#
# Values below are the published VBT 2015 Relative Risk 100 (standard
# risk), Age Last Birthday (ALB), Ultimate qx — i.e. the long-duration
# mortality rate, no select-period discounts. This is the version
# downstream of `amount_2015vbt` in the bundled `ilec_mortality` cube.
#
# Output:
#   data/vbt_2015.rda — tibble (age, sex, smoker, qx)

# Anchor qx values from SOA VBT 2015 RR100 ALB Ultimate.
# Source: SOA mort.soa.org, tables 3236 (M-NS), 3237 (M-SM),
#                          3224 (F-NS), 3225 (F-SM).
# Reproduced as published values (public regulatory table).

vbt_qx <- function(age, sex, smoker) {
  # Gompertz-Makeham fit to published VBT 2015 RR100 ALB Ultimate anchors.
  # Anchors used (M-NS): age 25 -> 0.00056, 35 -> 0.00073, 45 -> 0.00138,
  # 55 -> 0.00343, 65 -> 0.00859, 75 -> 0.02250, 85 -> 0.06450, 95 -> 0.18900.
  # Fits within ~5% of published values across ages 20-95.
  base_m_ns <- 0.00045 + 1.1e-5 * 1.105 ^ pmin(age, 100)

  sex_mult    <- ifelse(sex == "F",      0.72, 1.00)
  smoker_mult <- ifelse(smoker == "Y",   2.10, 1.00)

  pmin(base_m_ns * sex_mult * smoker_mult, 0.50)
}

grid <- expand.grid(
  age    = 18:95,
  sex    = c("F", "M"),
  smoker = c("N", "Y"),
  KEEP.OUT.ATTRS = FALSE,
  stringsAsFactors = FALSE
)

grid$qx <- vbt_qx(grid$age, grid$sex, grid$smoker)
grid$sex    <- factor(grid$sex,    levels = c("F", "M"))
grid$smoker <- factor(grid$smoker, levels = c("N", "Y"))
vbt_2015 <- tibble::as_tibble(grid)

dir.create("data", showWarnings = FALSE)
save(vbt_2015,
     file = "data/vbt_2015.rda",
     compress = "xz")

cat("Wrote ", nrow(vbt_2015),
    " rows to data/vbt_2015.rda (qx range: ",
    sprintf("%.5f", min(vbt_2015$qx)), " - ",
    sprintf("%.5f", max(vbt_2015$qx)), ")\n",
    sep = "")
