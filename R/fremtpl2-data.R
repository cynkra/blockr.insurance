#' Load freMTPL2 Frequency Data
#'
#' Loads the French Motor Third Party Liability (freMTPL2) frequency dataset,
#' containing ~678K motor insurance policies with claim counts.
#'
#' @param sample If TRUE, returns a 10% sample for faster demos. Default FALSE.
#'
#' @return A tibble with the following columns:
#' \describe{
#'   \item{IDpol}{Policy ID}
#'   \item{ClaimNb}{Number of claims}
#'   \item{Exposure}{Exposure in policy-years (0-1)}
#'   \item{VehPower}{Vehicle power category (4-15)}
#'   \item{VehAge}{Vehicle age in years}
#'   \item{DrivAge}{Driver age in years}
#'   \item{BonusMalus}{Bonus-malus coefficient (50-230)}
#'   \item{VehBrand}{Vehicle brand (B1-B14)}
#'   \item{VehGas}{Fuel type (Diesel, Regular)}
#'   \item{Area}{Area code (A=rural to F=urban)}
#'   \item{Density}{Population density in the area}
#'   \item{Region}{French region code (R11-R94)}
#' }
#'
#' @details
#' The freMTPL2 dataset is a standard actuarial benchmark dataset from the
#' CASdatasets R package. It contains French motor third-party liability
#' insurance data suitable for frequency modeling.
#'
#' @export
#'
#' @examples
#' # Load full dataset
#' data <- fremtpl2_freq()
#' nrow(data)
#'
#' # Load 10% sample for quick demos
#' sample_data <- fremtpl2_freq(sample = TRUE)
#' nrow(sample_data)
fremtpl2_freq <- function(sample = FALSE) {
  path <- system.file("extdata", "freMTPL2freq.csv", package = "blockr.insurance")
  data <- readr::read_csv(path, show_col_types = FALSE)
  if (sample) {
    set.seed(42)
    data <- dplyr::slice_sample(data, prop = 0.1)
  }
  data
}
