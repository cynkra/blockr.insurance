test_that("fremtpl2_freq loads full data", {
  data <- fremtpl2_freq(sample = FALSE)
  expect_s3_class(data, "tbl_df")
  expect_equal(nrow(data), 677991)
  expect_equal(ncol(data), 12)
})

test_that("fremtpl2_freq loads sample data", {
  data <- fremtpl2_freq(sample = TRUE)
  expect_s3_class(data, "tbl_df")
  expect_true(nrow(data) > 0)
  expect_true(nrow(data) < 100000)
})

test_that("fremtpl2_freq sample is reproducible", {
  data1 <- fremtpl2_freq(sample = TRUE)
  data2 <- fremtpl2_freq(sample = TRUE)
  expect_equal(nrow(data1), nrow(data2))
  expect_equal(data1$IDpol[1], data2$IDpol[1])
})

test_that("fremtpl2_freq has expected columns", {
  data <- fremtpl2_freq(sample = TRUE)
  expected_cols <- c(
    "IDpol", "ClaimNb", "Exposure", "VehPower", "VehAge", "DrivAge",
    "BonusMalus", "VehBrand", "VehGas", "Area", "Density", "Region"
  )
  expect_equal(names(data), expected_cols)
})

test_that("fremtpl2_freq has correct column types", {
  data <- fremtpl2_freq(sample = TRUE)

 # Numeric columns
  expect_type(data$IDpol, "double")
  expect_type(data$ClaimNb, "double")
  expect_type(data$Exposure, "double")
  expect_type(data$VehPower, "double")
  expect_type(data$DrivAge, "double")
  expect_type(data$BonusMalus, "double")
  expect_type(data$Density, "double")

 # Character columns
  expect_type(data$VehBrand, "character")
  expect_type(data$VehGas, "character")
  expect_type(data$Area, "character")
  expect_type(data$Region, "character")
})

test_that("fremtpl2_freq data has valid ranges", {
  data <- fremtpl2_freq(sample = TRUE)

  # Exposure should be non-negative (typically 0-1, but can exceed 1)
  expect_true(all(data$Exposure >= 0))
  expect_true(all(data$Exposure <= 2))

  # ClaimNb should be non-negative
 expect_true(all(data$ClaimNb >= 0))

  # Area should be A-F
  expect_true(all(data$Area %in% c("A", "B", "C", "D", "E", "F")))

  # VehGas should be Diesel or Regular
  expect_true(all(data$VehGas %in% c("Diesel", "Regular")))
})
