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

test_that("fremtpl2_freq data has valid values", {
  data <- fremtpl2_freq(sample = TRUE)

  # Exposure should be non-negative
  expect_true(all(data$Exposure >= 0))

  # ClaimNb should be non-negative
  expect_true(all(data$ClaimNb >= 0))

  # VehPower should be in expected range
  expect_true(all(data$VehPower >= 4))
  expect_true(all(data$VehPower <= 15))

  # DrivAge should be reasonable
  expect_true(all(data$DrivAge >= 18))
  expect_true(all(data$DrivAge <= 100))
})
