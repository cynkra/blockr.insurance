test_that("fremtpl2_freq loads data", {
  data <- fremtpl2_freq(sample = TRUE)
  expect_s3_class(data, "tbl_df")
  expect_true(nrow(data) > 0)
  expect_true("ClaimNb" %in% names(data))
  expect_true("Exposure" %in% names(data))
})

test_that("fremtpl2_freq sample is reproducible", {
  data1 <- fremtpl2_freq(sample = TRUE)
  data2 <- fremtpl2_freq(sample = TRUE)
  expect_equal(nrow(data1), nrow(data2))
})
