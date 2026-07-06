# Unit tests for engine_property_v2 — CAT-loaded variant of engine_property.

inp <- list(
  locations = blockr.insurance::property_locations,
  claims    = blockr.insurance::property_claims
)

test_that("engine_property_v2 returns same schema as engine_property plus cat_factor", {
  v1 <- engine_property(inp)$premium
  v2 <- engine_property_v2(inp)$premium
  expect_setequal(names(v2), c(names(v1), "cat_factor"))
  expect_equal(nrow(v2), nrow(v1))
})

test_that("engine_property_v2 model_price equals v1 model_price * cat_factor", {
  v1 <- engine_property(inp)$premium
  v2 <- engine_property_v2(inp)$premium
  m <- merge(
    v1[, c("location_id", "model_price")],
    v2[, c("location_id", "model_price", "cat_factor")],
    by = "location_id", suffixes = c("_v1", "_v2")
  )
  expect_equal(m$model_price_v2, m$model_price_v1 * m$cat_factor,
               tolerance = 1e-10)
})

test_that("engine_property_v2 risk_premium equals v1 risk_premium * cat_factor", {
  v1 <- engine_property(inp)$premium
  v2 <- engine_property_v2(inp)$premium
  m <- merge(
    v1[, c("location_id", "risk_premium")],
    v2[, c("location_id", "risk_premium", "cat_factor")],
    by = "location_id", suffixes = c("_v1", "_v2")
  )
  expect_equal(m$risk_premium_v2, m$risk_premium_v1 * m$cat_factor,
               tolerance = 1e-10)
})

test_that("engine_property_v2 leaves base_premium and exposure_premium unchanged", {
  v1 <- engine_property(inp)$premium
  v2 <- engine_property_v2(inp)$premium
  m <- merge(
    v1[, c("location_id", "base_premium", "exposure_premium")],
    v2[, c("location_id", "base_premium", "exposure_premium")],
    by = "location_id", suffixes = c("_v1", "_v2")
  )
  expect_equal(m$base_premium_v1, m$base_premium_v2, tolerance = 1e-10)
  expect_equal(m$exposure_premium_v1, m$exposure_premium_v2, tolerance = 1e-10)
})

test_that("engine_property_v2 collapses to v1 when cat_factor table is absent", {
  params_no_cat <- property_params
  params_no_cat$cat_factor <- NULL
  v1 <- engine_property(inp)$premium
  v2 <- engine_property_v2(inp, params = params_no_cat)$premium
  expect_equal(v2$model_price, v1$model_price, tolerance = 1e-10)
  expect_true(all(v2$cat_factor == 1))
})

test_that("engine_property_v2 high-CAT countries get loaded by their factor", {
  v2 <- engine_property_v2(inp)$premium
  italy <- v2[v2$country == "Italy", ]
  expect_true(all(italy$cat_factor == 1.30))
  germany <- v2[v2$country == "Germany", ]
  expect_true(all(germany$cat_factor == 1.00))
})

test_that("engine_property_v2 accepts dm inputs", {
  inp_dm <- dm::as_dm(inp)
  v2_list <- engine_property_v2(inp)$premium
  v2_dm   <- engine_property_v2(inp_dm)$premium
  expect_equal(v2_list, v2_dm)
})

test_that("engine_property_v2 with empty claims handles experience aggregation", {
  inp_no_claims <- inp
  inp_no_claims$claims <- inp$claims[0, ]
  v2 <- engine_property_v2(inp_no_claims)$premium
  expect_equal(unique(v2$total_incurred), 0)
  expect_equal(unique(v2$n_claim_years), 0L)
  # cat_factor still applies to model_price even with no claims
  expect_equal(
    v2$model_price[v2$country == "Italy"][1] /
      engine_property(inp_no_claims)$premium$model_price[
        v2$country == "Italy"
      ][1],
    1.30,
    tolerance = 1e-10
  )
})
