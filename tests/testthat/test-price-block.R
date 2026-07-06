# Tests for new_price_block — runtime engine selection.

test_that("new_price_block constructor creates correct class", {
  blk <- new_price_block()
  expect_s3_class(blk, c("price_block", "dm_block", "transform_block", "block"))
})

test_that("new_price_block default engine is the first available", {
  expect_identical(available_engines()[[1L]], "engine_property")
})

test_that("new_price_block rejects engines not in available_engines()", {
  expect_error(
    new_price_block(engine = "nonexistent_engine"),
    "must be one of"
  )
})

test_that("new_price_block accepts engine_property_v2", {
  blk <- new_price_block(engine = "engine_property_v2")
  expect_s3_class(blk, "price_block")
})

test_that("available_engines lists both v1 and v2", {
  eng <- available_engines()
  expect_setequal(eng, c("engine_property", "engine_property_v2"))
})

# ---- testServer-level: engine toggle re-evaluates the block expression -----

test_that("new_price_block runs the default engine and produces a dm with premium", {
  block <- new_price_block()
  inputs_dm <- dm::as_dm(list(
    locations = property_locations,
    claims    = property_claims
  ))

  testServer(
    blockr.core:::get_s3_method("block_server", block),
    {
      session$flushReact()
      result <- session$returned$result()
      expect_s3_class(result, "dm")
      tbls <- dm::dm_get_tables(result)
      expect_true("premium" %in% names(tbls))
      expect_equal(nrow(tbls$premium), nrow(property_locations))
    },
    args = list(
      x = block,
      data = list(...args = reactiveValues(inputs = inputs_dm))
    )
  )
})

test_that("new_price_block engine state updates when input$engine_select changes", {
  block <- new_price_block(engine = "engine_property")
  inputs_dm <- dm::as_dm(list(
    locations = property_locations,
    claims    = property_claims
  ))

  testServer(
    blockr.core:::get_s3_method("block_server", block),
    {
      session$flushReact()
      expect_identical(session$returned$state$engine(), "engine_property")

      session$makeScope("expr")$setInputs(engine_select = "engine_property_v2")
      session$flushReact()
      expect_identical(session$returned$state$engine(), "engine_property_v2")
    },
    args = list(
      x = block,
      data = list(...args = reactiveValues(inputs = inputs_dm))
    )
  )
})

test_that("new_price_block result switches numerically when engine toggles", {
  block <- new_price_block(engine = "engine_property")
  inputs_dm <- dm::as_dm(list(
    locations = property_locations,
    claims    = property_claims
  ))

  testServer(
    blockr.core:::get_s3_method("block_server", block),
    {
      session$flushReact()

      v1 <- dm::dm_get_tables(session$returned$result())$premium
      v1_italy <- v1$model_price[v1$country == "Italy"][1]

      session$makeScope("expr")$setInputs(engine_select = "engine_property_v2")
      session$flushReact()

      v2 <- dm::dm_get_tables(session$returned$result())$premium
      v2_italy <- v2$model_price[v2$country == "Italy"][1]

      # Italy on v2 is 1.30x v1 (CAT loading)
      expect_equal(v2_italy / v1_italy, 1.30, tolerance = 1e-9)
    },
    args = list(
      x = block,
      data = list(...args = reactiveValues(inputs = inputs_dm))
    )
  )
})
