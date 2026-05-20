# Tests for new_insurance_dataset_block — blockr.core::new_dataset_block
# variant with the package locked to blockr.insurance.

test_that("new_insurance_dataset_block constructor creates correct class", {
  blk <- new_insurance_dataset_block()
  expect_s3_class(
    blk,
    c("insurance_dataset_block", "dataset_block", "data_block", "block")
  )
})

test_that("new_insurance_dataset_block default dataset is motor_portfolio", {
  block <- new_insurance_dataset_block()

  testServer(
    blockr.core:::get_s3_method("block_server", block),
    {
      session$flushReact()
      expect_identical(session$returned$state$dataset(), "motor_portfolio")
      expect_identical(session$returned$state$package, "blockr.insurance")
    },
    args = list(x = block, data = list())
  )
})

test_that("new_insurance_dataset_block returns the selected dataset", {
  block <- new_insurance_dataset_block()

  testServer(
    blockr.core:::get_s3_method("block_server", block),
    {
      session$flushReact()
      result <- session$returned$result()
      expect_identical(result, motor_portfolio)
    },
    args = list(x = block, data = list())
  )
})

test_that("new_insurance_dataset_block dataset state updates on input change", {
  block <- new_insurance_dataset_block()

  testServer(
    blockr.core:::get_s3_method("block_server", block),
    {
      session$flushReact()
      expect_identical(session$returned$state$dataset(), "motor_portfolio")

      session$makeScope("expr")$setInputs(dataset = "motor_losses")
      session$flushReact()
      expect_identical(session$returned$state$dataset(), "motor_losses")
      expect_identical(session$returned$result(), motor_losses)
    },
    args = list(x = block, data = list())
  )
})
