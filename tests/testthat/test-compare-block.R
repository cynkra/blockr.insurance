# Tests for compare block

# --- compare_frames() unit tests ---

test_that("compare_frames computes diff metric", {
  x <- data.frame(region = c("A", "B"), val = c(100, 200))
  y <- data.frame(region = c("A", "B"), val = c(90, 210))

  res <- compare_frames(x, y, "region", "val", metric = "diff")

  expect_equal(res$region, c("A", "B"))
  expect_equal(res$val, c(10, -10))
  expect_equal(colnames(res), c("region", "val"))
})

test_that("compare_frames computes abs_diff metric", {
  x <- data.frame(k = "A", v = 100)
  y <- data.frame(k = "A", v = 130)

  res <- compare_frames(x, y, "k", "v", metric = "abs_diff")
  expect_equal(res$v, 30)
})

test_that("compare_frames computes rel_diff metric", {
  x <- data.frame(k = "A", v = 110)
  y <- data.frame(k = "A", v = 100)

  res <- compare_frames(x, y, "k", "v", metric = "rel_diff")
  expect_equal(res$v, 10)
})

test_that("compare_frames computes ratio metric", {
  x <- data.frame(k = "A", v = 200)
  y <- data.frame(k = "A", v = 100)

  res <- compare_frames(x, y, "k", "v", metric = "ratio")
  expect_equal(res$v, 2)
})

test_that("compare_frames computes pct_change metric", {
  x <- data.frame(k = "A", v = 120)
  y <- data.frame(k = "A", v = 80)

  res <- compare_frames(x, y, "k", "v", metric = "pct_change")
  # (120 - 80) / avg(120,80) * 100 = 40 / 100 * 100 = 40
  expect_equal(res$v, 40)
})

test_that("compare_frames handles multiple measure columns", {
  x <- data.frame(k = "A", p = 100, q = 50)
  y <- data.frame(k = "A", p = 90, q = 60)

  res <- compare_frames(x, y, "k", c("p", "q"), metric = "diff")

  expect_equal(res$p, 10)
  expect_equal(res$q, -10)
  expect_equal(colnames(res), c("k", "p", "q"))
})

test_that("compare_frames full join produces NAs for unmatched rows", {
  x <- data.frame(k = c("A", "B"), v = c(100, 200))
  y <- data.frame(k = c("A", "C"), v = c(90, 300))

  res <- compare_frames(x, y, "k", "v", join_type = "full", metric = "diff")

  expect_equal(nrow(res), 3L)
  # Unmatched rows should have NA diffs
  expect_true(any(is.na(res$v)))
  # Matched row A: 100 - 90 = 10
  expect_equal(res$v[res$k == "A"], 10)
})

test_that("compare_frames division by zero produces NA", {
  x <- data.frame(k = "A", v = 10)
  y <- data.frame(k = "A", v = 0)

  res_rel <- compare_frames(x, y, "k", "v", metric = "rel_diff")
  expect_true(is.na(res_rel$v))

  res_ratio <- compare_frames(x, y, "k", "v", metric = "ratio")
  expect_true(is.na(res_ratio$v))
})

test_that("compare_frames pct_change with symmetric zero produces NA", {
  x <- data.frame(k = "A", v = 0)
  y <- data.frame(k = "A", v = 0)

  res <- compare_frames(x, y, "k", "v", metric = "pct_change")
  expect_true(is.na(res$v))
})

# --- classify_columns() unit tests ---

test_that("classify_columns identifies character columns as keys", {
  x <- data.frame(name = "A", val = 1.0)
  y <- data.frame(name = "B", val = 2.0)

  cls <- blockr.insurance:::classify_columns(x, y)
  expect_true("name" %in% cls$key_cols)
  expect_false("name" %in% cls$measure_cols)
})

test_that("classify_columns identifies factor columns as keys", {
  x <- data.frame(grp = factor("X"), val = 1.0)
  y <- data.frame(grp = factor("Y"), val = 2.0)

  cls <- blockr.insurance:::classify_columns(x, y)
  expect_true("grp" %in% cls$key_cols)
})

test_that("classify_columns identifies numeric columns as measures", {
  x <- data.frame(name = "A", val = 1.5, count = 10L)
  y <- data.frame(name = "B", val = 2.5, count = 20L)

  cls <- blockr.insurance:::classify_columns(x, y)
  expect_true("val" %in% cls$measure_cols)
})

test_that("classify_columns treats low-cardinality integer as key", {
  x <- data.frame(id = 1:5, val = rnorm(5))
  y <- data.frame(id = 1:5, val = rnorm(5))

  cls <- blockr.insurance:::classify_columns(x, y)
  expect_true("id" %in% cls$key_cols)
  expect_true("val" %in% cls$measure_cols)
})

# --- Block construction tests ---

test_that("new_compare_block creates valid block with correct classes", {
  block <- new_compare_block()

  expect_s3_class(block, "compare_block")
  expect_s3_class(block, "transform_block")
  expect_s3_class(block, "block")
})

test_that("compare_block has arity 2 with x and y inputs", {
  block <- new_compare_block()

  expect_equal(blockr.core::block_arity(block), 2L)
  expect_equal(blockr.core::block_inputs(block), c("x", "y"))
})
