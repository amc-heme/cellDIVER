# Unit tests for to_log2()

test_that("to_log2 converts natural log to log2", {
  # log2(exp(1)) = log2(e) ≈ 1.4427
  result <- scExploreR:::to_log2(1)
  expect_equal(result, log2(exp(1)), tolerance = 1e-10)
})

test_that("to_log2 handles zero input", {
  # log2(exp(0)) = log2(1) = 0
  result <- scExploreR:::to_log2(0)
  expect_equal(result, 0)
})

test_that("to_log2 handles negative values", {
  # log2(exp(-1)) = -log2(e) ≈ -1.4427
  result <- scExploreR:::to_log2(-1)
  expect_equal(result, -log2(exp(1)), tolerance = 1e-10)
})

test_that("to_log2 preserves known relationships", {
  # ln(2) → log2(exp(ln(2))) = log2(2) = 1
  result <- scExploreR:::to_log2(log(2))
  expect_equal(result, 1, tolerance = 1e-10)
})

test_that("to_log2 handles vector input", {
  values <- c(0, log(2), log(4), log(8))
  result <- scExploreR:::to_log2(values)
  expect_equal(result, c(0, 1, 2, 3), tolerance = 1e-10)
})
