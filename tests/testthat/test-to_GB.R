# Unit tests for to_GB()

test_that("to_GB converts bytes to gigabytes", {
  result <- scExploreR:::to_GB(1000000000)
  expect_equal(as.character(result), "1 GB")
})

test_that("to_GB handles zero bytes", {
  result <- scExploreR:::to_GB(0)
  expect_equal(as.character(result), "0 GB")
})

test_that("to_GB rounds to three decimal places", {
  # 1,500,000,000 bytes = 1.5 GB
  result <- scExploreR:::to_GB(1500000000)
  expect_equal(as.character(result), "1.5 GB")
})

test_that("to_GB handles small values", {
  # 500,000 bytes = 0.0005 GB, rounds to 0.001
  result <- scExploreR:::to_GB(500000)
  expect_equal(as.character(result), "0.001 GB")
})

test_that("to_GB handles large values", {
  # 10 billion bytes = 10 GB

  result <- scExploreR:::to_GB(10000000000)
  expect_equal(as.character(result), "10 GB")
})
