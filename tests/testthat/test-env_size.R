# Unit tests for env_size()

test_that("env_size errors on invalid units", {
  test_env <- new.env(parent = emptyenv())
  expect_error(
    scExploreR:::env_size(test_env, units = "TB"),
    "Unrecognized specification for units"
  )
})

test_that("env_size reports empty environment", {
  test_env <- new.env(parent = emptyenv())
  expect_output(
    scExploreR:::env_size(test_env, units = "B"),
    "No objects found"
  )
})

test_that("env_size computes size for non-empty environment", {
  test_env <- new.env(parent = emptyenv())
  test_env$x <- 1:100
  test_env$y <- "hello"
  expect_output(
    scExploreR:::env_size(test_env, units = "B"),
    "Total RAM usage"
  )
})
