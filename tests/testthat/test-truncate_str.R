# Unit tests for truncate_str()

test_that("truncate_str shortens long strings", {
  result <- scExploreR:::truncate_str("Hello World", 5)
  expect_equal(result, "Hello...")
})

test_that("truncate_str returns short strings unchanged", {
  result <- scExploreR:::truncate_str("Hi", 10)
  expect_equal(result, "Hi")
})

test_that("truncate_str handles exact length strings", {
  result <- scExploreR:::truncate_str("Hello", 5)
  expect_equal(result, "Hello")
})

test_that("truncate_str handles single character max_length", {

  result <- scExploreR:::truncate_str("Hello", 1)
  expect_equal(result, "H...")
})

test_that("truncate_str handles empty string", {
  result <- scExploreR:::truncate_str("", 5)
  expect_equal(result, "")
})
