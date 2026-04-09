# Unit tests for vector_to_text()

test_that("vector_to_text returns 'NULL' for NULL input", {
  result <- scExploreR:::vector_to_text(NULL)
  expect_equal(result, "NULL")
})

test_that("vector_to_text handles single element", {
  result <- scExploreR:::vector_to_text("apple")
  expect_equal(result, "apple")
})

test_that("vector_to_text joins two elements with 'and'", {
  result <- scExploreR:::vector_to_text(c("apple", "banana"))
  expect_equal(result, "apple and banana")
})

test_that(
  "vector_to_text joins three elements with commas and 'and'",
  {
    result <- scExploreR:::vector_to_text(
      c("apple", "banana", "cherry")
    )
    expect_equal(result, "apple, banana, and cherry")
  }
)

test_that("vector_to_text handles four elements", {
  result <- scExploreR:::vector_to_text(
    c("apple", "banana", "cherry", "date")
  )
  expect_equal(
    result,
    "apple, banana, cherry, and date"
  )
})

test_that("vector_to_text handles numeric vector", {
  result <- scExploreR:::vector_to_text(c(1, 2, 3))
  expect_equal(result, "1, 2, and 3")
})
