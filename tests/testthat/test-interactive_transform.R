# Unit tests for interactive_transform()

test_that("interactive_transform computes correct midpoint", {
  # When x_coord is at the midpoint of the plot coordinate
  # range, it should return the midpoint of the distribution
  result <- scExploreR:::interactive_transform(
    x_coord = 0.5,
    distribution_range = 10,
    distribution_minimum = 0,
    plot_min_coord = 0,
    plot_max_coord = 1
  )
  expect_equal(result, 5)
})

test_that("interactive_transform returns min at left edge", {
  result <- scExploreR:::interactive_transform(
    x_coord = 0,
    distribution_range = 10,
    distribution_minimum = 0,
    plot_min_coord = 0,
    plot_max_coord = 1
  )
  expect_equal(result, 0)
})

test_that("interactive_transform returns max at right edge", {
  result <- scExploreR:::interactive_transform(
    x_coord = 1,
    distribution_range = 10,
    distribution_minimum = 0,
    plot_min_coord = 0,
    plot_max_coord = 1
  )
  expect_equal(result, 10)
})

test_that(
  "interactive_transform handles non-zero distribution minimum",
  {
    # Distribution from 5 to 15 (range = 10, min = 5)
    result <- scExploreR:::interactive_transform(
      x_coord = 0.5,
      distribution_range = 10,
      distribution_minimum = 5,
      plot_min_coord = 0,
      plot_max_coord = 1
    )
    expect_equal(result, 10)
  }
)

test_that(
  "interactive_transform handles non-standard plot coords",
  {
    # Plot coords range from 0.1 to 0.9
    result <- scExploreR:::interactive_transform(
      x_coord = 0.5,
      distribution_range = 100,
      distribution_minimum = 0,
      plot_min_coord = 0.1,
      plot_max_coord = 0.9
    )
    expect_equal(result, 50)
  }
)

test_that(
  "interactive_transform handles negative distribution values",
  {
    # Distribution from -5 to 5 (range = 10, min = -5)
    result <- scExploreR:::interactive_transform(
      x_coord = 0.5,
      distribution_range = 10,
      distribution_minimum = -5,
      plot_min_coord = 0,
      plot_max_coord = 1
    )
    expect_equal(result, 0)
  }
)
