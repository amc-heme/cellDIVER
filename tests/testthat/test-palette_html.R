# Unit tests for palette_html()

test_that(
  "palette_html returns a shiny.tag for categorical palette",
  {
    result <- scExploreR:::palette_html(
      palette = c("#FF0000", "#00FF00", "#0000FF"),
      type = "categorical",
      n = 3
    )
    expect_s3_class(result, "shiny.tag")
    expect_equal(result$name, "div")
  }
)

test_that("palette_html returns HTML string when requested", {
  result <- scExploreR:::palette_html(
    palette = c("#FF0000", "#00FF00", "#0000FF"),
    type = "categorical",
    n = 3,
    output_html = TRUE
  )
  expect_type(result, "character")
  expect_true(grepl("<div", result))
  expect_true(grepl("background-color", result))
})

test_that(
  "palette_html includes palette name when provided",
  {
    result <- scExploreR:::palette_html(
      palette = c("#FF0000", "#0000FF"),
      type = "categorical",
      palette_name = "My Palette",
      n = 2,
      output_html = TRUE
    )
    expect_true(grepl("My Palette", result))
  }
)

test_that(
  "palette_html returns a shiny.tag for continuous palette",
  {
    result <- scExploreR:::palette_html(
      palette = c("#FF0000", "#FFFF00", "#00FF00"),
      type = "continuous"
    )
    expect_s3_class(result, "shiny.tag")
    expect_equal(result$name, "span")
  }
)

test_that(
  "palette_html continuous type returns HTML with gradient",
  {
    result <- scExploreR:::palette_html(
      palette = c("#FF0000", "#00FF00"),
      type = "continuous",
      output_html = TRUE
    )
    expect_type(result, "character")
    expect_true(grepl("linear-gradient", result))
  }
)
