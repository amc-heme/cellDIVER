test_that("diverging scales center on zero and clamp at the cap", {
  palette <- c("#2166AC", "#F7F7F7", "#B2182B")

  uncapped <- cellDIVER:::diverging_scale("colour", palette)
  expect_s3_class(uncapped, "ScaleContinuous")
  expect_identical(uncapped$aesthetics[[1]], "colour")
  expect_null(uncapped$limits)

  capped <- cellDIVER:::diverging_scale("colour", palette, limit = 2)
  expect_equal(capped$limits, c(-2, 2))
  # Out-of-range values are squished to the ends rather than dropped
  expect_equal(capped$oob(c(-5, 0, 5), c(-2, 2)), c(-2, 0, 2))
  expect_identical(capped$palette(0.5), "#F7F7F7")

  fill <- cellDIVER:::diverging_scale("fill", palette, limit = 0.5)
  expect_identical(fill$aesthetics[[1]], "fill")
  expect_equal(fill$limits, c(-0.5, 0.5))

  # Non-positive or missing caps fall back to the data range
  for (limit in list(NULL, 0, -1, NA_real_, Inf)) {
    expect_null(cellDIVER:::diverging_scale("fill", palette, limit)$limits)
  }
})

test_that("dot plots pass the diverging cap through to their colour scale", {
  object <- unit_single_cell_object()
  palette <- c("#2166AC", "#F7F7F7", "#B2182B")

  capped <- suppressWarnings(cellDIVER:::shiny_dot(
    object,
    features_entered = c("rna_GeneAlpha", "rna_GeneBeta"),
    group_by = "cell_type",
    diverging_palette = palette,
    diverging_limit = 1
  ))
  expect_equal(capped$scales$get_scales("colour")$limits, c(-1, 1))

  uncapped <- suppressWarnings(cellDIVER:::shiny_dot(
    object,
    features_entered = c("rna_GeneAlpha", "rna_GeneBeta"),
    group_by = "cell_type",
    diverging_palette = palette
  ))
  expect_null(uncapped$scales$get_scales("colour")$limits)
})
