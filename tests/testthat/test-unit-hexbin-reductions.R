test_that("hexbin plots reject unknown reductions with a cellDIVER message", {
  object <- browser_object()
  available <- paste(cellDIVER:::reduction_names(object), collapse = ", ")
  message <- paste0(
    "Reduction 'does_not_exist' not found in object. Available reductions: ",
    available
  )
  expect_identical(available, "pca, umap")

  expect_error(
    cellDIVER:::shiny_hexbin_density(object, reduction = "does_not_exist"),
    message, fixed = TRUE, class = "validation"
  )
  expect_error(
    cellDIVER:::shiny_hexbin_feature(
      object, features_entered = "rna_CD34", reduction = "does_not_exist"
    ),
    message, fixed = TRUE, class = "validation"
  )
})

test_that("hexbin plots default to the object's own reduction name", {
  object <- browser_object()
  default <- SCUBA::default_reduction(object)
  expect_identical(default, "umap")

  density <- cellDIVER:::shiny_hexbin_density(object, nbins = 30)
  expect_identical(density$labels$x, paste0(default, "_1"))

  feature <- cellDIVER:::shiny_hexbin_feature(
    object, features_entered = "rna_CD34", nbins = 30
  )
  expect_identical(feature$labels$x, paste0(default, "_1"))
})
