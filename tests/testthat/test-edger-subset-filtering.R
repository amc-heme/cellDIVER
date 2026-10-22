subset_filter_test_environment <- new.env(parent = globalenv())
package_root <- if (dir.exists("R")) "." else file.path("..", "..")
sys.source(
  file.path(package_root, "R", "module-subset_selections.R"),
  envir = subset_filter_test_environment
)
subset_filter_type_choices <-
  subset_filter_test_environment$subset_filter_type_choices

test_that("edgeR-compatible subset menus exclude expression filters", {
  seurat_like <- structure(list(), class = "Seurat")
  expect_identical(
    subset_filter_type_choices(seurat_like, FALSE),
    c("Select Type" = "", "Categorical Metadata" = "categorical")
  )
})

test_that("Wilcoxon subset menus retain object-supported filters", {
  seurat_like <- structure(list(), class = "Seurat")
  expect_identical(
    subset_filter_type_choices(seurat_like, TRUE),
    c(
      "Select Type" = "",
      "Categorical Metadata" = "categorical",
      "Feature Expression" = "numeric",
      "Advanced Subsetting" = "advanced"
    )
  )

  sce_like <- structure(list(), class = "SingleCellExperiment")
  expect_identical(
    subset_filter_type_choices(sce_like, TRUE),
    c("Select Type" = "", "Categorical Metadata" = "categorical")
  )
})
