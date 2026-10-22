configured_assay_test_environment <- new.env(parent = globalenv())
package_root <- if (dir.exists("R")) "." else file.path("..", "..")
sys.source(
  file.path(package_root, "R", "configured_genes_assay.R"),
  envir = configured_assay_test_environment
)
configured_genes_assay <-
  configured_assay_test_environment$configured_genes_assay

test_that("canonical genes assay configuration is preferred", {
  config <- list(
    other_assay_options = list(
      genes_assay = "RNA",
      gene_assay = "legacy_RNA"
    )
  )

  expect_identical(configured_genes_assay(config), "RNA")
})

test_that("legacy singular genes assay configuration remains supported", {
  config <- list(other_assay_options = list(gene_assay = "RNA"))
  expect_identical(configured_genes_assay(config), "RNA")
})

test_that("unconfigured genes assays retain first-assay fallback behavior", {
  expect_null(configured_genes_assay(list()))
  expect_null(configured_genes_assay(list(other_assay_options = list())))
  expect_null(
    configured_genes_assay(
      list(other_assay_options = list(genes_assay = "none"))
    )
  )
  expect_null(
    configured_genes_assay(
      list(other_assay_options = list(genes_assay = NA_character_))
    )
  )
})
