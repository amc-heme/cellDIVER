# Unit tests for load_config()

test_that("load_config loads test dataset config file", {
  config_path <- system.file(
    "extdata",
    "test_dataset_config.yaml",
    package = "scExploreR",
    mustWork = TRUE
  )

  config <- scExploreR:::load_config(config_path)

  expect_type(config, "list")
  expect_equal(config$label, "AML Reference Dataset")
  expect_true("assays" %in% names(config))
  expect_true("metadata" %in% names(config))
  expect_true("reductions" %in% names(config))
})

test_that(
  "load_config creates empty tibble for null adt_thresholds",
  {
    config_path <- system.file(
      "extdata",
      "test_dataset_config.yaml",
      package = "scExploreR",
      mustWork = TRUE
    )

    config <- scExploreR:::load_config(config_path)

    # adt_thresholds is "~" (NULL) in test config,
    # should become empty tibble
    expect_s3_class(config$adt_thresholds, "tbl_df")
    expect_equal(nrow(config$adt_thresholds), 0)
    expect_named(config$adt_thresholds, c("adt", "value"))
  }
)

test_that("load_config preserves assay structure", {
  config_path <- system.file(
    "extdata",
    "test_dataset_config.yaml",
    package = "scExploreR",
    mustWork = TRUE
  )

  config <- scExploreR:::load_config(config_path)

  # Test dataset has RNA and AB assays
  expect_true("RNA" %in% names(config$assays))
  expect_true("AB" %in% names(config$assays))
  expect_equal(config$assays$RNA$key, "rna_")
  expect_equal(config$assays$AB$key, "ab_")
  expect_equal(
    config$assays$AB$suffix_human,
    "Surface Protein"
  )
})

test_that("load_config preserves metadata structure", {
  config_path <- system.file(
    "extdata",
    "test_dataset_config.yaml",
    package = "scExploreR",
    mustWork = TRUE
  )

  config <- scExploreR:::load_config(config_path)

  expect_true(
    "condensed_cell_type" %in% names(config$metadata)
  )
  expect_equal(
    config$metadata$condensed_cell_type$label,
    "Cell Type"
  )
  expect_true("Batch" %in% names(config$metadata))
  expect_equal(config$metadata$Batch$label, "Batch")
})

test_that("load_config preserves reduction structure", {
  config_path <- system.file(
    "extdata",
    "test_dataset_config.yaml",
    package = "scExploreR",
    mustWork = TRUE
  )

  config <- scExploreR:::load_config(config_path)

  expect_true("pca" %in% names(config$reductions))
  expect_true("umap" %in% names(config$reductions))
  expect_equal(config$reductions$umap$label, "UMAP")
})
