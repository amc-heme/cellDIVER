test_that("the shipped configuration loads with its nested settings intact", {
  configuration_path <- system.file(
    "extdata", "test_dataset_config.yaml",
    package = "cellDIVER", mustWork = TRUE
  )
  configuration <- cellDIVER:::load_config(configuration_path)

  expect_identical(configuration$label, "AML Reference Dataset")
  expect_identical(configuration$assays$RNA$key, "rna_")
  expect_identical(configuration$assays$AB$suffix_human, "Surface Protein")
  expect_identical(
    configuration$metadata$condensed_cell_type$label,
    "Cell Type"
  )
  expect_identical(configuration$reductions$umap$label, "UMAP")
  expect_identical(configuration$preview$plot_settings$label, TRUE)
  expect_s3_class(configuration$adt_thresholds, "tbl_df")
  expect_identical(
    configuration$adt_thresholds,
    tibble::tibble(adt = character(), value = numeric())
  )
})

test_that("missing and null thresholds get the same typed empty table", {
  for (configuration in list(
    list(label = "No threshold field"),
    list(label = "Null thresholds", adt_thresholds = NULL)
  )) {
    loaded_configuration <- unit_load_configuration(configuration)
    expect_identical(loaded_configuration$label, configuration$label)
    expect_identical(
      loaded_configuration$adt_thresholds,
      tibble::tibble(adt = character(), value = numeric())
    )
  }
})

test_that("configured thresholds retain feature order and numeric values", {
  configuration <- list(
    label = "Threshold fixture",
    adt_thresholds = list(
      adt = c("GeneBeta", "GeneAlpha"),
      value = c(1.25, 2.5)
    ),
    metadata = list(
      cell_type = list(
        label = "Cell type",
        groups = list(
          list(group_name = "Immune", group_members = c("Type 1", "Type 2"))
        )
      )
    )
  )
  loaded_configuration <- unit_load_configuration(configuration)

  expect_identical(
    loaded_configuration$adt_thresholds,
    tibble::tibble(adt = c("GeneBeta", "GeneAlpha"), value = c(1.25, 2.5))
  )
  expect_identical(loaded_configuration$metadata, configuration$metadata)
})

test_that("browser information must be complete for every dataset", {
  dataset_information <- list(
    label = "Dataset",
    description = "Description",
    plot = "preview.png"
  )
  datasets <- list(first = dataset_information, second = dataset_information)
  expect_true(cellDIVER:::browser_config_has_info(datasets))
  expect_false(cellDIVER:::dataset_config_has_info(datasets))

  for (field_name in c("label", "description", "plot")) {
    incomplete_datasets <- datasets
    incomplete_datasets$second[[field_name]] <- NULL
    expect_false(
      cellDIVER:::browser_config_has_info(incomplete_datasets),
      info = paste("Missing browser field:", field_name)
    )
  }
})

test_that("dataset information is checked inside every dataset config", {
  dataset_configuration <- list(
    label = "Dataset",
    description = "Description",
    preview = list(type = "dimplot")
  )
  datasets <- list(
    first = list(config = dataset_configuration),
    second = list(config = dataset_configuration)
  )
  expect_true(cellDIVER:::dataset_config_has_info(datasets))
  expect_false(cellDIVER:::browser_config_has_info(datasets))

  for (field_name in c("label", "description", "preview")) {
    incomplete_datasets <- datasets
    incomplete_datasets$second$config[[field_name]] <- NULL
    expect_false(
      cellDIVER:::dataset_config_has_info(incomplete_datasets),
      info = paste("Missing dataset field:", field_name)
    )
  }
  datasets$second$config <- NULL
  expect_false(cellDIVER:::dataset_config_has_info(datasets))
})

test_that("mixed information locations are not mistaken for complete configs", {
  datasets <- list(
    first = list(
      label = "Browser dataset",
      description = "Browser description",
      plot = "preview.png"
    ),
    second = list(
      config = list(
        label = "Dataset configuration",
        description = "Dataset description",
        preview = list(type = "dimplot")
      )
    )
  )

  expect_false(cellDIVER:::browser_config_has_info(datasets))
  expect_false(cellDIVER:::dataset_config_has_info(datasets))
})
