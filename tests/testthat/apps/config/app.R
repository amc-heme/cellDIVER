library(cellDIVER)

run_config(
  object_path = system.file(
    "extdata", "test_dataset.rds", package = "cellDIVER", mustWork = TRUE
  ),
  config_path = getOption(
    "cellDIVER.test_config",
    system.file(
      "extdata", "test_dataset_config.yaml",
      package = "cellDIVER", mustWork = TRUE
    )
  )
)
