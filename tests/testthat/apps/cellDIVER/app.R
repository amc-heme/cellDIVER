library(cellDIVER)

run_cellDIVER(
  object_path = system.file("extdata", "test_dataset.rds", package = "cellDIVER", mustWork = TRUE),
  config_path = system.file("extdata", "test_dataset_config.yaml", package = "cellDIVER", mustWork = TRUE),
  dev_mode = TRUE
)
