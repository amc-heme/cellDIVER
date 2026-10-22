# Browser-test fixture: the AML test dataset as an .h5ad file served by SCUBA's Julia engine.
options(SCUBA.engine = "julia", cellDIVER.h5ad_engine = "julia")
library(cellDIVER)

run_cellDIVER(
  object_path = system.file("extdata", "AML_h5ad.h5ad", package = "SCUBA", mustWork = TRUE),
  config_path = getOption(
    "cellDIVER.test_config",
    system.file("extdata", "test_dataset_h5ad_config.yaml", package = "cellDIVER", mustWork = TRUE)
  ),
  dev_mode = TRUE
)
