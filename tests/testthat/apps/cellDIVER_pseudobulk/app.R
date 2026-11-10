library(cellDIVER)

source(file.path(getwd(), "pseudobulk_fixture.R"), local = TRUE)

object_path <- tempfile("pseudobulk-object-", fileext = ".rds")
saveRDS(pseudobulk_browser_object(), object_path)
config_path <- tempfile("pseudobulk-config-", fileext = ".yaml")
yaml::write_yaml(pseudobulk_browser_config(), config_path)

run_cellDIVER(
  object_path = object_path,
  config_path = config_path,
  dev_mode = TRUE
)
