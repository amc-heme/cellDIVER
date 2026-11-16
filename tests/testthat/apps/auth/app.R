library(cellDIVER)

# Browser config with authentication enabled: the bundled test dataset plus the
# credentials file next to this app. The YAML needs absolute paths, so it is
# written to a temporary file at startup.
app_dir <- normalizePath(".", mustWork = TRUE)
browser_config <- tempfile("browser_config_", fileext = ".yaml")
writeLines(
  c(
    "datasets:",
    "  object:",
    sprintf(
      "    object: \"%s\"",
      system.file("extdata", "test_dataset.rds", package = "cellDIVER", mustWork = TRUE)
    ),
    sprintf(
      "    config: \"%s\"",
      system.file("extdata", "test_dataset_config.yaml", package = "cellDIVER", mustWork = TRUE)
    ),
    sprintf("credentials: \"%s\"", file.path(app_dir, "credentials.tsv")),
    "users:",
    "  - alice",
    "RSTUDIO_PANDOC: ~"
  ),
  browser_config
)

run_cellDIVER(browser_config = browser_config, dev_mode = TRUE)
