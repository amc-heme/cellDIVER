# The shipped browser dataset has two samples (Batch), so pseudobulk tests can
# never be replicated on it. These helpers add a deterministic `sample` column
# (three samples per batch, cells dealt round-robin within each batch) and a
# matching configuration that exposes it and makes it the default replicate
# column, and relabel one cell type so that it exists in a single sample. Both
# the browser fixture app and the tests source this file so the expected
# results are computed from an identical object.

pseudobulk_browser_object <- function() {
  object <- readRDS(system.file(
    "extdata", "test_dataset.rds", package = "cellDIVER", mustWork = TRUE
  ))
  batch <- as.character(object$Batch)
  sample <- character(length(batch))
  for (level in unique(batch)) {
    index <- which(batch == level)
    sample[index] <- paste0(level, "_", rep_len(1:3, length(index)))
  }
  object$sample <- sample
  # A cell type confined to one sample, so marker identification has a
  # contrast that cannot be replicated and falls back to the cell-level test
  cell_type <- as.character(object$condensed_cell_type)
  rare <- which(cell_type == "Primitive" & sample == "BM_200AB_2")
  cell_type[rare] <- "Rare cells"
  object$condensed_cell_type <- cell_type
  object
}

pseudobulk_browser_config <- function() {
  configuration <- yaml::read_yaml(system.file(
    "extdata", "test_dataset_config.yaml", package = "cellDIVER", mustWork = TRUE
  ))
  configuration$metadata$sample <- list(
    meta_colname = "sample", label = "Sample", groups = NULL
  )
  configuration$other_metadata_options$patient_colname <- "sample"
  configuration
}
