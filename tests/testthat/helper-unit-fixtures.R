#' Create a deterministic single-cell fixture
#'
#' Builds a small Seurat object in memory without external datasets or
#' normalization. An explicit Assay keeps the fixture compatible with the
#' package's supported Seurat assay API across Seurat versions.
#'
#' @return A Seurat object with three features, six cells, and metadata suitable
#'   for categorical, numeric, and logical selections.
unit_single_cell_object <- function() {
  cell_names <- paste0("cell_", c("one", "two", "three", "four", "five", "six"))
  expression_counts <- matrix(
    c(
      0, 1, 2, 2, 4, 5,
      6, 5, 4, 3, 2, 1,
      3, 1, 2, 1, 4, 3
    ),
    nrow = 3,
    byrow = TRUE,
    dimnames = list(c("GeneAlpha", "GeneBeta", "GeneGamma"), cell_names)
  )
  metadata <- data.frame(
    cell_type = factor(
      rep(c("Type 1", "Type 2", "Type 3"), each = 2)
    ),
    condition = rep(c("control", "treated"), 3),
    quality_score = c(-2, 0, 1, 1, 3, 5),
    selected = c(TRUE, FALSE, TRUE, FALSE, TRUE, FALSE),
    row.names = cell_names
  )
  metadata[["quality-score"]] <- metadata$quality_score

  Seurat::CreateSeuratObject(
    counts = Seurat::CreateAssayObject(counts = expression_counts),
    meta.data = metadata,
    project = "unit_tests"
  )
}

#' Create the equivalent SingleCellExperiment fixture
#'
#' Converts the deterministic Seurat fixture through Seurat's public API,
#' preserving cell names, assay values, and metadata across object formats.
#'
#' @return A SingleCellExperiment with three features and six cells.
unit_single_cell_experiment <- function() {
  Seurat::as.SingleCellExperiment(unit_single_cell_object())
}

#' Round-trip a unit configuration through YAML
#'
#' Writes the fixture beside the test files so it does not depend on the
#' machine's temporary directory, and removes it even if loading fails.
#'
#' @param configuration A configuration list to serialize.
#' @return The configuration processed by the package's real YAML loader.
unit_load_configuration <- function(configuration) {
  configuration_path <- tempfile(
    pattern = "unit-configuration-",
    tmpdir = testthat::test_path(),
    fileext = ".yaml"
  )
  on.exit(unlink(configuration_path), add = TRUE)
  yaml::write_yaml(configuration, configuration_path)
  cellDIVER:::load_config(configuration_path)
}

#' Create a replicated pseudobulk fixture
#'
#' A Seurat v5 object with six samples (three per condition) that each hold
#' both cell types, so `condition` gives an independent design and `cell_type`
#' a paired one. Poisson counts with a few genes shifted by condition and by
#' cell type; seeded, so results are reproducible.
#'
#' @param cells_per_profile Cells per sample x cell type profile.
#' @return A Seurat object with raw counts and log-normalised data.
unit_pseudobulk_object <- function(cells_per_profile = 20L) {
  set.seed(20261023)
  samples <- paste0("s", 1:6)
  condition <- rep(c("control", "treated"), each = 3L)
  profile <- expand.grid(
    cell_type = c("Type 1", "Type 2"), sample = samples, stringsAsFactors = FALSE
  )
  metadata <- profile[rep(seq_len(nrow(profile)), each = cells_per_profile), ]
  metadata$condition <- condition[match(metadata$sample, samples)]
  rownames(metadata) <- paste0("cell_", seq_len(nrow(metadata)))
  genes <- paste0("Gene", sprintf("%02d", 1:40))
  counts <- matrix(
    stats::rpois(length(genes) * nrow(metadata), lambda = 3),
    nrow = length(genes),
    dimnames = list(genes, rownames(metadata))
  )
  counts[1:3, metadata$condition == "treated"] <-
    counts[1:3, metadata$condition == "treated"] + 8L
  counts[4:6, metadata$cell_type == "Type 1"] <-
    counts[4:6, metadata$cell_type == "Type 1"] + 8L
  object <- suppressWarnings(SeuratObject::CreateSeuratObject(
    Matrix::Matrix(counts, sparse = TRUE), meta.data = metadata
  ))
  object <- SeuratObject::SetAssayData(
    object, layer = "data",
    new.data = log1p(Matrix::Matrix(counts, sparse = TRUE))
  )
  # The DGE tab draws a reduction after every run (an observer error there
  # would end the session), so the fixture carries a UMAP embedding.
  embeddings <- matrix(
    stats::rnorm(2L * nrow(metadata)), ncol = 2L,
    dimnames = list(rownames(metadata), c("UMAP_1", "UMAP_2"))
  )
  object[["umap"]] <- SeuratObject::CreateDimReducObject(
    embeddings = embeddings, key = "UMAP_", assay = "RNA"
  )
  object
}
