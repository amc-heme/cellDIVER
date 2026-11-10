suppressPackageStartupMessages(library(shiny))

dge_run_test_environment <- new.env(parent = globalenv())
package_root <- if (dir.exists("R")) "." else file.path("..", "..")
for (file in c("icon_notification_ui.R", "module-dge_tab.R", "load_single_cell_object.R")) {
  sys.source(file.path(package_root, "R", file), envir = dge_run_test_environment)
}
dge_counts_assays <- dge_run_test_environment$dge_counts_assays
dge_results_design_type <- dge_run_test_environment$dge_results_design_type
edger_details_table <- dge_run_test_environment$edger_details_table
dge_edger_error_ui <- dge_run_test_environment$dge_edger_error_ui
h5_object_loader <- dge_run_test_environment$h5_object_loader

test_that("only assays with a raw counts layer are offered to edgeR", {
  object <- unit_single_cell_object()
  expect_identical(dge_counts_assays(object, "RNA"), "RNA")
  data_only <- object
  data_only[["norm"]] <- SeuratObject::CreateAssayObject(
    data = log1p(SeuratObject::LayerData(object[["RNA"]], layer = "counts"))
  )
  expect_identical(dge_counts_assays(data_only, c("RNA", "norm")), "RNA")
  expect_identical(dge_counts_assays(object, c("RNA", "missing")), "RNA")
  expect_identical(dge_counts_assays(object, character()), character())
})

test_that("the design type of a result is lifted from its details", {
  expect_identical(
    dge_results_design_type(list(details = list(design_type = "paired"))),
    "paired"
  )
  multi <- list(multicontrast_details = list(contrast_table = data.frame(
    method = c("edgeR", "wilcoxon", "edgeR"),
    design_type = c("paired", "paired", "paired")
  )))
  expect_identical(dge_results_design_type(multi), "paired")
  mixed <- list(multicontrast_details = list(contrast_table = data.frame(
    method = c("edgeR", "edgeR"), design_type = c("paired", "independent")
  )))
  expect_identical(dge_results_design_type(mixed), "mixed")
  only_fallback <- list(multicontrast_details = list(contrast_table = data.frame(
    method = "wilcoxon", design_type = "paired"
  )))
  expect_true(is.na(dge_results_design_type(only_fallback)))
  expect_true(is.na(dge_results_design_type(list())))
})

test_that("pseudobulk sample tables flatten every result shape", {
  details <- list(
    method = "edgeR", design_type = "independent", group_1 = "A", group_2 = "B",
    fallback_reason = NA_character_,
    retained_profiles = data.frame(
      profile_id = c("s1__A", "s2__B"), sample = c("s1", "s2"), group = c("A", "B"),
      n_cells = c(20L, 30L), stringsAsFactors = FALSE
    ),
    excluded_profiles = data.frame(
      profile_id = "s3__A", sample = "s3", group = "A", n_cells = 4L,
      exclusion_reason = "fewer than 10 cells", stringsAsFactors = FALSE
    )
  )
  fallback <- list(
    method = "wilcoxon", design_type = NA_character_, group_1 = "A", group_2 = "C",
    fallback_reason = "'C' has 1 sample",
    retained_profiles = data.frame(), excluded_profiles = data.frame()
  )
  single <- data.frame(feature = "g")
  attr(single, "edger_details") <- details
  table <- edger_details_table(single)
  expect_identical(nrow(table), 3L)
  expect_identical(table$contrast, rep("A vs B", 3))
  expect_identical(table$retained, c(TRUE, TRUE, FALSE))
  expect_identical(table$exclusion_reason[3], "fewer than 10 cells")

  multi <- data.frame(feature = "g")
  attr(multi, "edger_multicontrast_details") <- list(
    contrasts = list("A vs B" = details, "A vs C" = fallback)
  )
  table <- edger_details_table(multi)
  expect_identical(table$contrast, c(rep("A vs B", 3), "A vs C"))
  expect_identical(table$method, c(rep("edgeR", 3), "wilcoxon"))
  expect_identical(table$fallback_reason[4], "'C' has 1 sample")

  per_assay <- data.frame(feature = "g")
  attr(per_assay, "edger_assay_details") <- list(
    RNA = list(details = details, multicontrast_details = NULL),
    AB = list(details = NULL, multicontrast_details = list(contrasts = list("A vs C" = fallback)))
  )
  table <- edger_details_table(per_assay)
  expect_identical(table$assay, c(rep("RNA", 3), "AB"))
  expect_identical(
    names(table),
    c("assay", "contrast", "method", "design_type", "sample", "group",
      "n_cells", "retained", "exclusion_reason", "fallback_reason")
  )
  expect_identical(nrow(edger_details_table(data.frame(feature = "g"))), 0L)
})

test_that("the error notification lists per-assay failures", {
  condition <- rlang::catch_cnd(rlang::abort(
    "Differential expression analysis failed for all selected assays.",
    class = "dge_all_assays_failed",
    assay_errors = c(RNA = "'A' has 1 sample", AB = "no counts")
  ))
  rendered <- as.character(dge_edger_error_ui(condition))
  expect_match(rendered, "edgeR could not run", fixed = TRUE)
  expect_match(rendered, "RNA: 'A' has 1 sample", fixed = TRUE)
  expect_match(rendered, "AB: no counts", fixed = TRUE)
  expect_match(rendered, "<li>", fixed = TRUE)
})

test_that("the h5ad loader preference is honoured without starting Julia", {
  withr::local_options(list(cellDIVER.h5_engine = "reticulate"))
  expect_identical(h5_object_loader(), "reticulate")
  expect_identical(h5_object_loader("reticulate"), "reticulate")
  expect_error(h5_object_loader("python"), "cellDIVER.h5_engine")
})

test_that("a selected alternative experiment is swapped in for scDE", {
  skip_if_not_installed("SingleCellExperiment")
  object <- unit_pseudobulk_object()
  sce <- Seurat::as.SingleCellExperiment(object)
  protein <- matrix(
    seq_len(4 * ncol(sce)), nrow = 4,
    dimnames = list(paste0("p", 1:4), colnames(sce))
  )
  SingleCellExperiment::altExp(sce, "ADT") <-
    SingleCellExperiment::SingleCellExperiment(list(counts = protein))

  swapped <- cellDIVER:::dge_assay_object(sce, "ADT")
  expect_identical(SingleCellExperiment::mainExpName(swapped), "ADT")
  expect_identical(rownames(swapped), paste0("p", 1:4))
  # the cell metadata (groups) travels with the swapped experiment
  expect_identical(
    SummarizedExperiment::colData(swapped)$condition,
    SummarizedExperiment::colData(sce)$condition
  )
  # the main experiment, Seurat objects and unknown assays are left alone
  expect_identical(cellDIVER:::dge_assay_object(sce, "RNA"), sce)
  expect_identical(cellDIVER:::dge_assay_object(sce, "missing"), sce)
  expect_identical(cellDIVER:::dge_assay_object(object, "ADT"), object)
})

test_that("the cell-level test of AnnData objects is limited to the X matrix", {
  anndata_like <- structure(list(), class = "AnnDataR6")
  expect_identical(cellDIVER:::dge_wilcoxon_assays(anndata_like, c("X", "protein")), "X")
  expect_identical(cellDIVER:::dge_wilcoxon_assays(anndata_like, "protein"), character(0))
  object <- unit_pseudobulk_object()
  expect_identical(cellDIVER:::dge_wilcoxon_assays(object, c("RNA", "ADT")), c("RNA", "ADT"))
})
