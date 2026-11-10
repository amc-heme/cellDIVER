# File-backed .h5ad objects served by SCUBA's Julia engine. These tests are
# registered only when CELLDIVER_TEST_JULIA=true and the engine starts, so a
# machine without Julia records no skip (the required CI job treats skips as
# failures); the optional Julia CI job sets the variable.

celldiver_julia_tests_enabled <- function() {
  if (!identical(Sys.getenv("CELLDIVER_TEST_JULIA"), "true")) return(FALSE)
  if (!exists("open_h5ad", asNamespace("SCUBA"))) return(FALSE)
  isTRUE(tryCatch(SCUBA::scuba_julia_available(quiet = TRUE), error = function(e) FALSE))
}

# testthat reports a file that registers no test as an "empty test" skip, so
# one test always runs: it documents the switch the rest of the file hangs on.
test_that("the file-backed object tests are opt-in", {
  enabled <- celldiver_julia_tests_enabled()
  expect_type(enabled, "logical")
  expect_length(enabled, 1L)
  if (!identical(Sys.getenv("CELLDIVER_TEST_JULIA"), "true")) {
    expect_false(enabled)
  }
})

if (celldiver_julia_tests_enabled()) {
  withr::local_options(list(SCUBA.engine = "julia", cellDIVER.h5_engine = "julia"))
  h5ad <- system.file("extdata", "AML_h5ad.h5ad", package = "SCUBA", mustWork = TRUE)

  test_that("h5ad files open as file-backed SCUBA objects", {
    object <- cellDIVER:::load_single_cell_object(h5ad)
    expect_s3_class(object, "scuba_source")
    expect_identical(cellDIVER:::h5_object_loader(), "scuba_source")
    expect_identical(cellDIVER:::assay_names(object), c("X", "protein"))
    expect_identical(cellDIVER:::reduction_names(object), c("X_pca", "X_umap"))
    expect_identical(cellDIVER:::make_key(object, "X"), "X_")
    expect_identical(cellDIVER:::relabel_axis(object, "X_umap_1"), "Embeddings Value")
    expect_true(cellDIVER:::dge_object_supports_edger(object))
    expect_true(cellDIVER:::dge_sample_column_status(object, "Batch")$valid)
    expect_identical(cellDIVER:::dge_counts_assays(object, c("X", "protein")), "X")
  })

  test_that("subsets and metadata updates work on file-backed objects", {
    object <- cellDIVER:::load_single_cell_object(h5ad)
    subset <- cellDIVER:::make_subset(
      object,
      list(list(type = "categorical", var = "Batch", value = "BM_200AB"))
    )
    expect_s3_class(subset, "scuba_source")
    expect_identical(
      SCUBA::get_all_cells(subset),
      SCUBA::fetch_cells(object, "Batch", "BM_200AB")
    )
    table <- SCUBA::fetch_metadata(subset, full_table = TRUE)
    table$metacluster <- ifelse(table$condensed_cell_type == "B Cells", "B", "other")
    updated <- cellDIVER:::update_object_metadata(subset, table)
    expect_identical(unname(SCUBA::n_unique(updated, "metacluster")), 2L)
    expect_identical(
      cellDIVER:::subset_filter_type_choices(object),
      c("Select Type" = "", "Categorical Metadata" = "categorical", "Feature Expression" = "numeric")
    )
  })

  test_that("the edgeR preflight and run work on a file-backed object", {
    skip_if_not_installed("edgeR")
    object <- cellDIVER:::load_single_cell_object(h5ad)
    # Batch has one sample per level: every contrast falls back
    inputs <- list(
      sample_by = "Batch", group_by = "Batch", dge_mode = "mode_dge",
      contrast_mode = "single", group_1 = "BM_200AB", group_2 = "PBMC_200AB",
      groups = NULL, reference_group = NULL, min_cells = 10L, filters = list()
    )
    preflight <- cellDIVER:::dge_edger_preflight(object, inputs)
    expect_identical(preflight$contrasts$method, "wilcoxon")
    # a paired contrast within the two batches runs edgeR
    paired <- cellDIVER:::dge_edger_preflight(object, modifyList(inputs, list(
      group_by = "condensed_cell_type", group_1 = "B Cells", group_2 = "NK Cells",
      min_cells = 5L
    )))
    expect_identical(paired$contrasts$method, "edgeR")
    expect_identical(paired$contrasts$design_type, "paired")
    result <- suppressWarnings(scDE::run_dge(
      object, group_by = "condensed_cell_type", layer = "counts", test_use = "edgeR",
      sample_by = "Batch", group_1 = "B Cells", group_2 = "NK Cells", min_cells = 5L
    ))
    expect_identical(attr(result, "edger_details")$aggregation$engine, "julia")
  })
}
