# Browser scenarios for pseudobulk edgeR, on a fixture with three samples per
# batch (see apps/cellDIVER_pseudobulk/pseudobulk_fixture.R).

# `cleanup_environment` must be the calling test's frame, not this wrapper's:
# otherwise the app is stopped as soon as the wrapper returns.
pseudobulk_app <- function(name, cleanup_environment = parent.frame()) {
  browser_app(
    name, fixture = "cellDIVER_pseudobulk",
    cleanup_environment = cleanup_environment
  )
}

test_that("browser edgeR run between the two batches matches scDE", {
  app <- pseudobulk_app("dge-pseudobulk-batches")
  browser_open_tab(app, "dge", "object_dge-test_selections-group_by")
  browser_set(app, "object_dge-test_selections-mode", "mode_dge")
  browser_select_method(app, "edger")
  # the configured replicate column is preselected
  app$wait_for_js(
    "document.getElementById('object_dge-test_selections-edger_sample_col').value === 'sample'",
    timeout = 30000
  )
  browser_set(app, "object_dge-test_selections-group_by", "Batch")
  browser_set(app, "object_dge-test_selections-group_1", "BM_200AB")
  browser_set(app, "object_dge-test_selections-group_2", "PBMC_200AB")
  browser_set(app, "object_dge-test_selections-edger_min_cells", 3)
  # the preflight plan reports three independent samples per batch
  app$wait_for_js(
    paste0(
      "(document.getElementById('object_dge-test_selections-edger_sample_status') || {})",
      ".textContent.includes('independent samples, 3 vs. 3')"
    ),
    timeout = 30000
  )
  object <- browser_pseudobulk_object()
  expected <- suppressWarnings(scDE::run_dge(
    object, group_by = "Batch", layer = "counts", seurat_assay = "RNA",
    test_use = "edgeR", sample_by = "sample", group_1 = "BM_200AB",
    group_2 = "PBMC_200AB", min_cells = 3L, positive_only = TRUE
  ))
  browser_click(app, "object_dge-submit")
  actual <- browser_expect_pseudobulk(app, expected)
  expect_true(all(actual$method == "edgeR"))
  expect_match(
    app$get_value(output = "object_dge-subset_stats-dge_method"),
    "edgeR pseudobulk"
  )
})

test_that("browser paired contrast reports its design and a sparse contrast falls back", {
  app <- pseudobulk_app("dge-pseudobulk-paired")
  browser_open_tab(app, "dge", "object_dge-test_selections-group_by")
  browser_set(app, "object_dge-test_selections-mode", "mode_dge")
  browser_select_method(app, "edger")
  browser_set(app, "object_dge-test_selections-group_by", "condensed_cell_type")
  browser_set(app, "object_dge-test_selections-group_1", "B Cells")
  browser_set(app, "object_dge-test_selections-group_2", "NK Cells")
  browser_set(app, "object_dge-test_selections-edger_min_cells", 2)
  app$wait_for_js(
    paste0(
      "(document.getElementById('object_dge-test_selections-edger_sample_status') || {})",
      ".textContent.includes('paired design')"
    ),
    timeout = 30000
  )
  object <- browser_pseudobulk_object()
  expected <- suppressWarnings(scDE::run_dge(
    object, group_by = "condensed_cell_type", layer = "counts", seurat_assay = "RNA",
    test_use = "edgeR", sample_by = "sample", group_1 = "B Cells", group_2 = "NK Cells",
    min_cells = 2L, positive_only = TRUE
  ))
  browser_click(app, "object_dge-submit")
  paired <- browser_expect_pseudobulk(app, expected)
  expect_identical(attr(paired, "edger_design_type"), "paired")
  expect_match(
    app$get_value(output = "object_dge-subset_stats-dge_method"),
    "paired by sample"
  )

  # a minimum far above every profile: the plan announces the fallback, the
  # run still completes and returns the cell-level result
  browser_set(app, "object_dge-test_selections-edger_min_cells", 100)
  app$wait_for_js(
    paste0(
      "(document.getElementById('object_dge-test_selections-edger_sample_status') || {})",
      ".textContent.includes('cell-level Wilcoxon')"
    ),
    timeout = 30000
  )
  expect_false(isTRUE(app$get_js("document.getElementById('object_dge-submit').disabled")))
  browser_click(app, "object_dge-submit")
  fallback <- app$wait_for_value(export = "object_dge-dge_table", ignore = list(NULL, paired))
  expect_true(all(fallback$method == "wilcoxon"))
  expect_match(
    app$get_value(output = "object_dge-subset_stats-dge_method"),
    "could not run"
  )
})

test_that("browser marker identification with edgeR tests every cell type vs the rest", {
  app <- pseudobulk_app("dge-pseudobulk-markers")
  browser_open_tab(app, "dge", "object_dge-test_selections-group_by")
  browser_select_method(app, "edger")
  browser_set(app, "object_dge-test_selections-group_by", "condensed_cell_type")
  browser_set(app, "object_dge-test_selections-edger_min_cells", 2)
  object <- browser_pseudobulk_object()
  expected <- suppressMessages(suppressWarnings(scDE::run_dge_multicontrast(
    object, group_by = "condensed_cell_type", sample_by = "sample",
    contrast_mode = "one_vs_rest", layer = "counts", seurat_assay = "RNA",
    min_cells = 2L, positive_only = TRUE
  )))
  browser_click(app, "object_dge-submit")
  actual <- browser_expect_pseudobulk(app, expected)
  expect_identical(attr(actual, "dge_contrast_mode"), "one_vs_rest")
  expect_setequal(unique(actual$contrast), unique(expected$contrast))
  # the cell type confined to one sample fell back to the cell-level test,
  # the replicated ones ran edgeR, and the table reports both
  table <- attr(expected, "edger_multicontrast_details")$contrast_table
  expect_identical(table$method[table$group_1 == "Rare cells"], "wilcoxon")
  expect_identical(table$method[table$group_1 == "B Cells"], "edgeR")
  expect_true(all(actual$method[actual$contrast == "Rare cells vs rest"] == "wilcoxon"))
  expect_true(all(actual$method[actual$contrast == "B Cells vs rest"] == "edgeR"))
  expect_match(
    app$get_value(output = "object_dge-subset_stats-dge_method"),
    "Wilcoxon fallback for 1 contrast"
  )
})
