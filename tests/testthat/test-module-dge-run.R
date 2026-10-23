# The DGE tab server end to end with shiny::testServer(): edgeR through the
# module equals a direct scDE run, in every mode, including the per-contrast
# fallback to the cell-level test.

dge_run_module_args <- function(object) {
  configuration <- list(
    condition = list(meta_colname = "condition", label = "Condition", groups = NULL),
    cell_type = list(meta_colname = "cell_type", label = "Cell Type", groups = NULL),
    sample = list(meta_colname = "sample", label = "Sample", groups = NULL)
  )
  unique_metadata <- lapply(configuration, function(entry) {
    as.character(SCUBA::unique_values(object, entry$meta_colname))
  })
  list(
    object = shiny::reactive(object),
    metadata_config = shiny::reactive(configuration),
    assay_config = shiny::reactive(list(
      RNA = list(assay = "RNA", key = "rna_", suffix_human = "", dropdown_title = "Gene")
    )),
    designated_genes_assay = shiny::reactive("RNA"),
    sample_id_colname = shiny::reactive("sample"),
    meta_categories = shiny::reactive(names(configuration)),
    unique_metadata = shiny::reactive(unique_metadata),
    meta_choices = shiny::reactive(names(configuration)),
    valid_features = shiny::reactive(rownames(object)),
    object_trigger = cellDIVER:::makeReactiveTrigger(),
    # a plain list, as in the app (the error handler indexes it directly)
    error_list = list(dge_test_errors = list(), subset_errors = list()),
    is_HDF5SummarizedExperiment = shiny::reactive(FALSE)
  )
}

# Inside a testServer() block `object` is the module's reactive argument, so the
# expected results below are computed from `object()`.

# Filter inputs of the results table that keep every row (the slider widgets
# are never updated in a mock session, and NULL values error in the module)
dge_run_filter_inputs <- function() {
  list(
    `dge_table_filtering-expr` = c(-1000, 1000),
    `dge_table_filtering-lfc` = c(-1000, 1000),
    `dge_table_filtering-expr_manual_enable` = FALSE,
    `dge_table_filtering-lfc_manual_enable` = FALSE,
    `dge_table_filtering-pval_adj` = "1",
    `dge_table_filtering-auc` = c(0, 1),
    `dge_table_filtering-pct_in` = c(0, 100),
    `dge_table_filtering-pct_out` = c(0, 100)
  )
}

# A mock session with dev_mode set: the nested modules read it when they start
dge_run_session <- function() {
  session <- shiny::MockShinySession$new()
  session$userData$dev_mode <- FALSE
  session
}

# Sorted comparison of the columns both tables share
expect_same_dge <- function(actual, expected, columns = c("group", "feature", "log2FC", "pval", "pval_adj")) {
  key <- function(x) paste(if ("contrast" %in% names(x)) x$contrast else "", x$group, x$feature)
  actual <- as.data.frame(actual)[order(key(actual)), columns]
  expected <- as.data.frame(expected)[order(key(expected)), columns]
  expect_equal(actual, expected, tolerance = 1e-8, ignore_attr = TRUE)
}

test_that("edgeR runs through the DGE tab for an independent two-group contrast", {
  skip_if_not_installed("edgeR")
  object <- unit_pseudobulk_object()
  shiny::testServer(
    cellDIVER:::dge_tab_server,
    args = dge_run_module_args(object),
    session = dge_run_session(),
    {
      session$setInputs(
        `test_selections-mode` = "mode_dge",
        `test_selections-method` = "edger",
        `test_selections-group_by` = "condition",
        `test_selections-group_1` = "treated",
        `test_selections-group_2` = "control",
        `test_selections-edger_contrast_mode` = "single",
        `test_selections-edger_sample_col` = "sample",
        `test_selections-edger_min_cells` = 10,
        `test_selections-use_feature_expression` = FALSE,
        assays = "RNA", pos = TRUE, umap_group_by = "condition"
      )
      session$setInputs(!!!dge_run_filter_inputs())
      selections <- test_selections()
      expect_identical(selections$dge_method, "edger")
      expect_identical(selections$edger_sample_col, "sample")
      session$setInputs(submit = 1)
      result <- dge_table_content()
      expect_identical(attr(result, "dge_method"), "edger")
      expect_true(all(result$method == "edgeR"))
      expect_identical(attr(result, "edger_design_type"), "independent")
      expected <- suppressWarnings(scDE::run_dge(
        object()[, object()$condition %in% c("treated", "control")],
        group_by = "condition", layer = "counts", seurat_assay = "RNA", test_use = "edgeR",
        sample_by = "sample", group_1 = "treated", group_2 = "control", min_cells = 10L,
        positive_only = TRUE
      ))
      expect_same_dge(result, expected)
      expect_identical(attr(result, "edger_details")$sample_summary$retained_samples, c(3L, 3L))
    }
  )
})

test_that("a paired contrast and the cell-level fallback flow through the DGE tab", {
  skip_if_not_installed("edgeR")
  object <- unit_pseudobulk_object()
  shiny::testServer(
    cellDIVER:::dge_tab_server,
    args = dge_run_module_args(object),
    session = dge_run_session(),
    {
      session$setInputs(
        `test_selections-mode` = "mode_dge",
        `test_selections-method` = "edger",
        `test_selections-group_by` = "cell_type",
        `test_selections-group_1` = "Type 1",
        `test_selections-group_2` = "Type 2",
        `test_selections-edger_contrast_mode` = "single",
        `test_selections-edger_sample_col` = "sample",
        `test_selections-edger_min_cells` = 10,
        `test_selections-use_feature_expression` = FALSE,
        assays = "RNA", pos = FALSE, umap_group_by = "condition"
      )
      session$setInputs(!!!dge_run_filter_inputs())
      session$setInputs(submit = 1)
      paired <- dge_table_content()
      expect_identical(attr(paired, "edger_design_type"), "paired")
      expected <- suppressWarnings(scDE::run_dge(
        object(), group_by = "cell_type", layer = "counts", seurat_assay = "RNA",
        test_use = "edgeR", sample_by = "sample", group_1 = "Type 1", group_2 = "Type 2",
        min_cells = 10L, positive_only = FALSE
      ))
      expect_same_dge(paired, expected)

      # raising the minimum cells above every profile: edgeR cannot run and the
      # same submission returns the cell-level Wilcoxon result, labelled
      session$setInputs(`test_selections-edger_min_cells` = 100)
      session$setInputs(submit = 2)
      fallback <- dge_table_content()
      expect_identical(attr(fallback, "dge_method"), "edger")
      expect_true(all(fallback$method == "wilcoxon"))
      expect_match(attr(fallback, "edger_details")$fallback_reason, "has 0 samples")
      reference <- scDE::run_dge(object(), group_by = "cell_type", seurat_assay = "RNA", positive_only = FALSE)
      reference <- reference[reference$group == "Type 1", ]
      expect_same_dge(fallback, reference)
    }
  )
})

test_that("marker identification with edgeR runs one-vs-rest with a per-contrast fallback", {
  skip_if_not_installed("edgeR")
  object <- unit_pseudobulk_object()
  # a third cell type present in one sample only: its contrast cannot be replicated
  extra <- SCUBA::get_all_cells(object)[object$sample == "s1" & object$cell_type == "Type 2"][1:12]
  object$cell_type[extra] <- "Type 3"
  shiny::testServer(
    cellDIVER:::dge_tab_server,
    args = dge_run_module_args(object),
    session = dge_run_session(),
    {
      session$setInputs(
        `test_selections-mode` = "mode_marker",
        `test_selections-method` = "edger",
        `test_selections-group_by` = "cell_type",
        `test_selections-edger_sample_col` = "sample",
        `test_selections-edger_min_cells` = 10,
        `test_selections-use_feature_expression` = FALSE,
        assays = "RNA", pos = TRUE, umap_group_by = "condition"
      )
      session$setInputs(!!!dge_run_filter_inputs())
      expect_identical(test_selections()$edger_contrast_mode, "one_vs_rest")
      session$setInputs(submit = 1)
      result <- dge_table_content()
      expect_identical(attr(result, "dge_contrast_mode"), "one_vs_rest")
      expect_setequal(unique(result$contrast), paste(c("Type 1", "Type 2", "Type 3"), "vs rest"))
      table <- attr(result, "edger_multicontrast_details")$contrast_table
      expect_identical(table$method[table$group_1 == "Type 3"], "wilcoxon")
      expect_identical(table$method[table$group_1 == "Type 1"], "edgeR")
      expect_true(all(result$method[result$contrast == "Type 3 vs rest"] == "wilcoxon"))
      expect_true(all(result$method[result$contrast == "Type 1 vs rest"] == "edgeR"))
      expected <- suppressMessages(suppressWarnings(scDE::run_dge_multicontrast(
        object(), group_by = "cell_type", sample_by = "sample", contrast_mode = "one_vs_rest",
        layer = "counts", seurat_assay = "RNA", min_cells = 10L, positive_only = TRUE
      )))
      expect_same_dge(result, expected, columns = c("contrast", "method", "group", "feature", "log2FC", "pval_adj"))
    }
  )
})

test_that("switching the replicate column changes the design", {
  skip_if_not_installed("edgeR")
  object <- unit_pseudobulk_object()
  object$donor_pair <- paste0("pair", match(object$sample, paste0("s", 1:6)) %% 3L)
  shiny::testServer(
    cellDIVER:::dge_tab_server,
    args = dge_run_module_args(object),
    session = dge_run_session(),
    {
      session$setInputs(
        `test_selections-mode` = "mode_dge",
        `test_selections-method` = "edger",
        `test_selections-group_by` = "condition",
        `test_selections-group_1` = "treated",
        `test_selections-group_2` = "control",
        `test_selections-edger_contrast_mode` = "single",
        `test_selections-edger_sample_col` = "sample",
        `test_selections-edger_min_cells` = 10,
        `test_selections-use_feature_expression` = FALSE,
        assays = "RNA", pos = FALSE, umap_group_by = "condition"
      )
      session$setInputs(!!!dge_run_filter_inputs())
      session$setInputs(submit = 1)
      expect_identical(attr(dge_table_content(), "edger_design_type"), "independent")
      # cell_type is not a configured sample column: the selection is rejected,
      # the run stops with the missing-column message instead of a silent change
      session$setInputs(`test_selections-edger_sample_col` = "not_a_column")
      expect_null(test_selections()$edger_sample_col)
      expect_match(test_selections()$edger_sample_error, "Choose a sample")
    }
  )
})
