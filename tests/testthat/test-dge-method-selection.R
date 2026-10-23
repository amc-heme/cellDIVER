suppressPackageStartupMessages({
  library(shiny)
  library(shinyBS)
  library(shinyjs)
  library(shinyWidgets)
})

dge_method_test_environment <- new.env(parent = globalenv())
package_root <- if (dir.exists("R")) "." else file.path("..", "..")
for (file in c(
  "module-threshold_picker.R", "module-dge_test_selection.R",
  "module-subset_stats.R", "vector_to_text.R"
)) {
  sys.source(
    file.path(package_root, "R", file),
    envir = dge_method_test_environment
  )
}
dge_method_choices <- dge_method_test_environment$dge_method_choices
resolve_dge_method <- dge_method_test_environment$resolve_dge_method
dge_sample_column_status <- dge_method_test_environment$dge_sample_column_status
dge_sample_column_candidates <-
  dge_method_test_environment$dge_sample_column_candidates
dge_sample_column_selection <-
  dge_method_test_environment$dge_sample_column_selection
dge_object_supports_edger <- dge_method_test_environment$dge_object_supports_edger
dge_method_label <- dge_method_test_environment$dge_method_label
dge_method_input_value <- dge_method_test_environment$dge_method_input_value
resolve_edger_contrast_mode <-
  dge_method_test_environment$resolve_edger_contrast_mode
show_feature_expression_control <-
  dge_method_test_environment$show_feature_expression_control
dge_test_selections_ui <- dge_method_test_environment$dge_test_selections_ui
subset_stats_ui <- dge_method_test_environment$subset_stats_ui
dge_mode_description <- dge_method_test_environment$dge_mode_description
dge_results_method_label <- dge_method_test_environment$dge_results_method_label
edger_sample_summary_text <-
  dge_method_test_environment$edger_sample_summary_text
dge_preflight_cells <- dge_method_test_environment$dge_preflight_cells
dge_preflight_group_labels <-
  dge_method_test_environment$dge_preflight_group_labels
dge_preflight_contrasts <- dge_method_test_environment$dge_preflight_contrasts
dge_edger_preflight <- dge_method_test_environment$dge_edger_preflight
edger_preflight_text <- dge_method_test_environment$edger_preflight_text
edger_preflight_ui <- dge_method_test_environment$edger_preflight_ui

test_that("edgeR is offered in both modes when a sample column is available", {
  expect_identical(
    dge_method_choices("mode_dge", FALSE),
    c("Wilcoxon rank-sum" = "wilcoxon", "edgeR pseudobulk" = "edger")
  )
  expect_identical(
    dge_method_choices("mode_marker", FALSE),
    c("Wilcoxon rank-sum" = "wilcoxon", "edgeR pseudobulk" = "edger")
  )
  # groups defined by a feature expression threshold stay cell-level
  expect_identical(
    dge_method_choices("mode_dge", TRUE),
    c("Wilcoxon rank-sum" = "wilcoxon")
  )
  # no sample column: no edgeR
  expect_identical(
    dge_method_choices("mode_dge", FALSE, sample_column_available = FALSE),
    c("Wilcoxon rank-sum" = "wilcoxon")
  )
  # the mode input is NULL while the tab initializes: never NA, never an error
  expect_identical(
    dge_method_choices(NULL, FALSE),
    c("Wilcoxon rank-sum" = "wilcoxon")
  )
  expect_identical(resolve_dge_method("edger", NULL, FALSE), "wilcoxon")
})

test_that("unsupported method combinations resolve safely to Wilcoxon", {
  expect_identical(resolve_dge_method("edger", "mode_dge", FALSE), "edger")
  expect_identical(resolve_dge_method("edger", "mode_marker", FALSE), "edger")
  expect_identical(resolve_dge_method("edger", "mode_dge", TRUE), "wilcoxon")
  expect_identical(
    resolve_dge_method("edger", "mode_dge", FALSE, sample_column_available = FALSE),
    "wilcoxon"
  )
  expect_identical(resolve_dge_method(NULL, "mode_dge", FALSE), "wilcoxon")
  expect_identical(resolve_dge_method("unknown", "mode_dge", FALSE), "wilcoxon")
})

test_that("submitted DGE methods have accurate result labels", {
  expect_identical(
    dge_method_label("edger"),
    "edgeR pseudobulk quasi-likelihood"
  )
  expect_identical(
    dge_method_label("edger", design_type = "paired"),
    "edgeR pseudobulk quasi-likelihood (paired by sample)"
  )
  expect_match(dge_method_label("edger", fallback = TRUE), "edgeR could not run")
  expect_identical(dge_method_label("wilcoxon"), "Wilcoxon rank sum")
  expect_identical(dge_method_label(NULL), "Wilcoxon rank sum")

  results <- data.frame(
    method = c("edgeR", "edgeR", "wilcoxon"),
    contrast = c("A vs B", "A vs B", "A vs C")
  )
  attr(results, "edger_design_type") <- "paired"
  expect_identical(
    dge_results_method_label(results, "edger"),
    paste0(
      "edgeR pseudobulk quasi-likelihood (paired by sample); ",
      "cell-level Wilcoxon fallback for 1 contrast"
    )
  )
  only_fallback <- data.frame(method = "wilcoxon", contrast = "A vs B")
  expect_match(dge_results_method_label(only_fallback, "edger"), "could not run")
  expect_identical(dge_results_method_label(results, "wilcoxon"), "Wilcoxon rank sum")
})

test_that("method input initialization is null-safe", {
  choices <- dge_method_choices("mode_dge", FALSE)

  expect_identical(dge_method_input_value(NULL, choices), "wilcoxon")
  expect_identical(dge_method_input_value(character(), choices), "wilcoxon")
  expect_identical(dge_method_input_value("edger", choices), "edger")
  expect_identical(dge_method_input_value("unsupported", choices), "wilcoxon")
})

test_that("edgeR contrast mode initialization is null-safe", {
  expect_identical(resolve_edger_contrast_mode(NULL), "single")
  expect_identical(resolve_edger_contrast_mode("pairwise"), "pairwise")
  expect_identical(resolve_edger_contrast_mode("one_vs_rest"), "one_vs_rest")
  expect_identical(resolve_edger_contrast_mode("reference"), "reference")
  expect_identical(resolve_edger_contrast_mode(NA_character_), "single")
  expect_identical(resolve_edger_contrast_mode("unknown"), "single")
})

test_that("feature-expression grouping is hidden for edgeR", {
  expect_true(show_feature_expression_control("mode_dge", "wilcoxon"))
  expect_false(show_feature_expression_control("mode_dge", "edger"))
  expect_false(show_feature_expression_control("mode_marker", "wilcoxon"))
})

test_that("DGE result summary reserves dynamic method and edgeR outputs", {
  rendered <- as.character(
    subset_stats_ui(
      id = "summary",
      tab = "dge",
      metadata_config = reactive(list()),
      meta_categories = reactive(character())
    )
  )

  expect_match(rendered, "summary-dge_method", fixed = TRUE)
  expect_match(rendered, "summary-edger_sample_summary", fixed = TRUE)
  expect_false(grepl(">Wilcoxon rank sum<", rendered, fixed = TRUE))
})

test_that("DGE summary preserves submitted contrast direction and design", {
  observed <- dge_mode_description(
    mode = "mode_dge",
    observed_groups = c("Control", "Treated"),
    group_1 = "Treated",
    group_2 = "Control"
  )

  expect_identical(
    as.character(observed),
    "Differential Expression (Treated vs. Control)"
  )
  expect_identical(
    as.character(dge_mode_description(
      "mode_dge", c("A", "B"), group_1 = "A", group_2 = "B", design_type = "paired"
    )),
    "Differential Expression (A vs. B, paired by sample)"
  )
  expect_identical(
    as.character(
      dge_mode_description("mode_marker", c("A", "B", "C"))
    ),
    "Marker Identification (3 groups)"
  )
  expect_identical(
    as.character(
      dge_mode_description("mode_marker", c("A", "B", "C"), contrast_mode = "one_vs_rest")
    ),
    "Marker Identification (3 groups; edgeR one-vs-rest)"
  )
})

test_that("DGE selection UI contains method, sample column and edgeR controls", {
  ui <- shiny::isolate(
    dge_test_selections_ui(
      id = "test",
      meta_choices = shiny::reactive(c("Condition" = "condition"))
    )
  )
  rendered <- htmltools::renderTags(ui)$html

  expect_match(rendered, "test-method", fixed = TRUE)
  expect_match(rendered, "test-edger_sample_col", fixed = TRUE)
  expect_match(rendered, "test-edger_min_cells", fixed = TRUE)
  expect_match(rendered, "test-edger_contrast_mode", fixed = TRUE)
  expect_match(rendered, "test-edger_groups", fixed = TRUE)
  expect_match(rendered, "test-edger_reference_group", fixed = TRUE)
  expect_match(rendered, "All pairwise comparisons", fixed = TRUE)
  expect_match(rendered, "Each selected group vs. rest", fixed = TRUE)
  expect_match(rendered, "Minimum Cells per Pseudobulk Sample", fixed = TRUE)
  expect_match(rendered, "value=\"10\"", fixed = TRUE)
  expect_match(rendered, "test-edger_sample_status", fixed = TRUE)
})

test_that("multi-contrast summaries retain contrast, design and fallback information", {
  results <- data.frame(feature = "gene1")
  detail <- list(
    method = "edgeR",
    design_type = "paired",
    sample_summary = data.frame(
      group = c("A", "B"),
      retained_samples = c(3L, 2L)
    ),
    excluded_profiles = data.frame(sample = "sample3")
  )
  fallback <- list(method = "wilcoxon", fallback_reason = "'C' has 1 sample")
  attr(results, "edger_multicontrast_details") <- list(
    contrasts = list("A vs B" = detail, "A vs C" = fallback)
  )

  expect_identical(
    edger_sample_summary_text(results),
    paste0(
      "A vs B [paired; A: 3; B: 2; excluded: 1] | ",
      "A vs C [cell-level Wilcoxon ('C' has 1 sample)]"
    )
  )
  expect_identical(
    dge_mode_description(
      "mode_dge", c("A", "B", "C"),
      contrast_mode = "pairwise", n_contrasts = 3L
    ),
    "Differential Expression (3 edgeR pairwise contrasts)"
  )
  single <- data.frame(feature = "gene1")
  attr(single, "edger_details") <- detail
  expect_identical(
    edger_sample_summary_text(single),
    "Paired by sample. A: 3; B: 2. 1 sample/group pseudobulk profile(s) excluded."
  )
  attr(single, "edger_details") <- fallback
  expect_identical(
    edger_sample_summary_text(single),
    "cell-level Wilcoxon ('C' has 1 sample)"
  )
})

test_that("configured biological sample columns are resolved for edgeR", {
  skip_if_not_installed("SeuratObject")
  counts <- matrix(
    c(1, 0, 2, 1),
    nrow = 2,
    dimnames = list(c("gene1", "gene2"), c("cell1", "cell2"))
  )
  metadata <- data.frame(
    sample_id = c("sample1", "sample2"),
    row.names = colnames(counts)
  )
  object <- suppressWarnings(
    SeuratObject::CreateSeuratObject(counts, meta.data = metadata)
  )

  valid <- dge_sample_column_status(object, "sample_id")
  expect_true(valid$valid)
  expect_identical(valid$column, "sample_id")
  expect_null(valid$error)

  missing_config <- dge_sample_column_status(object, NULL)
  expect_false(missing_config$valid)
  expect_match(missing_config$error, "No biological sample")

  missing_column <- dge_sample_column_status(object, "unknown_sample")
  expect_false(missing_column$valid)
  expect_match(missing_column$error, "not present")

  # file-backed objects: SCUBA reads the R-side metadata table
  fake_source <- structure(
    list(meta = data.frame(sample = c("a", "b"), row.names = c("c1", "c2"))),
    class = "scuba_source"
  )
  expect_true(dge_sample_column_status(fake_source, "sample")$valid)
  expect_true(dge_object_supports_edger(fake_source))
  expect_false(dge_object_supports_edger(structure(list(), class = "md._core.mudata.MuData")))
})

test_that("sample column candidates and the default selection", {
  object <- unit_single_cell_object()
  unique_metadata <- list(
    cell_type = c("Type 1", "Type 2", "Type 3"),
    condition = c("control", "treated"),
    constant = "only"
  )
  expect_identical(
    dge_sample_column_candidates(object, unique_metadata, NULL),
    c("cell_type", "condition")
  )
  # a configured column outside the exposed categories is offered first
  object$donor <- rep(c("d1", "d2", "d3"), 2)
  expect_identical(
    dge_sample_column_candidates(object, unique_metadata, "donor"),
    c("donor", "cell_type", "condition")
  )
  expect_identical(
    dge_sample_column_selection(NULL, "donor", c("donor", "cell_type")), "donor"
  )
  expect_identical(
    dge_sample_column_selection("cell_type", "donor", c("donor", "cell_type")),
    "cell_type"
  )
  expect_identical(
    dge_sample_column_selection("gone", NULL, c("donor", "cell_type")), "donor"
  )
  expect_identical(dge_sample_column_selection("x", "y", character()), character(0))
})

test_that("preflight helpers resolve cells, labels and contrasts from metadata", {
  object <- unit_single_cell_object()
  expect_null(dge_preflight_cells(object, list()))
  filters <- list(
    list(type = "categorical", var = "cell_type", value = c("Type 1", "Type 2")),
    list(type = "numeric", var = "GeneAlpha", value = 1, mode = "greater_than")
  )
  expect_identical(
    dge_preflight_cells(object, filters),
    paste0("cell_", c("one", "two", "three", "four"))
  )
  labels <- dge_preflight_group_labels(
    object, "cell_type", contrast_mode = "single",
    group_1 = "Type 1", group_2 = c("Type 2", "Type 3")
  )
  expect_identical(labels$group_2, vector_to_text(c("Type 2", "Type 3")))
  expect_identical(
    unname(labels$labels),
    c("Type 1", "Type 1", rep(vector_to_text(c("Type 2", "Type 3")), 4))
  )
  expect_identical(names(labels$labels), SCUBA::get_all_cells(object))
  raw <- dge_preflight_group_labels(object, "cell_type", contrast_mode = "pairwise")
  expect_identical(unname(raw$labels), as.character(object$cell_type))

  groups <- c("A", "B", "C")
  expect_null(dge_preflight_contrasts("mode_dge", "single", groups))
  expect_identical(
    dge_preflight_contrasts("mode_dge", "single", groups, group_1 = "A", group_2 = "B"),
    data.frame(group_1 = "A", group_2 = "B", stringsAsFactors = FALSE)
  )
  expect_identical(
    dge_preflight_contrasts("mode_marker", "one_vs_rest", groups)$group_2,
    rep("rest", 3)
  )
  expect_identical(
    nrow(dge_preflight_contrasts("mode_dge", "pairwise", groups)), 3L
  )
  expect_identical(
    nrow(dge_preflight_contrasts("mode_dge", "pairwise", groups, groups = c("A", "B"))), 1L
  )
  expect_null(dge_preflight_contrasts("mode_dge", "pairwise", groups, groups = "A"))
  reference <- dge_preflight_contrasts("mode_dge", "reference", groups, reference_group = "C")
  expect_identical(reference$group_1, c("A", "B"))
  expect_true(all(reference$group_2 == "C"))
  expect_null(dge_preflight_contrasts("mode_dge", "reference", groups, reference_group = "Z"))
  expect_identical(
    dge_preflight_contrasts("mode_dge", "one_vs_rest", groups, groups = "B")$group_1, "B"
  )
})

test_that("the preflight runs scDE's replication rules and renders a per-contrast plan", {
  skip_if_not_installed("scDE")
  skip_if_not(exists("check_pseudobulk_design", asNamespace("scDE")), "scDE without check_pseudobulk_design()")
  object <- unit_pseudobulk_object()
  inputs <- list(
    sample_by = "sample", group_by = "condition", dge_mode = "mode_dge",
    contrast_mode = "single", group_1 = "treated", group_2 = "control",
    groups = NULL, reference_group = NULL, min_cells = 10L, filters = list()
  )
  preflight <- dge_edger_preflight(object, inputs)
  expect_identical(preflight$status, "ready")
  expect_identical(preflight$contrasts$method, "edgeR")
  expect_identical(preflight$contrasts$design_type, "independent")
  expect_identical(preflight$contrasts$samples_group_1, 3L)
  expect_match(edger_preflight_text(preflight), "edgeR \\(independent samples, 3 vs. 3\\)")
  rendered <- as.character(edger_preflight_ui(preflight, "sample"))
  expect_match(rendered, "edgeR: 1 contrast", fixed = TRUE)
  expect_match(rendered, "text-success", fixed = TRUE)

  # cell type within donors: paired
  paired <- dge_edger_preflight(object, modifyList(inputs, list(
    group_by = "cell_type", group_1 = "Type 1", group_2 = "Type 2"
  )))
  expect_identical(paired$contrasts$design_type, "paired")
  expect_identical(paired$contrasts$paired_samples, 6L)

  # too few cells per profile: the contrast falls back, the plan says so
  sparse <- dge_edger_preflight(object, modifyList(inputs, list(min_cells = 100L)))
  expect_identical(sparse$contrasts$method, "wilcoxon")
  expect_true(sparse$any_fallback)
  expect_match(edger_preflight_text(sparse), "cell-level Wilcoxon")
  expect_match(as.character(edger_preflight_ui(sparse, "sample")), "alert-warning", fixed = TRUE)

  # marker mode: every cell type vs the rest; a subset filter restricts the cells
  # (built explicitly: modifyList() merges nested lists and drops NULLs)
  marker_inputs <- inputs
  marker_inputs$dge_mode <- "mode_marker"
  marker_inputs$contrast_mode <- "one_vs_rest"
  marker_inputs$group_by <- "cell_type"
  marker_inputs$group_1 <- NULL
  marker_inputs$group_2 <- NULL
  marker_inputs$filters <- list(
    list(type = "categorical", var = "condition", value = "treated")
  )
  marker <- dge_edger_preflight(object, marker_inputs)
  expect_identical(marker$contrasts$group_1, c("Type 1", "Type 2"))
  expect_identical(marker$contrasts$samples_group_1, c(3L, 3L))
  expect_identical(marker$contrasts$design_type, c("paired", "paired"))

  # incomplete selections and the missing-column state render without error
  incomplete <- inputs
  incomplete$group_1 <- NULL
  expect_identical(dge_edger_preflight(object, incomplete)$status, "incomplete")
  expect_match(as.character(edger_preflight_ui(NULL, NULL)), "Choose a sample", fixed = TRUE)
  expect_match(as.character(edger_preflight_ui(NULL, "sample")), "Checking", fixed = TRUE)
  expect_match(
    as.character(edger_preflight_ui(list(status = "error", message = "boom"), "sample")),
    "boom", fixed = TRUE
  )
})
