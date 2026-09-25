suppressPackageStartupMessages({
  library(shiny)
  library(shinyBS)
  library(shinyjs)
  library(shinyWidgets)
})

dge_method_test_environment <- new.env(parent = globalenv())
package_root <- if (dir.exists("R")) "." else file.path("..", "..")
sys.source(
  file.path(package_root, "R", "module-threshold_picker.R"),
  envir = dge_method_test_environment
)
sys.source(
  file.path(package_root, "R", "module-dge_test_selection.R"),
  envir = dge_method_test_environment
)
sys.source(
  file.path(package_root, "R", "module-subset_stats.R"),
  envir = dge_method_test_environment
)
dge_method_choices <- dge_method_test_environment$dge_method_choices
resolve_dge_method <- dge_method_test_environment$resolve_dge_method
dge_sample_column_status <- dge_method_test_environment$dge_sample_column_status
dge_method_label <- dge_method_test_environment$dge_method_label
dge_method_input_value <- dge_method_test_environment$dge_method_input_value
dge_test_selections_ui <- dge_method_test_environment$dge_test_selections_ui
subset_stats_ui <- dge_method_test_environment$subset_stats_ui
dge_mode_description <- dge_method_test_environment$dge_mode_description

test_that("edgeR is offered only for standard two-group DGE", {
  expect_identical(
    dge_method_choices("mode_dge", FALSE),
    c("Wilcoxon rank-sum" = "wilcoxon", "edgeR pseudobulk" = "edger")
  )
  expect_identical(
    dge_method_choices("mode_marker", FALSE),
    c("Wilcoxon rank-sum" = "wilcoxon")
  )
  expect_identical(
    dge_method_choices("mode_dge", TRUE),
    c("Wilcoxon rank-sum" = "wilcoxon")
  )
})

test_that("unsupported method combinations resolve safely to Wilcoxon", {
  expect_identical(resolve_dge_method("edger", "mode_dge", FALSE), "edger")
  expect_identical(resolve_dge_method("edger", "mode_marker", FALSE), "wilcoxon")
  expect_identical(resolve_dge_method("edger", "mode_dge", TRUE), "wilcoxon")
  expect_identical(resolve_dge_method(NULL, "mode_dge", FALSE), "wilcoxon")
  expect_identical(resolve_dge_method("unknown", "mode_dge", FALSE), "wilcoxon")
})

test_that("submitted DGE methods have accurate result labels", {
  expect_identical(
    dge_method_label("edger"),
    "edgeR pseudobulk quasi-likelihood"
  )
  expect_identical(dge_method_label("wilcoxon"), "Wilcoxon rank sum")
  expect_identical(dge_method_label(NULL), "Wilcoxon rank sum")
})

test_that("method input initialization is null-safe", {
  choices <- dge_method_choices("mode_dge", FALSE)

  expect_identical(dge_method_input_value(NULL, choices), "wilcoxon")
  expect_identical(dge_method_input_value(character(), choices), "wilcoxon")
  expect_identical(dge_method_input_value("edger", choices), "edger")
  expect_identical(dge_method_input_value("unsupported", choices), "wilcoxon")
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

test_that("DGE summary preserves submitted contrast direction", {
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
    as.character(
      dge_mode_description("mode_marker", c("A", "B", "C"))
    ),
    "Marker Identification (3 groups)"
  )
})

test_that("DGE selection UI contains method and edgeR controls", {
  ui <- shiny::isolate(
    dge_test_selections_ui(
      id = "test",
      meta_choices = shiny::reactive(c("Condition" = "condition"))
    )
  )
  rendered <- htmltools::renderTags(ui)$html

  expect_match(rendered, "test-method", fixed = TRUE)
  expect_match(rendered, "test-edger_min_cells", fixed = TRUE)
  expect_match(rendered, "Minimum Cells per Pseudobulk Sample", fixed = TRUE)
  expect_match(rendered, "value=\"10\"", fixed = TRUE)
  expect_match(rendered, "test-edger_sample_status", fixed = TRUE)
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
})
