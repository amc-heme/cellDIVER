test_that("empty subset criteria preserve the complete object", {
  object <- unit_single_cell_object()
  expect_identical(cellDIVER:::make_subset(object, list()), object)
})

test_that("categorical and numeric subset criteria are intersected", {
  object <- unit_single_cell_object()
  selected_object <- cellDIVER:::make_subset(
    object,
    list(
      list(
        type = "categorical", var = "cell_type", value = c("Type 1", "Type 2")
      ),
      list(
        type = "numeric", var = "quality_score", mode = "range", value = c(0, 1)
      )
    )
  )

  expect_identical(
    SCUBA::get_all_cells(selected_object),
    c("cell_two", "cell_three", "cell_four")
  )
  expect_identical(SCUBA::get_all_cells(object), colnames(object))
  expect_equal(cellDIVER:::n_cells(object), 6)
  expect_equal(cellDIVER:::n_cells(selected_object), 3)
})

test_that("numeric subset comparisons include their boundary values", {
  object <- unit_single_cell_object()
  filter_cases <- list(
    list(mode = "less_than", value = 1, cells = colnames(object)[1:4]),
    list(mode = "greater_than", value = 1, cells = colnames(object)[3:6]),
    list(mode = "range", value = c(0, 3), cells = colnames(object)[2:5])
  )

  for (filter_case in filter_cases) {
    selected_object <- cellDIVER:::make_subset(
      object,
      list(list(
        type = "numeric", var = "quality-score",
        mode = filter_case$mode, value = filter_case$value
      ))
    )
    expect_identical(
      SCUBA::get_all_cells(selected_object),
      filter_case$cells,
      info = filter_case$mode
    )
  }
})

test_that("subsetting supports keyed expression and advanced criteria", {
  object <- unit_single_cell_object()
  expression_subset <- cellDIVER:::make_subset(
    object,
    list(list(
      type = "numeric", var = "rna_GeneAlpha",
      mode = "greater_than", value = 2
    ))
  )
  expect_identical(
    SCUBA::get_all_cells(expression_subset),
    colnames(object)[3:6]
  )

  advanced_subset <- cellDIVER:::make_subset(
    object,
    list(list(
      type = "advanced",
      value = 'quality_score > 0 & condition == "control"'
    ))
  )
  expect_identical(
    SCUBA::get_all_cells(advanced_subset),
    c("cell_three", "cell_five")
  )
})

test_that("subset calls dispatch to the supported object argument names", {
  expect_identical(
    cellDIVER:::subset_call(
      unit_single_cell_object(), "quality_score >= 1"
    ),
    "subset(object, subset = quality_score >= 1)"
  )
  expect_identical(
    cellDIVER:::subset_call(
      unit_single_cell_experiment(), "quality_score >= 1"
    ),
    "subset(object, select = quality_score >= 1)"
  )
  expect_warning(
    cellDIVER:::subset_call(list(), "quality_score >= 1"),
    "does not know how to handle object of class list"
  )
})

test_that("threshold statistics count equality as above the threshold", {
  object <- unit_single_cell_object()

  expect_identical(
    cellDIVER:::threshold_stats(object, "rna_GeneAlpha", 2),
    list(
      n_total = 6L, n_above = 4L, percent_above = "66.67",
      n_below = 2L, percent_below = "33.33"
    )
  )
  for (threshold in c(-1, 6)) {
    statistics <- cellDIVER:::threshold_stats(
      object, "rna_GeneAlpha", threshold
    )
    expect_equal(statistics$n_above + statistics$n_below, statistics$n_total)
    expect_equal(statistics$n_above, if (threshold == -1) 6 else 0)
    expect_identical(
      statistics$percent_above,
      if (threshold == -1) "100.00" else "0.00"
    )
  }
  expect_error(
    cellDIVER:::threshold_stats(object, character(), 2),
    "Please enter only one feature"
  )
  expect_error(
    cellDIVER:::threshold_stats(object, c("rna_GeneAlpha", "rna_GeneBeta"), 2),
    "Please enter only one feature"
  )
})

test_that("threshold assays clamp selected features without changing source", {
  object <- unit_single_cell_object()
  threshold_object <- cellDIVER:::adt_threshold_assay(
    object,
    threshold_table = tibble::tibble(adt = "GeneAlpha", value = 2),
    designated_adt_assay = "RNA"
  )

  expect_identical(rownames(threshold_object[["adtThreshold"]]), "GeneAlpha")
  expect_equal(
    SCUBA::fetch_data(threshold_object, vars = "adtThreshold_GeneAlpha")[[1]],
    c(0, 0, 0, 0, 2, 3)
  )
  expect_equal(
    SCUBA::fetch_data(threshold_object, vars = "rna_GeneAlpha")[[1]],
    c(0, 1, 2, 2, 4, 5)
  )
  expect_false("adtThreshold" %in% Seurat::Assays(object))
})

test_that("metadata helpers classify values and count represented groups", {
  object <- unit_single_cell_object()

  expect_identical(
    cellDIVER:::metadata_type(object, "cell_type"), "Categorical"
  )
  expect_identical(
    cellDIVER:::metadata_type(object, "condition"), "Categorical"
  )
  expect_identical(
    cellDIVER:::metadata_type(object, "quality_score"), "Numeric"
  )
  expect_identical(cellDIVER:::metadata_type(object, "selected"), "Logical")
  expect_equal(cellDIVER:::n_unique(object, "cell_type"), 3)
  expect_equal(
    cellDIVER:::ncol_settings(
      object, rule = "split_by", split_by = "cell_type"
    ),
    c(min = 1, max = 3, default = 3)
  )
})

test_that("metadata dispatch handles a real SingleCellExperiment and subset", {
  object <- unit_single_cell_experiment()
  expect_s4_class(object, "SingleCellExperiment")
  expect_equal(cellDIVER:::n_cells(object), 6)
  expect_equal(cellDIVER:::n_unique(object, "cell_type"), 3)
  expect_identical(
    cellDIVER:::metadata_type(object, "cell_type"), "Categorical"
  )
  expect_identical(
    cellDIVER:::metadata_type(object, "quality_score"), "Numeric"
  )
  expect_identical(cellDIVER:::metadata_type(object, "selected"), "Logical")

  selected_object <- object[, c("cell_one", "cell_two")]
  expect_identical(
    SCUBA::get_all_cells(selected_object), c("cell_one", "cell_two")
  )
  expect_equal(cellDIVER:::n_cells(selected_object), 2)
  expect_equal(cellDIVER:::n_unique(selected_object, "cell_type"), 1)
  expect_equal(
    cellDIVER:::ncol_settings(
      selected_object, rule = "split_by", split_by = "cell_type"
    ),
    c(min = 1, max = 1, default = 1)
  )
})

test_that("threshold statistics also use SingleCellExperiment metadata", {
  object <- unit_single_cell_experiment()

  expect_identical(
    cellDIVER:::threshold_stats(object, "quality_score", 1),
    list(
      n_total = 6L, n_above = 4L, percent_above = "66.67",
      n_below = 2L, percent_below = "33.33"
    )
  )
})

test_that("subset statistics distinguish marker and two-group analyses", {
  object <- unit_single_cell_object()
  marker_statistics <- cellDIVER:::subset_stats_function(
    object, mode = "dge", metadata_categories = "cell_type",
    group_by_category = "cell_type"
  )

  expect_equal(marker_statistics$n_cells, 6)
  expect_equal(marker_statistics$n_classes, 3)
  expect_identical(
    marker_statistics$n_by_class, "Type 1: 2\nType 2: 2\nType 3: 2"
  )
  expect_match(marker_statistics$mode_description, "Marker Identification")

  differential_statistics <- cellDIVER:::subset_stats_function(
    object, mode = "dge", metadata_categories = "condition",
    group_by_category = "condition"
  )
  expect_equal(differential_statistics$n_classes, 2)
  expect_match(
    differential_statistics$mode_description, "Differential Expression"
  )
  expect_identical(
    differential_statistics$n_by_class,
    "control: 3\ntreated: 3"
  )
})

test_that("correlation statistics count cells with nonzero expression", {
  object <- unit_single_cell_object()
  statistics <- cellDIVER:::subset_stats_function(
    object, mode = "corr", metadata_categories = "cell_type",
    gene_selected = "GeneAlpha", nonzero_threshold = 10
  )

  expect_equal(statistics$n_cells, 6)
  expect_equal(statistics$n_nonzero, 5)
  expect_equal(statistics$prop_nonzero, 5 / 6)
  expect_identical(statistics$percent_nonzero, "83.33")
})
