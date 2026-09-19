test_that("browser marker identification retains numeric DGE regression", {
  app <- browser_app("dge-markers")
  browser_open_tab(app, "dge", "object_dge-test_selections-group_by")
  expect_equal(
    app$get_value(input = "object_dge-test_selections-group_by"),
    "condensed_cell_type"
  )
  browser_click(app, "object_dge-submit")
  browser_expect_dge(app, browser_object(), "condensed_cell_type")
})

test_that("browser pairwise DGE compares independently selected metaclusters", {
  app <- browser_app("dge-pairwise")
  browser_open_tab(app, "dge", "object_dge-test_selections-group_by")
  browser_set(app, "object_dge-test_selections-mode", "mode_dge")
  browser_set(app, "object_dge-test_selections-group_by", "condensed_cell_type")
  browser_set(app, "object_dge-test_selections-group_1", "Primitive")
  browser_set(
    app, "object_dge-test_selections-group_2",
    c("BM Monocytes", "PBMC Monocytes")
  )

  object <- browser_object()
  metadata <- SCUBA::fetch_metadata(object, full_table = TRUE)
  cells <- rownames(metadata)[metadata$condensed_cell_type %in% c(
    "Primitive", "BM Monocytes", "PBMC Monocytes"
  )]
  object <- object[, cells]
  object$metacluster <- ifelse(
    metadata[cells, "condensed_cell_type"] == "Primitive",
    "Primitive", "BM Monocytes and PBMC Monocytes"
  )
  browser_click(app, "object_dge-submit")
  result <- browser_expect_dge(app, object, "metacluster")
  expect_setequal(
    unique(result$group), c("Primitive", "BM Monocytes and PBMC Monocytes")
  )
})

test_that("browser pairwise DGE compares the two recorded sample batches", {
  app <- browser_app("dge-pairwise-batches")
  browser_open_tab(app, "dge", "object_dge-test_selections-group_by")
  browser_set(app, "object_dge-test_selections-mode", "mode_dge")
  browser_set(app, "object_dge-test_selections-group_by", "Batch")
  browser_set(app, "object_dge-test_selections-group_1", "BM_200AB")
  browser_set(app, "object_dge-test_selections-group_2", "PBMC_200AB")
  browser_click(app, "object_dge-submit")
  result <- browser_expect_dge(app, browser_object(), "Batch")
  expect_setequal(unique(result$group), c("BM_200AB", "PBMC_200AB"))
})

test_that("browser marker filters affect DGE and reset restores all cells", {
  app <- browser_app("dge-subset-reset")
  browser_open_tab(app, "dge", "object_dge-test_selections-group_by")
  browser_filter(
    app, "object_dge-subset_selections", "Batch", "BM_200AB"
  )
  object <- browser_object()
  metadata <- SCUBA::fetch_metadata(object, full_table = TRUE)
  cells <- rownames(metadata)[metadata$Batch == "BM_200AB"]
  expect_lt(length(cells), nrow(metadata))
  browser_click(app, "object_dge-submit")
  subset_result <- browser_expect_dge(
    app, object[, cells], "condensed_cell_type"
  )

  browser_click(app, "object_dge-subset_selections-reset_all_filters")
  browser_click(app, "object_dge-submit")
  browser_expect_dge(
    app, object, "condensed_cell_type", previous = subset_result
  )
})

test_that("a real threshold plot click partitions cells and computes DGE", {
  app <- browser_app("dge-expression-threshold")
  browser_open_tab(app, "dge", "object_dge-test_selections-group_by")
  browser_set(app, "object_dge-test_selections-mode", "mode_dge")
  browser_set(app, "object_dge-test_selections-use_feature_expression", TRUE)
  browser_set(
    app, "object_dge-test_selections-simple_threshold_feature", "ab_CD34-AB"
  )
  namespace <- "object_dge-test_selections-simple_threshold"
  browser_plot(app, paste0(namespace, "-ridge_plot"))
  browser_click_plot(app, paste0(namespace, "-ridge_plot"))
  threshold <- as.numeric(app$wait_for_value(
    output = paste0(namespace, "-chosen_threshold")
  ))
  object <- browser_object()
  expression <- SCUBA::fetch_data(object, vars = "ab_CD34-AB")[[1]]
  expect_true(is.finite(threshold))
  click <- app$get_value(input = paste0(namespace, "-plot_click"))
  lower_limit <- as.numeric(app$get_value(
    input = paste0(namespace, "-lower_xlim")
  ))
  upper_limit <- as.numeric(app$get_value(
    input = paste0(namespace, "-upper_xlim")
  ))
  expect_equal(
    threshold,
    round(
      (click$x - 0.06) * (upper_limit - lower_limit) / 0.9 + min(expression),
      2
    )
  )
  expect_gt(sum(expression >= threshold), 1)
  expect_gt(sum(expression < threshold), 1)
  expect_equal(
    as.integer(sub(" .*", "", app$get_value(
      output = paste0(namespace, "-above_stats")
    ))),
    sum(expression >= threshold)
  )
  expect_equal(
    as.integer(sub(" .*", "", app$get_value(
      output = paste0(namespace, "-below_stats")
    ))),
    sum(expression < threshold)
  )
  object$simple_expr_threshold <- ifelse(
    expression >= threshold, "CD34-AB High", "CD34-AB Low"
  )
  browser_click(app, "object_dge-submit")
  result <- browser_expect_dge(app, object, "simple_expr_threshold")
  expect_setequal(unique(result$group), c("CD34-AB High", "CD34-AB Low"))
})
