test_that("DimPlot renders, groups, splits, filters and resets actual cells", {
  app <- browser_app("plots-group-split-filter-reset")
  browser_open_tab(app, "plots", "object_plots-make_dimplot")
  expect_true(app$get_value(input = "object_plots-make_dimplot"))
  browser_set(app, "object_plots-dimplot-reduction", "umap")
  browser_set(app, "object_plots-dimplot-label", FALSE)
  browser_set(app, "object_plots-dimplot-legend", FALSE)
  browser_plot(app, "object_plots-dimplot-plot")
  original <- browser_svg(app, "object_plots-dimplot")
  object <- browser_object()
  metadata <- SCUBA::fetch_metadata(object, full_table = TRUE)
  expect_equal(browser_point_count(original), nrow(metadata))
  expect_match(original, "UMAP", ignore.case = TRUE)

  browser_set(app, "object_plots-dimplot-group_by", "Batch")
  grouped <- browser_svg(app, "object_plots-dimplot")
  expect_false(identical(original, grouped))
  expect_match(grouped, "Batch")
  expect_equal(browser_point_count(grouped), nrow(metadata))
  browser_set(app, "object_plots-dimplot-group_by", "condensed_cell_type")
  browser_set(app, "object_plots-dimplot-split_by", "Batch")
  split <- browser_svg(app, "object_plots-dimplot")
  for (batch in unique(metadata$Batch)) {
    expect_match(split, batch, fixed = TRUE)
  }
  expect_equal(browser_point_count(split), nrow(metadata))

  browser_filter(
    app, "object_plots-subset_selections", "Batch", "BM_200AB",
    panel = "object_plots-subset_collapsible"
  )
  browser_click(app, "object_plots-subset_submit")
  browser_plot(app, "object_plots-dimplot-plot")
  filtered <- browser_svg(app, "object_plots-dimplot")
  expect_equal(
    browser_point_count(filtered), sum(metadata$Batch == "BM_200AB")
  )
  browser_click(app, "object_plots-subset_selections-reset_all_filters")
  browser_click(app, "object_plots-subset_submit")
  reset <- browser_svg(app, "object_plots-dimplot")
  expect_equal(browser_point_count(reset), nrow(metadata))
})

for (plot_type in c("feature", "violin", "dot", "ridge", "scatter",
                    "proportion")) {
  test_that(paste("browser renders real", plot_type, "plot content"), {
    app <- browser_app(paste0("plot-", plot_type))
    browser_open_tab(app, "plots", "object_plots-make_dimplot")
    switch <- if (plot_type == "violin") "vln" else plot_type
    switch <- paste0("object_plots-make_", switch)
    expect_false(app$get_value(input = switch))
    browser_set(app, switch, TRUE)
    namespace <- paste0("object_plots-", plot_type)
    if (plot_type == "scatter") {
      # Scatter has its own two feature inputs; text_features is not used.
      browser_set_feature(app, paste0(namespace, "-scatter_1"), "rna_CD34")
      browser_set_feature(app, paste0(namespace, "-scatter_2"), "rna_CD38")
    } else if (plot_type == "proportion") {
      # The stacked bar plot compares one metadata category across the levels
      # of another, so it refuses to draw while both default to the same
      # column ("Proportions" and "Proportion Comparison" must differ).
      browser_set(app, paste0(namespace, "-split_by"), "Batch")
    } else {
      browser_set_feature(app, "object_plots-text_features", "rna_CD34")
    }
    browser_plot(app, paste0(namespace, "-plot"))
    svg <- browser_svg(app, namespace)
    if (plot_type == "proportion") {
      expect_match(svg, "B Cells", fixed = TRUE)
      expect_match(svg, "<rect")
    } else {
      expect_match(svg, "CD34", fixed = TRUE)
      if (plot_type == "scatter") expect_match(svg, "CD38", fixed = TRUE)
      expect_match(svg, "<(circle|polygon|polyline|path)\\b")
    }
  })
}
