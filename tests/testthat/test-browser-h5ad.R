# The AML test dataset as an .h5ad file served by SCUBA's Julia engine (fixture app
# tests/testthat/apps/cellDIVER-h5ad). Mirrors the core DimPlot / FeaturePlot scenarios.
skip_if_not_installed("SCUBA")
skip_if_not(exists("open_h5ad", asNamespace("SCUBA")), "SCUBA without open_h5ad()")
skip_if_not(isTRUE(tryCatch(SCUBA::scuba_julia_available(quiet = TRUE), error = function(e) FALSE)), "Julia engine not available")

h5ad_browser_object <- function() {
  withr::local_options(list(SCUBA.engine = "julia"))
  SCUBA::open_h5ad(system.file("extdata", "AML_h5ad.h5ad", package = "SCUBA", mustWork = TRUE))
}

test_that("h5ad dataset: DimPlot renders, groups and splits on the Julia engine", {
  app <- browser_app("h5ad-dimplot", fixture = "cellDIVER-h5ad")
  browser_open_tab(app, "plots", "object_plots-make_dimplot")
  expect_true(app$get_value(input = "object_plots-make_dimplot"))
  browser_set(app, "object_plots-dimplot-reduction", "X_umap")
  browser_set(app, "object_plots-dimplot-label", FALSE)
  browser_set(app, "object_plots-dimplot-legend", FALSE)
  browser_plot(app, "object_plots-dimplot-plot")
  original <- browser_svg(app, "object_plots-dimplot")
  object <- h5ad_browser_object()
  metadata <- SCUBA::fetch_metadata(object, full_table = TRUE)
  expect_equal(browser_point_count(original), nrow(metadata))
  browser_set(app, "object_plots-dimplot-group_by", "Batch")
  grouped <- browser_svg(app, "object_plots-dimplot")
  expect_false(identical(original, grouped))
  expect_match(grouped, "Batch")
  browser_set(app, "object_plots-dimplot-group_by", "condensed_cell_type")
  browser_set(app, "object_plots-dimplot-split_by", "Batch")
  split <- browser_svg(app, "object_plots-dimplot")
  for (batch in unique(as.character(metadata$Batch))) {
    expect_match(split, batch, fixed = TRUE)
  }
})

for (plot_type in c("feature", "violin", "dot")) {
  test_that(paste("h5ad dataset:", plot_type, "plot renders a gene and a surface protein"), {
    app <- browser_app(paste0("h5ad-plot-", plot_type), fixture = "cellDIVER-h5ad")
    browser_open_tab(app, "plots", "object_plots-make_dimplot")
    switch <- paste0("object_plots-make_", if (plot_type == "violin") "vln" else plot_type)
    browser_set(app, switch, TRUE)
    namespace <- paste0("object_plots-", plot_type)
    browser_set_feature(app, "object_plots-text_features", "X_CD34")
    browser_plot(app, paste0(namespace, "-plot"))
    svg <- browser_svg(app, namespace)
    expect_match(svg, "CD34", fixed = TRUE)
    expect_match(svg, "<(circle|polygon|polyline|path)\\b")
    browser_set_feature(app, "object_plots-text_features", "protein_CD117-AB")
    svg2 <- browser_svg(app, namespace)
    expect_match(svg2, "CD117", fixed = TRUE)
    expect_false(identical(svg, svg2))
  })
}

test_that("h5ad dataset: differential expression runs through scDE on the file-backed object", {
  skip_if_not(
    exists("run_dge.scuba_source", envir = asNamespace("scDE"), inherits = FALSE),
    "scDE without run_dge.scuba_source()"
  )
  app <- browser_app("h5ad-dge", fixture = "cellDIVER-h5ad")
  browser_open_tab(app, "dge", "object_dge-test_selections-group_by")
  expect_equal(
    app$get_value(input = "object_dge-test_selections-group_by"),
    "condensed_cell_type"
  )
  browser_click(app, "object_dge-submit")
  browser_expect_dge(app, h5ad_browser_object(), "condensed_cell_type", assay = "X")
})
