# End-to-end tests using shinytest2
#
# These tests verify core functionality of the scExploreR app
# using the test dataset (250 cells, RNA + AB assays, UMAP).
#
# The test dataset has:
#   - Assays: RNA (462 genes), AB (197 surface proteins)
#   - Metadata: condensed_cell_type, Batch
#   - Reductions: pca, umap
#   - Cell types: B Cells, BM Monocytes, CD4+ T Cells,
#     CD8+ T Cells, Dendritic cells, NK Cells,
#     PBMC Monocytes, Plasma cells,
#     Plasmacytoid dendritic cells, Primitive
#   - Batches: BM_200AB, Pheresis_200AB
#
# Namespace convention for single-object deployment:
#   Plots tab: "object_plots-{input_id}"
#   DGE tab:   "object_dge-{input_id}"
#   Plot modules: "object_plots-{plot_id}-{input_id}"

test_that("DimPlot is visible on app startup", {
  skip_if_not_installed("shinytest2")
  skip_on_cran()

  app_dir <- test_path("apps", "scExploreR")
  if (!dir.exists(app_dir)) {
    skip("Test app directory not found")
  }

  app <- shinytest2::AppDriver$new(
    app_dir = app_dir,
    name = "e2e-dimplot-visible",
    timeout = 60000,
    width = 1200,
    height = 900
  )
  withr::defer(app$stop())

  # Navigate to the Plots tab
  app$click(
    selector = "a[data-value='plots']"
  )
  app$wait_for_idle(timeout = 10000)

  # DimPlot switch should be TRUE by default
  values <- app$get_values()
  expect_true(
    values$input$`object_plots-make_dimplot`
  )

  # The DimPlot plot output UI should be rendered
  # (the uiOutput for the dimplot module should exist)
  dimplot_output <- app$get_value(
    output = "object_plots-dimplot-plot_output_ui"
  )
  expect_true(!is.null(dimplot_output))
})

test_that("toggling Feature Plot switch makes it appear", {
  skip_if_not_installed("shinytest2")
  skip_on_cran()

  app_dir <- test_path("apps", "scExploreR")
  if (!dir.exists(app_dir)) {
    skip("Test app directory not found")
  }

  app <- shinytest2::AppDriver$new(
    app_dir = app_dir,
    name = "e2e-feature-toggle",
    timeout = 60000,
    width = 1200,
    height = 900
  )
  withr::defer(app$stop())

  # Navigate to Plots tab
  app$click(
    selector = "a[data-value='plots']"
  )
  app$wait_for_idle(timeout = 10000)

  # Feature Plot should be OFF by default
  values <- app$get_values()
  expect_false(
    values$input$`object_plots-make_feature`
  )

  # Enable Feature Plot
  app$set_inputs(
    `object_plots-make_feature` = TRUE
  )
  app$wait_for_idle(timeout = 5000)

  # Enter a feature (RNA gene) to plot
  app$set_inputs(
    `object_plots-text_features` = "rna_CD34"
  )
  app$wait_for_idle(timeout = 10000)

  # Verify Feature Plot output UI is rendered
  feature_output <- app$get_value(
    output = "object_plots-feature-plot_output_ui"
  )
  expect_true(!is.null(feature_output))
})

test_that("toggling Violin Plot switch makes it appear", {
  skip_if_not_installed("shinytest2")
  skip_on_cran()

  app_dir <- test_path("apps", "scExploreR")
  if (!dir.exists(app_dir)) {
    skip("Test app directory not found")
  }

  app <- shinytest2::AppDriver$new(
    app_dir = app_dir,
    name = "e2e-violin-toggle",
    timeout = 60000,
    width = 1200,
    height = 900
  )
  withr::defer(app$stop())

  # Navigate to Plots tab
  app$click(
    selector = "a[data-value='plots']"
  )
  app$wait_for_idle(timeout = 10000)

  # Violin should be OFF by default
  values <- app$get_values()
  expect_false(
    values$input$`object_plots-make_vln`
  )

  # Enable violin plot
  app$set_inputs(
    `object_plots-make_vln` = TRUE
  )
  app$wait_for_idle(timeout = 5000)

  # Enter a feature for the violin plot
  app$set_inputs(
    `object_plots-text_features` = "rna_CD34"
  )
  app$wait_for_idle(timeout = 10000)

  # Verify violin plot output is rendered
  violin_output <- app$get_value(
    output = "object_plots-violin-plot_output_ui"
  )
  expect_true(!is.null(violin_output))
})

test_that("toggling Dot Plot switch makes it appear", {
  skip_if_not_installed("shinytest2")
  skip_on_cran()

  app_dir <- test_path("apps", "scExploreR")
  if (!dir.exists(app_dir)) {
    skip("Test app directory not found")
  }

  app <- shinytest2::AppDriver$new(
    app_dir = app_dir,
    name = "e2e-dot-toggle",
    timeout = 60000,
    width = 1200,
    height = 900
  )
  withr::defer(app$stop())

  # Navigate to Plots tab
  app$click(
    selector = "a[data-value='plots']"
  )
  app$wait_for_idle(timeout = 10000)

  # Dot plot should be OFF by default
  values <- app$get_values()
  expect_false(
    values$input$`object_plots-make_dot`
  )

  # Enable dot plot
  app$set_inputs(
    `object_plots-make_dot` = TRUE
  )
  app$wait_for_idle(timeout = 5000)

  # Enter a feature for the dot plot
  app$set_inputs(
    `object_plots-text_features` = "rna_CD34"
  )
  app$wait_for_idle(timeout = 10000)

  # Verify dot plot output is rendered
  dot_output <- app$get_value(
    output = "object_plots-dot-plot_output_ui"
  )
  expect_true(!is.null(dot_output))
})

test_that("toggling Ridge Plot switch makes it appear", {
  skip_if_not_installed("shinytest2")
  skip_on_cran()

  app_dir <- test_path("apps", "scExploreR")
  if (!dir.exists(app_dir)) {
    skip("Test app directory not found")
  }

  app <- shinytest2::AppDriver$new(
    app_dir = app_dir,
    name = "e2e-ridge-toggle",
    timeout = 60000,
    width = 1200,
    height = 900
  )
  withr::defer(app$stop())

  # Navigate to Plots tab
  app$click(
    selector = "a[data-value='plots']"
  )
  app$wait_for_idle(timeout = 10000)

  # Ridge plot should be OFF by default
  values <- app$get_values()
  expect_false(
    values$input$`object_plots-make_ridge`
  )

  # Enable ridge plot
  app$set_inputs(
    `object_plots-make_ridge` = TRUE
  )
  app$wait_for_idle(timeout = 5000)

  # Enter a feature for the ridge plot
  app$set_inputs(
    `object_plots-text_features` = "rna_CD34"
  )
  app$wait_for_idle(timeout = 10000)

  # Verify ridge plot output is rendered
  ridge_output <- app$get_value(
    output = "object_plots-ridge-plot_output_ui"
  )
  expect_true(!is.null(ridge_output))
})

test_that(
  "toggling Scatterplot switch makes it appear",
  {
    skip_if_not_installed("shinytest2")
    skip_on_cran()

    app_dir <- test_path("apps", "scExploreR")
    if (!dir.exists(app_dir)) {
      skip("Test app directory not found")
    }

    app <- shinytest2::AppDriver$new(
      app_dir = app_dir,
      name = "e2e-scatter-toggle",
      timeout = 60000,
      width = 1200,
      height = 900
    )
    withr::defer(app$stop())

    # Navigate to Plots tab
    app$click(
      selector = "a[data-value='plots']"
    )
    app$wait_for_idle(timeout = 10000)

    # Scatter should be OFF by default
    values <- app$get_values()
    expect_false(
      values$input$`object_plots-make_scatter`
    )

    # Enable scatter plot
    app$set_inputs(
      `object_plots-make_scatter` = TRUE
    )
    app$wait_for_idle(timeout = 5000)

    # Scatterplots require exactly two features
    app$set_inputs(
      `object_plots-text_features` = c(
        "rna_CD34", "rna_CD38"
      )
    )
    app$wait_for_idle(timeout = 10000)

    # Verify scatter plot output is rendered
    scatter_output <- app$get_value(
      output = "object_plots-scatter-plot_output_ui"
    )
    expect_true(!is.null(scatter_output))
  }
)

test_that(
  "toggling Cell Proportion switch makes it appear",
  {
    skip_if_not_installed("shinytest2")
    skip_on_cran()

    app_dir <- test_path("apps", "scExploreR")
    if (!dir.exists(app_dir)) {
      skip("Test app directory not found")
    }

    app <- shinytest2::AppDriver$new(
      app_dir = app_dir,
      name = "e2e-proportion-toggle",
      timeout = 60000,
      width = 1200,
      height = 900
    )
    withr::defer(app$stop())

    # Navigate to Plots tab
    app$click(
      selector = "a[data-value='plots']"
    )
    app$wait_for_idle(timeout = 10000)

    # Proportion switch should be OFF by default
    values <- app$get_values()
    expect_false(
      values$input$`object_plots-make_proportion`
    )

    # Enable cell proportion plot
    app$set_inputs(
      `object_plots-make_proportion` = TRUE
    )
    app$wait_for_idle(timeout = 10000)

    # Verify proportion plot output is rendered
    proportion_output <- app$get_value(
      output =
        "object_plots-proportion-plot_output_ui"
    )
    expect_true(!is.null(proportion_output))
  }
)
