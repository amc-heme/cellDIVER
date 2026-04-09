# End-to-end tests for the DGE tab using shinytest2
#
# Tests the Differential Gene Expression functionality
# using the test dataset (250 cells, RNA + AB assays).
#
# DGE module namespace: "object_dge"
# Test selection sub-module: "object_dge-test_selections"
# Subset selection sub-module: "object_dge-subset_selections"

test_that("DGE: Marker ID mode produces a table", {
  skip_if_not_installed("shinytest2")
  skip_on_cran()

  app_dir <- test_path("apps", "scExploreR")
  if (!dir.exists(app_dir)) {
    skip("Test app directory not found")
  }

  app <- shinytest2::AppDriver$new(
    app_dir = app_dir,
    name = "e2e-dge-marker-id",
    timeout = 60000,
    width = 1200,
    height = 900
  )
  withr::defer(app$stop())

  # Navigate to the DGE tab
  app$click(
    selector = "a[data-value='dge']"
  )
  app$wait_for_idle(timeout = 10000)

  # In Marker ID mode (the default), click submit without
  # changing any settings
  app$click(
    input = "object_dge-submit"
  )
  app$wait_for_idle(timeout = 30000)

  # Verify the DGE table is produced
  dge_table <- app$get_value(
    export = "object_dge-dge_table"
  )
  expect_true(!is.null(dge_table))
  expect_true(is.data.frame(dge_table))
  expect_true(nrow(dge_table) > 0)
})

test_that(
  "DGE: Differential Expression mode produces a table",
  {
    skip_if_not_installed("shinytest2")
    skip_on_cran()

    app_dir <- test_path("apps", "scExploreR")
    if (!dir.exists(app_dir)) {
      skip("Test app directory not found")
    }

    app <- shinytest2::AppDriver$new(
      app_dir = app_dir,
      name = "e2e-dge-de-mode",
      timeout = 60000,
      width = 1200,
      height = 900
    )
    withr::defer(app$stop())

    # Navigate to the DGE tab
    app$click(
      selector = "a[data-value='dge']"
    )
    app$wait_for_idle(timeout = 10000)

    # Switch to Differential Expression mode
    app$set_inputs(
      `object_dge-test_selections-mode` = "mode_dge"
    )
    app$wait_for_idle(timeout = 5000)

    # Set group_by to condensed_cell_type
    app$set_inputs(
      `object_dge-test_selections-group_by` =
        "condensed_cell_type"
    )
    app$wait_for_idle(timeout = 5000)

    # Set group 1 to Primitive
    app$set_inputs(
      `object_dge-test_selections-group_1` = "Primitive"
    )
    app$wait_for_idle(timeout = 5000)

    # Set group 2 to BM Monocytes and PBMC Monocytes
    app$set_inputs(
      `object_dge-test_selections-group_2` =
        c("BM Monocytes", "PBMC Monocytes")
    )
    app$wait_for_idle(timeout = 5000)

    # Run the analysis
    app$click(
      input = "object_dge-submit"
    )
    app$wait_for_idle(timeout = 30000)

    # Verify the DGE table is produced
    dge_table <- app$get_value(
      export = "object_dge-dge_table"
    )
    expect_true(!is.null(dge_table))
    expect_true(is.data.frame(dge_table))
    expect_true(nrow(dge_table) > 0)
  }
)

test_that(
  "DGE: Marker ID with subset produces a table",
  {
    skip_if_not_installed("shinytest2")
    skip_on_cran()

    app_dir <- test_path("apps", "scExploreR")
    if (!dir.exists(app_dir)) {
      skip("Test app directory not found")
    }

    app <- shinytest2::AppDriver$new(
      app_dir = app_dir,
      name = "e2e-dge-subset",
      timeout = 60000,
      width = 1200,
      height = 900
    )
    withr::defer(app$stop())

    # Navigate to the DGE tab
    app$click(
      selector = "a[data-value='dge']"
    )
    app$wait_for_idle(timeout = 10000)

    # Set a subset filter (Batch = BM_200AB)
    app$set_inputs(
      `object_dge-subset_selections-Batch_selection` =
        "BM_200AB"
    )
    app$wait_for_idle(timeout = 5000)

    # Run marker identification with subset
    app$click(
      input = "object_dge-submit"
    )
    app$wait_for_idle(timeout = 30000)

    # Verify the DGE table is produced
    dge_table <- app$get_value(
      export = "object_dge-dge_table"
    )
    expect_true(!is.null(dge_table))
    expect_true(is.data.frame(dge_table))
    expect_true(nrow(dge_table) > 0)

    # Reset the subset filter
    app$click(
      input =
        "object_dge-subset_selections-reset_filter"
    )
    app$wait_for_idle(timeout = 5000)
  }
)
