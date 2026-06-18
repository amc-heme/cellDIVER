library(shinytest)
library(shiny)

# Path for running tests
app <- ShinyDriver$new("./apps/cellDIVER", seed = 325)

# Use this path when editing tests
# app <- ShinyDriver$new("tests/testthat/apps/cellDIVER", seed = 325)

test_that("DGE: Marker ID Mode Works", {
  # Swtich to DGE tab: find button in navbar panel, and click the button 
  dge_tab_btn <- app$findElement(linkText = "Differential Expression")
  dge_tab_btn$click()
  
  # Must wait for the DGE tab submit button to appear 
  app$waitForShiny()
  
  # No adjustments necessary to run marker identification; just click submit
  app$click("object_dge-submit")
  # Wait for table to compute after pressing submit
  app$waitForShiny()
  
  # Fetch DGE table and verify it exists
  dge_table <- app$getAllValues()$export$`object_dge-dge_table`
  expect_true(!is.null(dge_table))
  
  # Verify that numeric values for the table match previously-computed values 
  # for the object and test
  expect_equal(
    object = colSums(dge_table[, 3:8]),
    expected = 
      c(2763.29852416002, 
        1505.23168019154, 
        975.060444444444, 
        426.766090186197, 
        83508, 
        53266.6666666667),
    tolerance = 1e-6,
    ignore_attr = TRUE
  )
})

test_that("DGE: Differential Expression Mode Works", {
  app$setValue(
    name = "object_dge-test_selections-mode", 
    value = "mode_dge"
    )
  
  app$setValue(
    name = "object_dge-test_selections-group_by",
    value = "condensed_cell_type"
  )
  
  app$setValue(
    name = "object_dge-test_selections-group_1",
    value = "Primitive"
  )
  
  # Must wait for selections for group 1 to register before setting group 2
  # (table will not appear if this is not added)
  app$waitForShiny()
  
  app$setValue(
    name = "object_dge-test_selections-group_2",
    value = c("BM Monocytes", "PBMC Monocytes")
  )
  
  app$waitForShiny()
  
  # Press submit and wait for table to compute
  app$click("object_dge-submit")
  app$waitForShiny()
  
  
  # Fetch DGE table and verify it exists
  dge_table <- app$getAllValues()$export$`object_dge-dge_table`
  expect_true(!is.null(dge_table))
  
  # Check that the numeric values of the table match previously-computed values 
  # for this object and test
  expect_equal(
    object = colSums(dge_table[, 3:8]),
    expected = 
      c(668.001327940329, 
        502.33400527721, 
        295.2868, 
        53.6666287556446, 
        21982, 
        11692),
    tolerance = 1e-6,
    ignore_attr = TRUE
  )
})

test_that("Threshold-Based DGE Works", {
  # Because the simple threshold DGE is a point-and-click widget without an 
  # input binding, it can't be set programmatically. To design a test, one of 
  # the options below must be completed:
  # 1. Create a text input for manually entering a threshold (useful to the user
  # anyway)
  # 2. Turn the widget into a custom input binding, which would probably require
  # re-creating the plot using JavaScript libraries (this is more difficult but
  # would get around the clunkiness of ggridges)
  
  
  # app$setValue(
  #   name = "object_dge-test_selections-use_feature_expression",
  #   value = TRUE
  #   )
  # 
  # app$waitForShiny()
  # 
  # app$setValue(
  #   name = "object_dge-test_selections-simple_threshold_feature",
  #   value = "ab_CD34-AB"
  #   )
  
  
  
  })

test_that("Marker Identification With a Subset Works", {
  # Set mode back to marker identification
  app$setValue(
    name = "object_dge-test_selections-mode", 
    value = "mode_marker"
  )
  
  app$waitForShiny()
  
  # Options for group_by and marker_group_selection should not have changed,
  expect_equal(
    app$getValue("object_dge-test_selections-group_by"), 
    "condensed_cell_type"
    )
  expect_equal(
    app$getValue(
      "object_dge-test_selections-marker_group_selection",
      ),
    list(
      "B Cells", 
      "BM Monocytes", 
      "CD4+ T Cells", 
      "CD8+ T Cells", 
      "Dendritic cells", 
      "NK Cells", 
      "PBMC Monocytes", 
      "Plasma cells", 
      "Plasmacytoid dendritic cells", 
      "Primitive"
      )
    )
  
  # Select a subset of cells
  app$setValue("object_dge-subset_selections-Batch_selection", "BM_200AB")
  app$waitForShiny()
  
  # Run DGE
  app$click("object_dge-submit")
  app$waitForShiny()
  
  # Fetch DGE table and verify it exists
  dge_table <- app$getAllValues()$export$`object_dge-dge_table`
  expect_true(!is.null(dge_table))
  
  # Check values on table 
  expect_equal(
    object = colSums(dge_table[, 3:8]),
    expected =
      c(2528.8364, 
        1397.2407, 
        866.0678, 
        419.5748, 
        76499.3792, 
        48360.2609),
    tolerance = 1e-6,
    ignore_attr = TRUE
  )
  
  # Reset subset
  app$click("object_dge-subset_selections-reset_filter")
  app$waitForShiny()
})