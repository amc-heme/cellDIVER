test_that("subset summaries react to selected cells and an absent subset", {
  original_object <- unit_single_cell_object()
  object_reactive <- shiny::reactiveVal(original_object)
  category_labels <- shiny::reactiveVal(c(
    cell_type = "Cell type", condition = "Condition"
  ))
  unique_metadata <- shiny::reactiveVal(list(
    cell_type = c("Type 1", "Type 2", "Type 3"),
    condition = c("control", "treated")
  ))

  shiny::testServer(
    cellDIVER:::subset_summary_server,
    args = list(
      object = object_reactive,
      category_labels = category_labels,
      unique_metadata = unique_metadata
    ),
    {
      session$flushReact()
      expect_identical(output$selected_cell_type, "All")
      expect_identical(output$selected_condition, "All")

      object_reactive(original_object[, c("cell_one", "cell_three")])
      session$flushReact()
      expect_identical(output$selected_cell_type, "Type 1 and Type 2")
      expect_identical(output$selected_condition, "control")

      object_reactive(NULL)
      session$flushReact()
      expect_error(output$selected_cell_type, "no cells in subset")

      object_reactive(original_object)
      session$flushReact()
      expect_identical(output$selected_cell_type, "All")
    }
  )
})

test_that("feature summaries react to selected features and subset cells", {
  original_object <- unit_single_cell_object()
  object_reactive <- shiny::reactiveVal(original_object)
  selected_features <- shiny::reactiveVal(character())
  assay_configuration <- shiny::reactiveVal(list(
    RNA = list(key = "rna_", suffix_human = "")
  ))

  shiny::testServer(
    cellDIVER:::feature_stats_server,
    args = list(
      object = object_reactive,
      features_entered = selected_features,
      assay_config = assay_configuration
    ),
    {
      session$flushReact()
      expect_match(
        output$feature_statistics$html,
        "Please enter a feature to view statistics",
        fixed = TRUE
      )

      selected_features(c("rna_GeneAlpha", "rna_GeneBeta"))
      session$flushReact()
      expect_named(summary_by_feature(), c("rna_GeneAlpha", "rna_GeneBeta"))
      expect_equal(
        summary_by_feature()$rna_GeneAlpha, summary(c(0, 1, 2, 2, 4, 5))
      )
      expect_equal(
        summary_by_feature()$rna_GeneBeta, summary(c(6, 5, 4, 3, 2, 1))
      )
      expect_match(output$feature_statistics$html, "GeneAlpha", fixed = TRUE)

      object_reactive(original_object[, c("cell_one", "cell_two")])
      session$flushReact()
      expect_equal(summary_by_feature()$rna_GeneAlpha, summary(c(0, 1)))

      selected_features("rna_GeneBeta")
      session$flushReact()
      expect_named(summary_by_feature(), "rna_GeneBeta")
      expect_equal(summary_by_feature()$rna_GeneBeta, summary(c(6, 5)))
    }
  )
})
