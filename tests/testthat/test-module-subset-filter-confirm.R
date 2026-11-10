# The subset filter dialog must not save a categorical filter without values:
# the confirm button is disabled client-side while the selection is empty, but
# a late refresh of the value choices can empty it after the button was
# enabled, and a filter with no values matches no cells.

subset_module_args <- function(object) {
  configuration <- list(
    condition = list(meta_colname = "condition", label = "Condition", groups = NULL),
    cell_type = list(meta_colname = "cell_type", label = "Cell Type", groups = NULL)
  )
  unique_metadata <- lapply(configuration, function(entry) {
    as.character(SCUBA::unique_values(object, entry$meta_colname))
  })
  list(
    object = shiny::reactive(object),
    unique_metadata = shiny::reactive(unique_metadata),
    metadata_config = shiny::reactive(configuration),
    assay_config = shiny::reactive(list(
      RNA = list(assay = "RNA", key = "rna_", suffix_human = "", dropdown_title = "Gene")
    )),
    meta_categories = shiny::reactive(names(configuration)),
    valid_features = shiny::reactive(rownames(object))
  )
}

test_that("a categorical filter is only saved once at least one value is chosen", {
  object <- unit_pseudobulk_object()
  session <- shiny::MockShinySession$new()
  session$userData$dev_mode <- FALSE
  shiny::testServer(
    cellDIVER:::subset_selections_server,
    args = subset_module_args(object),
    session = session,
    {
      session$setInputs(add_filter = 1)
      session$setInputs(filter_type = "categorical")
      session$setInputs(categorical_var = "condition")
      # confirm with no values: nothing is saved and the dialog stays open
      session$setInputs(filter_confirm = 1)
      expect_length(session$getReturned()$selections(), 0)
      expect_identical(module_data$filter_menu_state, "add")
      expect_identical(module_data$filter_type, "categorical")
      # with a value chosen the filter is saved as before
      session$setInputs(categorical_values = "treated")
      session$setInputs(filter_confirm = 2)
      filters <- session$getReturned()$selections()
      expect_length(filters, 1)
      expect_identical(filters[[1]]$var, "condition")
      expect_identical(filters[[1]]$value, "treated")
      expect_identical(module_data$filter_menu_state, "idle")
    }
  )
})
