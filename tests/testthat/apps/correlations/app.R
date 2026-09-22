library(cellDIVER)
library(shiny)
library(shinyWidgets)
library(shinyjs)
library(shinyBS)
library(waiter)
library(dplyr)
library(stringr)
library(ggplot2)
library(glue)
library(DT)
library(rlog)
library(rlang)

# The main browser intentionally no longer mounts the correlations tab.
# Exercise its retained production module without enabling it in the product.
object <- readRDS(system.file(
  "extdata", "test_dataset.rds", package = "cellDIVER", mustWork = TRUE
))
config <- cellDIVER:::load_config(system.file(
  "extdata", "test_dataset_config.yaml", package = "cellDIVER", mustWork = TRUE
))
metadata <- SCUBA::fetch_metadata(object, full_table = TRUE)
metadata_config <- reactive(label = "fixture metadata config", config$metadata)
assay_config <- reactive(label = "fixture assay config", config$assays)
meta_categories <- reactive(
  label = "fixture metadata categories", names(config$metadata)
)
unique_metadata <- reactive(
  label = "fixture metadata values",
  lapply(metadata[names(config$metadata)], unique)
)
meta_choices <- reactive(
  label = "fixture metadata choices",
  c("Cell Type" = "condensed_cell_type", "Batch" = "Batch", "None" = "none")
)
valid_features <- reactive(
  label = "fixture feature choices",
  list(Gene = paste0(
    "rna_", SCUBA::features_in_assay(object, assay = "RNA")
  ))
)

ui <- fluidPage(
  useShinyjs(),
  useWaiter(),
  includeScript(system.file(
    "js", "collapsible_panel.js", package = "cellDIVER", mustWork = TRUE
  )),
  isolate(cellDIVER:::corr_tab_ui(
    id = "object_corr",
    unique_metadata = unique_metadata,
    metadata_config = metadata_config,
    auto_dictionary_path = "",
    string_subsetting_href = "#"
  ))
)

#' Mount the production correlation module on fixed example data
#'
#' @param input Shiny inputs.
#' @param output Shiny outputs.
#' @param session Shiny session.
#' @return No return value; installs the production module.
server <- function(input, output, session) {
  session$userData$dev_mode <- FALSE
  cellDIVER:::corr_tab_server(
    id = "object_corr",
    object = reactive(label = "fixture object", object),
    metadata_config = metadata_config,
    assay_config = assay_config,
    designated_genes_assay = reactive(label = "fixture genes assay", "RNA"),
    meta_categories = meta_categories,
    unique_metadata = unique_metadata,
    n_cells_original = reactive(
      label = "fixture cell count", length(SCUBA::get_all_cells(object))
    ),
    nonzero_threshold = 0.01,
    meta_choices = meta_choices,
    valid_features = valid_features,
    error_list = list(subset_errors = list()),
    update_features = cellDIVER:::makeReactiveTrigger(),
    object_trigger = cellDIVER:::makeReactiveTrigger()
  )
}

shinyApp(ui, server)
