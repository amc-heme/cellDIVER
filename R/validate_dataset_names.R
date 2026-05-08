#' Validate Dataset Names for Browser Config
#'
#' Checks that dataset names in browser config files do not contain whitespace
#' characters (spaces, tabs, newlines, etc.) that would break Shiny module IDs.
#' Dataset names are used to construct Shiny module identifiers, so they must
#' not contain any whitespace characters.
#'
#' @param datasets A named list of datasets from the browser config file. The
#'   names of this list are the dataset identifiers.
#'
#' @return NULL invisibly if validation passes. Throws an error with a helpful
#'   message if validation fails.
#'
#' @details
#' This function validates that dataset names do not contain whitespace
#' characters (spaces, tabs, newlines, etc.) as they are used to create Shiny
#' module IDs.
#'
#' The function is called during app initialization when loading multi-object
#' configurations from browser config files.
#'
#' @noRd
validate_dataset_names <- function(datasets) {
  dataset_names <- names(datasets)
  
  # Check if any dataset names contain whitespace characters (spaces, tabs,
  # newlines, etc.)
  names_with_whitespace <- dataset_names[grepl("\\s", dataset_names)]
  
  if (length(names_with_whitespace) > 0) {
    # Create a clear error message
    error_message <- paste0(
      "Invalid dataset names found in browser config file.\n\n",
      "The following dataset name(s) contain whitespace characters:\n",
      paste0("  - '", names_with_whitespace, "'", collapse = "\n"),
      "\n\n",
      "Dataset names are used to create Shiny module identifiers and cannot ",
      "contain whitespace (spaces, tabs, newlines, etc.).\n\n",
      "To fix this issue:\n",
      "1. Edit your browser config YAML file\n",
      "2. Replace whitespace in dataset names with underscores or remove it\n",
      "   For example: 'Seurat object' -> 'Seurat_object'\n\n",
      "Example of a valid browser config file:\n",
      "---\n",
      "datasets:\n",
      "    Seurat_object:\n",
      "        object: \"path/to/seurat.rds\"\n",
      "        config: \"path/to/config.yaml\"\n",
      "    SCE_object:\n",
      "        object: \"path/to/sce/\"\n",
      "        config: \"path/to/sce_config.yaml\"\n\n",
      "For more information on creating browser config files, see:\n",
      "https://amc-heme.github.io/scExploreR/articles/dataset_setup_walkthrough.html"
    )
    
    stop(error_message, call. = FALSE)
  }
  
  invisible(NULL)
}
