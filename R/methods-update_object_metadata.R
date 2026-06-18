#' Metadata for multiple single cell object classes
#'
#' Updates the metadata of either a Seurat or SingleCellExperiment object. This 
#' method is intended to be used with fetch_metadata to pull a table from an 
#' object, modify it, and then store the updated table. 
#'
#' @param object a single-cell object. Currently, Seurat and
#' SingleCellExperiment objects are supported.
#' @param table a modified metadata table.
#' @param mod For MuData objects, the modality for which to update metadata
#' can be specified. If NULL (the default), the obs table of the main object 
#' will be updated. As of v1.0.0, cellDIVER only operates on the main obs 
#' table. In the future, it may make sense to loop through each modality and 
#' update the obs table individually there.
#'
#' @return the object passed to \code{object} with the modified metadata table.
#' @noRd
update_object_metadata <-
  function(
    object,
    table,
    ...
  ){
    UseMethod("update_object_metadata")
  }

#' Function to display an error message when an unsupported object
#' type is detected
#'
#' @export
#' @noRd
update_object_metadata.default <-
  function(
    object,
    table
  ){
    warning(
      paste0(
        "update_object_metadata does not know how to handle object of class ",
        paste(class(object), collapse = ", "),
        ". Currently supported classes: Seurat and SingleCellExperiment."
      )
    )
  }

#' @describeIn update_object_metadata Seurat objects
#' @export
#' @noRd
update_object_metadata.Seurat <-
  function(
    object,
    table
  ){
    object@meta.data <- table
    
    object
  }

#' @describeIn update_object_metadata SingleCellExperiment objects
#' @export
#' @noRd
update_object_metadata.SingleCellExperiment <-
  function(
    object,
    table
  ){
    colData(object) <- table
    
    object
  }

#' @describeIn update_object_metadata Anndata objects
#' @export
#' @noRd
update_object_metadata.AnnDataR6 <-
  function(
    object,
    table
  ){
    object$obs <- table
    
    object
  }

#' @describeIn update_object_metadata Anndata objects
#' @export
#' @noRd
update_object_metadata.md._core.mudata.MuData <-
  function(
    object,
    table,
    mod = NULL
  ){
    if (is.null(mod)){
      # Modality is NULL: update the obs table of the main object with the table
      object$obs <- table
    } else {
      # Otherwise, update the obs table of the specified modality
      object[[mod]]$obs <- table
    }
    
    object
  }
