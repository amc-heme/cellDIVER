#' Get names of assays/experiments in object
#'
#' Returns the names of all assays in a single-cell object. For Seurat objects,
#' returns assay names, and for SingleCellExperiment objects "experiments", the
#' equivalent of assays, are returned.
#'
#' @param object a single-cell object. Currently, Seurat and
#' SingleCellExperiment objects are supported.
#' @param ... Currently unused.
#'
#' @rdname assay_names
#' 
#' @keywords internal
#'
assay_names <-
  function(
    object,
    ...
  ){
    UseMethod("assay_names")
  }

#' Function to display an error message when an unsupported object
#' type is detected
#'
#' @noRd
#' @export
assay_names.default <-
  function(
    object
  ){
    warning(
      paste0(
        "assay_names does not know how to handle object of class ",
        paste(class(object), collapse = ", "),
        ". Currently supported classes: Seurat and SingleCellExperiment."
      )
    )
  }

#' @describeIn assay_names Seurat objects
#' @export
assay_names.Seurat <-
  function(
    object
  ){
    # Seurat objects: access assays directly
    names(object@assays)
  }

#' @describeIn assay_names SingleCellExperiment objects
#' @export
assay_names.SingleCellExperiment <-
  function(
    object
  ){
    # SingleCellExperiment objects: return names of main, alternate experiments
    c(SingleCellExperiment::mainExpName(object), 
      SingleCellExperiment::altExpNames(object)
      )
  }

#' @describeIn assay_names Anndata objects
#'
#' Anndata objects do not have an exclusive structure for modalities/assays.
#' Additional modalities are stored in obsm, but this slot is not specific to
#' modalities. To use assay_names with Anndata objects, a vector of assays
#' (python list) must be stored in `object.uns["cellDIVER_assays"]`. This method
#' will return an error if this data is not present in the object.
#'
#' @export
assay_names.AnnDataR6 <-
  function(
    object
  ){
    if (!is.null(object$uns[["cellDIVER_assays"]])){
      # Add X matrix to the obsm keys matching modalities
      c("X", object$uns[["cellDIVER_assays"]])
    } else {
      stop("For anndata objects, the obsm_keys that correspond to modalities ",
           "must be defined in `uns$cellDIVER_assays`. If no modalities are ",
           "present in obsm, uns$cellDIVER_assays should be NULL/None.")
    }
  }

#' @describeIn assay_names MuData objects
#' @export
assay_names.md._core.mudata.MuData <-
  function(
    object
  ){
    # Mudata objects: use modality names
    object$mod_names
  }

#' @export
assay_names.mudata._core.mudata.MuData <-
  function(
    object
  ){
    # mudata._core.mudata.MuData: possible class when loading 
    # Redirect to md._core.mudata.MuData method
    assay_names.md._core.mudata.MuData(
      object
    )
  }
  