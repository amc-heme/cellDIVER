#' Get names of reductions in object
#'
#' Returns the names of all reductions in a single-cell object.
#'
#' @param object a single-cell object. Currently, Seurat and
#' SingleCellExperiment objects are supported.
#' @param ... Currently unused.
#'
#' @rdname reduction_names
#' 
#' @keywords internal
#'
reduction_names <-
  function(
    object,
    ...
  ){
    UseMethod("reduction_names")
  }

#' Function to display an error message when an unsupported object
#' type is detected
#'
#' @noRd
#' @export
reduction_names.default <-
  function(
    object
  ){
    warning(
      paste0(
        "reduction_names does not know how to handle object of class ",
        paste(class(object), collapse = ", "),
        ". Currently supported classes: Seurat, SingleCellExperiment, AnndataR6, md._core.mudata.MuData, and mudata._core.mudata.MuData."
      )
    )
  }

#' @describeIn reduction_names Seurat objects
#' @export
reduction_names.Seurat <-
  function(
    object
  ){
    # Seurat objects: access reductions directly
    names(object@reductions)
  }

#' @describeIn reduction_names SingleCellExperiment objects
#' @export
reduction_names.SingleCellExperiment <-
  function(
    object
  ){
    # SingleCellExperiment objects: use reducedDimNames
    reducedDimNames(object)
  }

#' @describeIn reduction_names Anndata objects
#'
#' Anndata objects do not have an exclusive structure for reductions. Reductions
#' are stored in obsm, but matrices that are not reductions may exist in this
#' slot. To use reduction_names with Anndata objects, a vector of reductions
#' (python list) must be stored in `object.uns["cellDIVER_reductions"]`. This method
#' will return an error if this data is not present in the object.
#'
#' @export
reduction_names.AnnDataR6 <-
  function(
    object
  ){
    if (!is.null(object$uns[["cellDIVER_reductions"]])){
      # Add X matrix to the obsm keys matching modalities
      c(object$uns[["cellDIVER_reductions"]])
    } else {
      stop("For anndata objects, the obsm_keys that correspond to reductions ",
           "must be defined in `uns$cellDIVER_reductions`. If no reductions are ",
           "present in obsm, uns$cellDIVER_reductions should be NULL/None.")
    }
  }

#' @describeIn reduction_names MuData objects
#' 
#' For MuData objects, the keys of reductions are the keys of obsm matrices 
#' in each modality (mod-obsm keys). obsm matrices in each modality are not
#' necesarially reductions. It is left to the admin to decide which reductions
#' to include.
#' 
#' @export
reduction_names.md._core.mudata.MuData <-
  function(
    object
  ){
    mod_obsm_keys <- c()
    
    for (mod in object$mod_names){
      # Record obsm keys for each modality, and construct mod_obsm keys 
      # using the name of the modality and an underscore
      mod_obsm <- object[[mod]]$obsm_keys()
      
      if (length(mod_obsm) > 0){
        mod_obsm_keys <- c(
          mod_obsm_keys,
          paste(mod, mod_obsm, sep = "_")
          )
        }
      }
    
    mod_obsm_keys
  }

#' @export
reduction_names.mudata._core.mudata.MuData <-
  function(
    object
  ){
    # mudata._core.mudata.MuData: possible class when loading 
    # Redirect to md._core.mudata.MuData method
    reduction_names.md._core.mudata.MuData(object)
  }
  
