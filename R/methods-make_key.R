#' Return key for an assay
#'
#' Equivalent to `Key(object[[assay]])` for Seurat objects. Returns the 
#' key associated with an assay, which is used in [feature_list_all] to define
#' the machine-readable names for all features, so they can be accessed in the 
#' app.
#'
#' @param object a single-cell object. Currently, Seurat and
#' SingleCellExperiment objects are supported.
#' @param assay the assay (or experiment for SingleCellExperiment objects) to 
#' return the key for.
#'
#' @return the key for the assay, ending with an underscore. The key can be used
#' for accessing features not in the main assay.
#' 
#' @noRd
make_key <-
  function(
    object,
    assay
  ){
    UseMethod("make_key")
  }

#' Function to display an error message when an unsupported object
#' type is detected
#'
#' @export
#' @noRd
make_key.default <-
  function(
    object,
    assay
  ){
    warning(
      paste0(
        "make_key does not know how to handle object of class ",
        paste(class(object), collapse = ", "),
        ". Currently supported classes: Seurat and SingleCellExperiment."
      )
    )
  }

#' @describeIn make_key Seurat objects
#' @export
#' @noRd
make_key.Seurat <-
  function(
    object,
    assay
  ){
    # Use key method from Seurat package
    Seurat::Key(object[[assay]])
  }

#' @describeIn make_key SingleCellExperiment objects
#' @export
#' @noRd
make_key.SingleCellExperiment <-
  function(
    object,
    assay
  ){
    # SingleCellExperiment objects don't have a Key() method. Instead, a 
    # Seurat-style key will be generated using the assay name and an underscore.
    # This will be intelligible by the fetch_data.SingleCellExperiment defined 
    # in the SCUBA package.
    paste0(assay, "_")
  }

#' @describeIn make_key Anndata objects
#' @export
#' @noRd
#' @describeIn make_key File-backed objects served by SCUBA's Julia engine (`scuba_source`)
#' @export
make_key.scuba_source <-
  function(
    object,
    assay
  ){
    paste0(assay, "_")
  }

make_key.AnnDataR6 <-
  function(
    object,
    assay
  ){
    # As with SingleCellExperiment, add an underscore to the assay name.
    paste0(assay, "_")
  }

#' @describeIn make_key MuData objects
#' @export
#' @noRd
make_key.md._core.mudata.MuData <-
  function(
    object,
    assay
  ){
    # Add an underscore to the assay (modality) name
    paste0(assay, "_")
  }

#' @export
#' @noRd
check_dataset.mudata._core.mudata.MuData <-
  function(
    object,
    assay
  ){
    # mudata._core.mudata.MuData: possible class when loading 
    # Redirect to md._core.mudata.MuData method
    check_dataset.md._core.mudata.MuData(
      object,
      assay
    )
  }
  