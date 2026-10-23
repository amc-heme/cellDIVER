#' Object class warning
#'
#' Returns a warning if the object loaded is not a supported single-cell-object,
#' and does nothing if it is. 
#'
#' @param object a single-cell object. 
#' @param path path of object loaded
#' @param return_error if TRUE, an error is returned if the dataset is of an 
#' unsupported class. If FALSE, a warning is returned.
#' 
#' @noRd
check_dataset <-
  function(
    object,
    path = NULL,
    return_error = FALSE
  ){
    UseMethod("check_dataset")
  }

#' Function to display an warning when an unsupported object type is detected
#'
#' @export
#' @noRd
check_dataset.default <-
  function(
    object,
    path = NULL,
    return_error = FALSE
  ){
    msg <-
      paste0(
        if (is.null(path)){
          paste0(
            "Unsupported object class: ",
            paste(class(object), collapse = ", "),
            ". ",
            )
        } else {
          paste0(
            "Object at path ",
            path,
            " is of an unsupported object class (",
            paste(class(object), collapse = ", "),
            "). "
          )
        },
        if (return_error == FALSE){
          "Unexpected behavior will likely result. "
        },
        "Currently supported classes: Seurat and SingleCellExperiment."
      )
    
    # Display error or warning based on return_error
    if (return_error){
      stop(msg)
      } else {
        warning(msg)
        }
    }

#' @describeIn check_dataset Seurat objects
#' @export
#' @noRd
check_dataset.Seurat <-
  function(
    object,
    path = NULL,
    return_error = FALSE
  ){
    # Do nothing (object passes)
  }

#' @describeIn check_dataset SingleCellExperiment objects
#' @export
#' @noRd
check_dataset.SingleCellExperiment <-
  function(
    object,
    path = NULL,
    return_error = FALSE
  ){
    # Do nothing (object passes)
  }

#' @describeIn check_dataset Anndata objects
#' @export
#' @noRd
check_dataset.AnnDataR6 <-
  function(
    object,
    path = NULL,
    return_error = FALSE
  ){
    # Do nothing (object passes)
  }

#' @describeIn check_dataset MuData objects
#' @export
#' @noRd
check_dataset.md._core.mudata.MuData <-
  function(
    object,
    path = NULL,
    return_error = FALSE
    ){
      # Do nothing (object passes)
  }

#' @export
#' @noRd
check_dataset.mudata._core.mudata.MuData <-
  function(
    object,
    path = NULL,
    return_error = FALSE
  ){
    # Do nothing (object passes)
  }
# File-backed SCUBA objects (.h5ad / .h5mu opened with SCUBA::open_h5ad() /
# open_h5mu()) always pass: SCUBA validated the file when it opened it
#' @export
#' @noRd
check_dataset.scuba_source <-
  function(
    object,
    path = NULL,
    return_error = FALSE
  ){
    # Do nothing (object passes)
  }
