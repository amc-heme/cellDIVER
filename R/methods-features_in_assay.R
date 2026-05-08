#' Get names of all features in an assay/experiment
#'
#' Returns the names of all features in an assay/modality
#'
#' @param object a single-cell object. 
#' @param assay the name of an assay/modality for which to view features
#' @param ... Currently unused.
#'
#' @rdname features_in_assay
#' 
#' @keywords internal
#'
features_in_assay <-
  function(
    object,
    assay,
    ...
  ){
    lifecycle::deprecate_warn(
      when = "1.0.0",
      what = "cellDIVER::features_in_assay()",
      details = 
        paste0(
          "Please use `SCUBA::features_in_assay()` instead."
        )
    )
    
    SCUBA::features_in_assay(object, assay)
  }