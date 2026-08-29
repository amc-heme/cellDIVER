#' Re-label axis behavior
#'
#' Code to relabel the axis in the expr_plot function. The default axis text is
#' "Feature Expression", and it changes or is removed based on the type of data
#' being plotted. If the data is a reduction, the label changes to "Embeddings
#' Value". If the data comes from a source that is not an assay or a reduction,
#' the label is removed. The label changed (x-axis or y-axis) depends on the
#' type of plot being created by expr_plot (violin, dot, etc.).
#'
#' For Anndata objects, the label will always show as "Feature Expression",
#' unless the feature plotted is a metadata variable, in which case no label
#' will be drawn.
#'
#' Labels can be manually added after plot creation with ggplot2::labs() in the
#' event a different label is more appropriate for the feature being plotted.
#'
#' @param object a single-cell object. Currently, Seurat,
#' SingleCellExperiment, Anndata, and MuData objects are supported.
#' @param feature the feature being plotted. Only one feature should be passed
#' to this generic at once. For multi-feature plot, this generic should be ran
#' separately on each feature, and the changes in labeling applied to each
#' individual plot (patchwork object) iteratively.
#'
#' @rdname relabel_axis
#' 
#' @keywords internal
#'
#' @export
relabel_axis <-
  function(
    object,
    feature,
    ...
  ){
    UseMethod("relabel_axis")
  }

#' Function to display an error message when an unsupported object
#' type is detected
#'
#' @noRd
#' @export
relabel_axis.default <-
  function(
    object,
    feature
  ){
    warning(
      paste0(
        "relabel_axis does not know how to handle object of class ",
        paste(class(object), collapse = ", "),
        ". Currently supported classes: Seurat, Anndata, and SingleCellExperiment."
      )
    )
  }

#' @describeIn relabel_axis Seurat objects
#' @export
relabel_axis.Seurat <-
  function(
    object,
    feature
  ){
    # 1. Determine key from feature entered
    key <-
      paste0(
        unlist(
          x = strsplit(x = feature, split = '_'))[1]
        , '_'
      )
    
    # 2. Determine if the feature is from an assay or a reduction
    # based on the key
    
    # obj_ref: Object referred to by key. The name of the assay/reduction/etc
    # associated with a key (what you would use to pull the data via `[[`.
    # For "rna_", this would be "RNA" (the RNA assay). For "UMAP_", this would
    # be "umap".
    # (Code adapted from Seurat:::ExIPlot)
    obj_ref <- names(which(Key(object) == key))
    
    # Seurat objects: Test class of obj[[obj_ref]]
    key_type <-
      if (length(obj_ref) == 1){
        # If the key aligns with the list of all keys, determine the type
        # of the object corresponding to the key
        if (inherits(x = object[[obj_ref]], what = 'Assay')){
          "Assay"
        } else if (inherits(x = object[[obj_ref]], what = 'DimReduc')){
          "Reduction"
        } else {
          "Other"
        }
      } else {
        # Return "other" in the event that `key` does not match any of the
        # object keys (or if it matches multiple, though this should never
        # happen)
        "Other"
      }
    
    # 3. Determine what label should be changed to, if anything
    if (key_type == "Assay"){
      # No changes to labels needed for assays
      return(NULL)
    } else if (key_type == "Reduction"){
      # If the feature is a reduction, change label "Embeddings Value"
      return("Embeddings Value")
    } else if (key_type == "Other") {
      # For "other" cases: key will be a feature name if a feature from the
      # default assay (or main experiment for SingleCellExperiment objects)
      # is entered.
      if (feature %in% rownames(object)){
        # If the feature is part of the default assay, no changes are needed
        return(NULL)
      } else {
        # Returns an empty character vector, which will remove the label
        return("")
      }
    }
  }

#' @describeIn relabel_axis SingleCellExperiment objects
#' @export
relabel_axis.SingleCellExperiment <-
  function(
    object,
    feature
  ){
    # 1. Determine key from feature entered
    key <-
      unlist(
        x = strsplit(x = feature, split = '_')
      )[1]
    
    # 2. Determine if the feature is from an assay or a reduction
    # based on the key
    
    # SingleCellExperiment objects: search for key in experiments, reductions
    # obj_ref: name of "object" (experiment or reduction) corresponding to key
    obj_ref <- names(which(SCUBA::all_keys(object) == key))
    
    # Check if key matches exactly one object. If not, return "Other"
    key_type <-
      if (length(obj_ref) == 1){
        # If exactly one object, set type if recognized
        if (key %in% c(mainExpName(object), altExpNames(object))){
          # Term "Assay" used for consistency with Seurat method
          "Assay"
        } else if (key %in% reducedDimNames(object)){
          "Reduction"
        } else {
          "Other"
        }
      } else {
        "Other"
      }
    
    # 3. Determine what label should be changed to, if anything
    if (key_type == "Assay"){
      # No changes to labels needed for assays
      return(NULL)
    } else if (key_type == "Reduction"){
      # If the feature is a reduction, change label "Embeddings Value"
      return("Embeddings Value")
    } else if (key_type == "Other") {
      # For "other" cases: key will be a feature name if a feature from the
      # default assay (or main experiment for SingleCellExperiment objects)
      # is entered.
      if (feature %in% rownames(object)){
        # If the feature is part of the default assay, no changes are needed
        return(NULL)
      } else {
        # Returns an empty character vector, which will remove the label
        return("")
      }
    }
  }

#' @describeIn relabel_axis Anndata objects
#' @export
relabel_axis.AnnDataR6 <-
  function(
    object,
    feature
  ){
    # Due to the flexibility of Anndata objects, it's impossible to determine
    # if a key corresponds to an assay or a reduction, unless it corresponds to
    # the X matrix
    
    # 1. Determine key from feature entered
    key <-
      unlist(
        x = strsplit(x = feature, split = '_')
      )[1]
    
    # 2. If the key is "X", leave the axis unchanged. If the key is not "X",
    # remove the label.
    if (key == "X"){
      # Special case: features like X_umap_1 are not genes. In the event of a
      # feature with multiple underscores, the label is removed.
      if (
        length(
          unlist(
            strsplit(
              x = feature,
              split = "_"
            )
          )
        ) > 2){
        ""
      } else {
        NULL
      }
    } else {
      # Labels can be changed manually with ggplot2::labs()
      ""
    }
  }

#' @describeIn relabel_axis MuData objects
#' @export
relabel_axis.md._core.mudata.MuData <-
  function(
    object,
    feature
  ){
    # As with anndata objects, it can't be definitively concluded that a key
    # corresponds to a reduction, so keys that are not modality keys will 
    # result in the removal of a label of an axis.
    # 1. Determine key from feature entered
    key <-
      unlist(
        x = strsplit(x = feature, split = '_')
      )[1]
    
    # 2. If the key is a modality key, leave the axis unchanged. 
    # Otherwise, remove the label.
    if (key %in% object$mod_names){
      # NULL will leave the axis unchanged
      NULL
    } else {
      # Labels can be changed manually with ggplot2::labs()
      ""
    }
  }

#' @export
relabel_axis.mudata._core.mudata.MuData <-
  function(
    object,
    feature
  ){
    # mudata._core.mudata.MuData: possible class when loading 
    # Redirect to md._core.mudata.MuData method
    relabel_axis.md._core.mudata.MuData(
      object,
      feature
    )
  }