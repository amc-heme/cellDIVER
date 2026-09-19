#' Creation of ADT threshold assay
#'
#' Applies a set of ADT thresholds defined in the config file to the assay 
#' designated in the config file as the ADT assay. 
#'
#' @param object a single-cell object. Currently, Seurat and 
#' SingleCellExperiment objects are supported.
#' @param threshold_table the threshold table defined in the config file.
#' @param designated_adt_assay the assay designated in the config file as the 
#' ADT assay. 
#'
#' @return the single cell object passed to \code{object} with the adt threshold
#' assay added.
#' 
#' @noRd
adt_threshold_assay <-
  function(
    object, 
    threshold_table,
    designated_adt_assay
  ){
    UseMethod("adt_threshold_assay")
  }

#' Function to display an error message when an unsupported object
#' type is detected
#'
#' @export
#' @noRd
adt_threshold_assay.default <-
  function(
    object, 
    threshold_table,
    designated_adt_assay
  ){
    warning(
      paste0(
        "adt_threshold_assay does not know how to handle object of class ",
        paste(class(object), collapse = ", "),
        ". Currently supported classes: Seurat and SingleCellExperiment."
      )
    )
  }

#' @describeIn adt_threshold_assay Seurat objects
#' @export
#' @noRd
adt_threshold_assay.Seurat <-
  function(
    object, 
    threshold_table,
    designated_adt_assay
  ){
    # 1. Duplicate ADT assay 
    adt_assay <- object[[designated_adt_assay]]
    
    # (camel case used because seruat can't save keys using snake case)
    Key(adt_assay) <- "adtThreshold_"
    
    # Save new assay under adtThreshold
    object[["adtThreshold"]] <- 
      adt_assay
    
    #  Subset assay to features for which threshold information exists
    #  to conserve memory.
    #  SeuratObject warns whenever an assay's features differ from the
    #  object's, which is exactly what this assay is for - it deliberately
    #  holds only the thresholded features. Silence that one message rather
    #  than letting an expected warning sit in every test run, and let any
    #  other warning through untouched.
    withCallingHandlers(
      object[["adtThreshold"]] <-
        subset(
          object[["adtThreshold"]],
          features = threshold_table$adt
          ),
      warning = function(condition) {
        if (grepl(
          "Different cells and/or features from existing assay",
          conditionMessage(condition), fixed = TRUE
        )) {
          invokeRestart("muffleWarning")
        }
      }
      )
    
    # 2. Apply normalization to ADT threshold assay
    for (i in 1:nrow(threshold_table)){
      ADT <- threshold_table$adt[i]
      threshold <- threshold_table$value[i]
      
      ## 2.1. Subtract threshold
      object@assays$adtThreshold@data[ADT,] <-
        object@assays$adtThreshold@data[ADT,] - threshold 
      
      ## 2.2. "Clamp" expression values for ADT to zero
      object@assays$adtThreshold@data[ADT,] <- 
        sapply(
          object@assays$adtThreshold@data[ADT,],
          function(value){
            if (value < 0) 0 else value
          }
        )
    }
    
    object
  }

#' @describeIn adt_threshold_assay SingleCellExperiment objects
#' @export
#' @noRd
adt_threshold_assay.SingleCellExperiment <-
  function(
    object, 
    threshold_table,
    designated_adt_assay
  ){
    # 1. Duplicate ADT assay (experiment) from SCE object as "adtThreshold"
    ## 1.1. Pull ADT assay (experiment) from SCE object
    # Switch to alternate experiment if that experiment is the ADT exp (assay). 
    adt_exp <-
      if (designated_adt_assay %in% altExpNames(object)){
        altExps(object)[[designated_adt_assay]]
      } else {
        object
      }
    
    ## 1.2. Change mainExpName of copied exp to "adtThreshold", 
    # then save to object
    # (camel case used due to specifics with Seruat method, see note above)
    mainExpName(adt_exp) <- "adtThreshold"
    altExp(object, "adtThreshold") <- adt_exp
    
    # 1.3. Subset threshold experiment 
    # Only include features in the ADT experiment
    adt_thresh_exp <- altExp(object, "adtThreshold")
    adt_thresh_exp <- 
      adt_thresh_exp[rownames(adt_thresh_exp) %in% threshold_table$adt,]
    
    # 2. Apply normalization to ADT threshold assay
    # Iterate through each ADT feature in table and clamp values 
    for (i in 1:nrow(threshold_table)){
      adt <- threshold_table$adt[i]
      threshold <- threshold_table$value[i]
      
      ## 2.1. Subtract threshold
      assay(adt_thresh_exp, "logcounts")[adt,] <-
        assay(adt_thresh_exp, "logcounts")[adt,] - threshold
      
      ## 2.2. "Clamp" expression values for ADT to zero
      assay(adt_thresh_exp, "logcounts")[adt,] <- 
        sapply(
          assay(adt_thresh_exp, "logcounts")[adt,],
          function(value){
            if (value < 0) 0 else value
          }
        )
    }
    
    # Save modified assay to object
    altExp(object, "adtThreshold") <- adt_thresh_exp
    
    # Return object with new assay
    object
  }
