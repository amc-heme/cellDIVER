#' ncol_limits
#'
#' Determines the min and max settings for the ncol slider for DimPlots, feature
#' plots, and violin plots based on selections made by the user in cellDIVER.
#'
#' @param object A single cell object (any object class supported by SCUBA
#' may be entered).
#' @param rule The means of calculating ncol limits, depending on the plot type.
#' This can be either "split_by" (for DimPlots and feature plots), or "features"
#' (for violin plots).
#' @param split_by If rule == "split_by", the current split_by selection.
#' @param features_entered If rule == "features", the number of features 
#' currently entered.
#'
#' @return A three element vector. The elements are as follows:
#' 1. Minimum ncol selection
#' 2. Maximum ncol selection
#' 3. Default ncol value
#' 
#' @noRd
ncol_settings <- 
  function(
    object,
    rule = "split_by",
    split_by = NULL,
    features_entered = NULL
    ){
    # Error handling
    if (!rule %in% c("split_by", "features")){
      stop('Argument `rule` must be set to either "split_by" or "features".')
    }
    
    if (rule == "split_by" & is.null(split_by)){
      stop('When rule == "split_by", argument `split_by` must be defined.')
    }
    
    if (rule == "features" & is.null(features_entered)){
      stop('When rule == "features", argument `features_entered` must be defined.')
    }
    
    # Lower limit is always equal to one.
    min = 1
    
    # Upper limit
    # Number of panels is used to calculate upper limit 
    if (rule == "split_by"){
      # split_by rule: n_panels based on number of split by groups
      n_panels <- 
        cellDIVER:::n_unique(
          object = object, 
          meta_var = split_by
          )
      
      # Max == n_panels
      max <- n_panels
    } else if (rule == "features"){
      # When rule == "features", n_panels is equal to number of features.
      n_panels <- length(features_entered)
      # Max == n_panels
      max <- n_panels
    }
    
    # Default: depends on number of panels
    if (n_panels < 4){
      # 1-3 panels: display side-by-side (default == n_panels)
      default <- n_panels
    } else if (n_panels >= 4 & n_panels < 12){
      # Between 4 and 12 panels:  use floor value of square root of 
      # number of panels.
      default <- 
        sqrt(n_panels) |> 
        floor()
    } else {
      # Greater than 12: 4 panels looks better than result of function above
      # (yields 3 for 12 < n_panels < 16). Beyond 16 panels, 4 columns is max
      # value that fits well on default display.
      default <- 4
    }
    
    # Return a vector with min and max limits, and default
    return(
      c("min" = min, 
        "max" = max, 
        "default" = default)
      )
  }
