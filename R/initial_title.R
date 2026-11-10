#' Initial Value for Single Custom Title Input
#' 
#' Using config info for the currently selected object, generates the default 
#' title for the custom title input box. The default title for DimPlots is the
#' currently selected group_by category, and the default title for feature plots
#' is the current feature. 
#'
#' @param plot_type The type of plot for which custom titles are being 
#'   generated. Either "dimplot", "feature", "hexbin_density", 
#'   "hexbin_feature", "ridge", "proportion", or "pie".
#' @param group_by The currently selected group_by category. Must be specified 
#'   when plot_type == "dimplot", and is not used when plot type == "feature".
#' @param metadata_config The metadata section of the config file loaded at app 
#'   startup. Must be specified when plot_type == "dimplot", and is not used 
#'   when plot type == "feature".
#' @param features_entered The features currently selected (for feature plots)
#' @param action The aggregation action for hexbin feature plots (mean, median, 
#'   or sum)
#'
#' @noRd
initial_title <- function(
  plot_type,
  group_by = NULL,
  metadata_config = NULL, 
  assay_config = NULL,
  features_entered = NULL,
  show_modality_key = FALSE,
  action = NULL
  ){
  if (!plot_type %in% 
      c("dimplot", "feature", "hexbin_density", "hexbin_feature", "ridge", 
        "proportion", "pie")){
    stop(
      'Agrument `plot_type` must be one of "dimplot", "feature", ',
      '"hexbin_density", "hexbin_feature", "ridge", "proportion", or "pie"'
      )
  }
  
  if (plot_type %in% c("dimplot", "proportion", "pie")){
    # For DimPlots, cell proportion plots, and pie charts: use name of group_by 
    # category
    
    # Throw error if metadata_config or group_by are undefined
    if (is.null(metadata_config)){
      stop(
        'When `plot_type` is equal to "dimplot" or "proportion", ',
        '`metadata_config` must be defined.'
      )
    }
    if (is.null(group_by)){
      stop(
        'When `plot_type` is equal to "dimplot" or "proportion", ',
        '`group_by` must be defined.'
      )
    }
    
    if (!is.null(group_by)){
      config_label <-
        metadata_config[[group_by]]$label
      
      # Define initial value of text entry
      initial_value <-
        # Use label if it exists
        if (!is.null(config_label)){
          config_label
        } else {
          # If a label is not defined, use the name
          # of the group by category
          group_by
        }
    } else {
      initial_value <- ""
    }
  } else if (plot_type == "hexbin_density"){
    # For hexbin density plots: use "Density" as default title
    initial_value <- "Density"
    
  } else if (plot_type == "hexbin_feature"){
    # For hexbin feature plots: use feature name with action method
    
    # Require assay_config to be defined for human-readable names
    if (is.null(assay_config)){
      stop("assay_config must be defined for hexbin feature plots.")
    }
    
    # Set default action if not provided
    if (is.null(action)){
      action <- "mean"
    }
    
    if (!is.null(features_entered)){
      if (length(features_entered) == 1){
        # For single feature plots
        if (show_modality_key == FALSE){
          feature_name <- 
            # Remove assay key from feature, and add assay label if defined
            hr_name(
              machine_readable_name = features_entered, 
              assay_config = assay_config,
              use_suffix = TRUE
            )
        } else {
          feature_name <- features_entered
        }
        # Add action method to title
        initial_value <- paste0(feature_name, " (", action, ")")
      } else {
        # For multi-feature plots, there currently is no framework for a
        # single-title entry. Set inital_value to "" regardless to avoid
        # errors (may have to change this to improve performance)
        initial_value <- ""
      }
    } else {
      initial_value <- ""
    }
    
  } else if (plot_type %in% c("feature", "ridge")){
    # For feature and ridge plots, require assay_config to be defined
    # (to print human-readable names as initial custom titles)
    if (is.null(assay_config)){
      stop("assay_config must be defined for feature and ridge plots.")
    }
    
    # For feature plots: name of feature
    if (!is.null(features_entered)){
      if (length(features_entered) == 1){
        if (show_modality_key == FALSE){
          initial_value <- 
            # Remove assay key from feature, and add assay label if defined
            hr_name(
              machine_readable_name = features_entered, 
              assay_config = assay_config,
              use_suffix = TRUE
            )
        } else {
          initial_value <-
            features_entered
        }
      } else {
        # For multi-feature plots, there currently is 
        # no framework for a single-title entry.
        # Set inital_value to "" regardless to avoid 
        # errors (may have to change this to improve
        # perfomrance)
        initial_value <- ""
      }
    } else {
      initial_value <- ""
    }
  }
  
  # Return value
  initial_value
}