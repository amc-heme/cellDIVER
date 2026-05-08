#' Wrapper for FeaturePlot
#'
#' This function allows for additional customization of feature plots. Only one
#' feature is supported by this function.
#'
#' @param object A Seurat object (may be the full object or a subset).
#' @param feature A feature to plot: only one feature is supported
#' @param split_by Metadata to split plots by. Identical in function to 
#' split.by used for FeaturePlots.
#' @param label_by Metadata variable to use for labeling groups.
#' @param colors A vector of colors to use for plotting expression values. Any
#' number of colors may be specified, and palettes can be passed if they are 
#' formatted so that they yield a list of color hex codes. If NULL, the default
#' Seurat palette will be used.
#' @param color_lower This is deprecated, please use `colors` instead.
#' @param color_upper This is deprecated, please use `colors` instead.
#' @param ncol The number of columns to use when plotting multiple panels 
#' (applies when split_by is defined)
#' @param show_title If TRUE, display titles on each panel of the plot.
#' @param custom_titles A vector of titles to use for each panel of the plot. 
#' If NULL, default titles will be used (defaults to name of each split by
#' category if split_by is defined, or the name of the feature if no 
#' split.by category is defined). 
#' @param legend_title 
#' 
#' If "default", display feature name above legend. If 
#' "right", display feature name vertically on the right-hand side of the plot. 
#' If "none", do not display a title. If NULL, legend_title is set to "default".
#' @param super_title If TRUE, display the feature name above all panels. This 
#' argument is ignored when split_by is NULL.
#' @param reduction The dimensionality reduction to use for plotting cell 
#' coordinates.
#' @param xlim If specified, custom limits to use for the x-axis of the feature 
#' plot. When xlim is NULL, x-axis limits are based on the min and max x 
#' coordinates in the selected reduction of the object specified. xlim should 
#' be a two-element numeric vector giving the min and max x values.
#' @param ylim If specified, custom limits to use for the y-axis of the
#' feature plot. ylim should be a two-element numeric vector giving the min and 
#' max y values.
#' @param show_legend If TRUE, show the legend of the feature plot (default is 
#' TRUE). 
#' @param assay_config the assays section of the config file, loaded at app 
#' startup and upon changing datasets.
#' @param ... A list of arguments to pass to FeaturePlot
#'
#' @examples
#' FeaturePlotSingle(
#'      object = prim.mono, 
#'      feature = "XIST", 
#'      split_by = "htb", 
#'      pt.size = 0.05, 
#'      colors = viridisLite::rocket(42, direction = -1),
#'      custom_titles = c("Normal", "Ven Reistant", "Ven Sensitive"),
#'      order = TRUE
#'      )
#'
#' @noRd
#' 
FeaturePlotSingle<- 
  function(
    object, 
    feature, 
    split_by = NULL, 
    label_by = NULL,
    colors = NULL,
    # Deprecated: use colors vector instead
    color_lower = NULL,
    color_upper = NULL,
    ncol = NULL,
    show_title = TRUE,
    custom_titles = NULL, 
    legend_title = "feature",
    super_title = FALSE,
    reduction = NULL,
    xlim = NULL,
    ylim = NULL,
    show_legend = TRUE,
    assay_config = NULL,
    ...
    ){
    # Handling of NULL values
    # If reduction is NULL, set to the default for the object
    if (is.null(reduction)){
      reduction <- SCUBA::default_reduction(object)
    }
    # If colors is NULL, set to default Seurat gradient for continuous data
    if (is.null(colors)){
      colors <- c('lightgrey', 'blue')
    }
    # If legend_title is NULL, set it to "default"
    if (is.null(legend_title)){
      legend_title <- "default"
    }
    
    # Legend title options
    # if (!legend_title %in% c("default", "right", "none")){
    #   stop("Invalid value for legend_title. Please enter one of `default`, `right`, or `none`, or input NULL for default behavior.")
    # }
    
    # Handling color_lower and color_upper inputs: form colors 
    # vector and warn user 
    if (!is.null(color_lower) & !is.null(color_upper)){
      warning(
        "Use of color_lower and color_upper is deprecated. Please pass a vector of colors to the `colors` argument instead"
      )
      colors <- c(color_lower, color_upper)
    }
  all_cells <- SCUBA::get_all_cells(object)
  # If a metadata column is defined, define a vector of groups based on the
  # unique values of the column.
  if (!is.null(split_by)){
    groups <- 
      SCUBA::unique_values(
        object,
        var = split_by
        ) |>  
      # Will plot groups in lexicographical order
      str_sort(numeric = TRUE)
  } else {
    # If a split.by column is not defined, groups is set to an empty character
    # vector of length 1
    groups <- ""
  }
  
  # Define number of columns
  # If ncol is not provided, default is number of groups
  if (is.null(ncol)){
    ncol <- length(groups)
  }
  
  # Pull minimum and maximum expression values to make the legend scale the same.
  feature_data <-
    SCUBA::fetch_data(
      object,
      vars = feature,
      # Slot: will use "data" for Seurat objects and "logcounts" 
      # for SingleCellExperiment objects
      layer = SCUBA::default_layer(object)
    )[,1]
  minimal <- min(feature_data)
  maximal <- max(feature_data)
  
  # Pull min/max values to make the x and y scales the same.
  # If limits are not defined (default behavior), use the min and max of cell
  # coordinates in the current object
  if (is.null(xlim) | is.null(ylim)){
    reduction_coords <-
      SCUBA::fetch_reduction(
        object,
        reduction = reduction,
        dims = c(1, 2)
      )
    }
  
  if (is.null(xlim)){
    xmin <- min(reduction_coords[,1])
    xmax <- max(reduction_coords[,1])
  } else {
    xmin <- xlim[1]
    xmax <- xlim[2]
  }
  
  if (is.null(ylim)){
    ymin <- min(reduction_coords[,2])
    ymax <- max(reduction_coords[,2]) 
  } else {
    ymin <- ylim[1]
    ymax <- ylim[2]
  }
  
  
  # Put feature name on top of colorbar legend
  # Choices for name on legend title
  legend_title <- 
    if (legend_title == "feature"){
      ggtitle(
        # Remove assay key from feature name
        hr_name(
          machine_readable_name = feature,
          assay_config = assay_config,
          # Do not use suffix for legend title
          use_suffix = FALSE
        )
      )
    } else if (legend_title == "assay_score"){
      # Assay score: short description of what is being measured in the assay
      # (expression, enrichment, etc.). Will be set in config file
      "Expression"
    } else if (legend_title == "none"){
      NULL
    }
  
  # If a split.by (metadata) column is defined, create a plot for each group 
  plots <- list()
  
  if (length(groups) > 1){
    # Fetch metadata table for referencing split_by groups
    meta_table <-
      SCUBA::fetch_metadata(
        object,
        full_table = TRUE
        )
    
    # Feature plot with multiple split.by groups
    for (group in groups) {
      i <- which(groups == group)
      
      subset_indx <- meta_table[, split_by] == group
      subset_cells <- all_cells[subset_indx]
      
      plot <- 
        cellDIVER::plot_feature(
          object, 
          features = feature, 
          cells = subset_cells, 
          reduction = reduction,
          label_by = label_by,
          ...
          ) +
        # List of layers to apply to plot: depends on settings specified
        coord_cartesian(
          xlim = c(xmin, xmax), 
          ylim = c(ymin, ymax)
        ) +
        theme(
          plot.title = 
            element_text(size = 14, face = "bold")
        ) 
      
      # Title: use custom titles if defined, otherwise use the group name
      if (show_title == TRUE){
        # If show_title is TRUE, use either default or custom titles
        if (is.null(custom_titles)){
          # custom_titles is NULL: use default behavior (group names)
          plot <-
            plot +
            ggtitle(group)
        } else {
          # custom_titles is not NULL: use custom titles 
          # (custom titles should be a character vector)
          plot <-
            plot +
            ggtitle(custom_titles[i])
        }
      } else {
        # If show_title is FALSE, remove titles
        plot <-
          plot +
          ggtitle(NULL)
      }

      plots[[group]] <- plot
      
    }
    
    plot <- 
      wrap_plots(
        plots, 
        ncol = ncol, 
        guides = "collect"
        ) 
    
    plot <-
      suppressMessages(
        plot &
          scale_color_gradientn(
            legend_title, 
            colors = colors, 
            limits = c(minimal, maximal)
          ) &
          theme(
            legend.position = 
              # Place legend at right if show_legend == TRUE, 
              # remove legend otherwise
              if (show_legend == TRUE) "right" else "none"
            )
      )
    
    # If super_title is TRUE, add feature name above all panels.
    if (super_title == TRUE){
      plot <-
        plot +
        plot_annotation(
          title = feature,
          theme = 
            theme(
              plot.title = 
                element_text(
                  face = "bold",
                  hjust = 0.5,
                  size = 16
                )
            )
        ) 
    }
    
    return(plot)
    
  } else {
    # Metadata column is not defined:
    # If only one group or if split_by is undefined, create one plot 
    # for the object provided.
    plot <- 
      suppressMessages(
        cellDIVER::plot_feature(
          object, 
          features = feature, 
          reduction = reduction,
          label_by = label_by,
          ...
        ) +
          coord_cartesian(
            xlim = c(xmin, xmax), 
            ylim = c(ymin, ymax)
          ) +
          scale_color_gradientn(
            legend_title, 
            colors = colors, 
            limits = c(minimal, maximal)
          ) +
          theme(
            legend.position = 
              # Place legend at right if show_legend == TRUE, 
              # remove legend otherwise
              if (show_legend == TRUE) "right" else "none"
            )
      )
    
    # Title: use custom titles if defined, otherwise use the group name
    if (show_title == TRUE){
      # If show_title is TRUE, use either default or custom titles
      if (is.null(custom_titles)){
        # custom_titles is NULL: use default behavior
        # Default behavior for single-group plots: use feature title
        plot <-
          plot +
          ggtitle(
            hr_name(
              machine_readable_name = feature,
              assay_config = assay_config
              )
            )
      } else {
        # custom_titles is not NULL: use custom titles
        # Custom titles should be a single-element character vector in this case
        plot <-
          plot +
          ggtitle(custom_titles)
      }
    } else {
      # If show_title is FALSE, remove titles
      plot <-
        plot +
        ggtitle(NULL)
    }
    
    
    
    
    # Apply title theme  
    plot <-
      plot +
      theme(
        plot.title = 
          element_text(size = 14, face = "bold")
      )
    
    return(plot)
      
  }
  
  }

#' Feature Plot Wrapper for Multi-Feature Plots
#' 
#' This wrapper is for multi-feature plots with no split.by variable.
#'
#' @param object A Seurat object (may be the full object or a subset).
#' @param features A series of features to plot.
#' @param label_by Metadata variable to use for labeling groups.
#' @param colors A vector of colors to use for plotting expression values. Any
#' number of colors may be specified, and palettes can be passed if they are 
#' formatted so that they yield a list of color hex codes. If NULL, the default
#' Seurat palette will be used.
#' @param color_by_feature If this is TRUE, a separate color will be used for
#' each feature. The colors to use are defined using the colors argument: any 
#' vector of colors defined using hex codes may be passed, and the gradients 
#' for each plot will go between light gray and the ith color. For example, if
#' colors is a vector of dark green (hex code #005500) and dark blue (hex code 
#' #000055), the first plot will have an expression gradient of light gray to 
#' dark green, and the  second plot will have an expression gradient of light 
#' gray to dark blue. 
#' @param ncol The number of columns to use when plotting multiple panels 
#' (applies when split_by is defined)
#' @param show_title If TRUE, display titles on each panel of the plot.
#' @param custom_titles A vector of titles to use for each panel of the plot. 
#' If NULL, default titles will be used (defaults to name of each feature). 
#' @param share_scale If TRUE, share the legend scale across features. If FALSE
#' (the default), use a different legend scale for different features.
#' @param legend_title May be one of "feature", "assay-score", "none", or NULL. 
#' If "feature", display the feature name above the plot. If a custom title is 
#' given for the feature, it will be applied to the legend if legend_title is
#' set to "feature". legend_title may not be set to "feature" if share_scale 
#' is TRUE. If "assay-score", the label for the assay score defined in the 
#' config file (i.e. "expression" for RNA, "enrichment" for gene signatures, 
#' etc.). If "none", no title is printed. If NULL (the default), legend_title 
#' will be set to "feature" if share_scale is FALSE, or "none" if share_scale
#' is TRUE.
#' @param reduction The dimensionality reduction to use for plotting cell 
#' coordinates.
#' @param xlim If specified, custom limits to use for the x-axis of the feature 
#' plot. When xlim is NULL, x-axis limits are based on the min and max x 
#' coordinates in the selected reduction of the object specified. xlim should 
#' be a two-element numeric vector giving the min and max x values.
#' @param ylim If specified, custom limits to use for the y-axis of the
#' feature plot. ylim should be a two-element numeric vector giving the min and 
#' max y values.
#' @param show_legend If TRUE, show the legend of the feature plot (default is 
#' TRUE). 
#' @param assay_config the assays section of the config file, loaded at app 
#' startup and upon changing datasets.
#' @param ... A list of arguments to pass to FeaturePlot
#' 
#' @noRd
MultiFeatureSimple <- 
  function(
    object, 
    features, 
    #split_by = NULL, 
    label_by = NULL,
    colors = NULL,
    color_by_feature = FALSE,
    ncol = NULL,
    show_title = TRUE,
    custom_titles = NULL, 
    share_scale = FALSE,
    legend_title = "feature",
    reduction = NULL,
    xlim = NULL,
    ylim = NULL,
    show_legend = TRUE,
    assay_config = NULL,
    #super_title = FALSE,
    ...
  ){
    # Handling of NULL values
    # If reduction is NULL, set to the default for the object
    if (is.null(reduction)){
      reduction <- SCUBA::default_reduction(object)
    }
    # If colors is NULL, set to default Seurat gradient for continuous data
    if (is.null(colors)){
      if (color_by_feature == FALSE){
        # Use default continuous palette if color_by_feature is FALSE 
        colors <- c('lightgrey', 'blue')
      } else {
        # If color_by_feature is TRUE, use default categorical palette
        # Number of colors in palette is set to number of features
        colors <- hue_pal()(length(features))
      }
      
    }
    # If legend_title is NULL, set it to "default"
    if (is.null(legend_title)){
      if (share_scale == TRUE){
        legend_title <- "feature"
      } else {
        legend_title <- "none"
      }
    } else {
      # If legend_title is not NULL but not equal to recognized options, warn
      # user
      if (!legend_title %in% c("feature", "assay_score", "none")){
        warning("Value for `legend_title` is not recognized; no legend title will be plotted. Please use one of `feature`, `assay_score`, or `none`.")
        # Plot no legend title when an invalid option is selected
        legend_title <- "none"
      }
      
      # If legend_title is not NULL but equal to "feature",
      # *when share_scale == TRUE*, warn the user and set to "none"
      if (share_scale == TRUE & legend_title == "feature"){
        warning("`legend_title` may not be equal to `feature` when `share_scale==TRUE`. No legend title will be plotted. Please use one of  `assay_score` or `none`.")
        legend_title <- "none"
      }
    }
    
    all_cells <- SCUBA::get_all_cells(object)
    
    # 1. Build groups ----------------------------------------------------------
    # Groups are features in this case
    groups <- features  
    
    # Define number of columns
    # If ncol is not provided, default is number of groups
    if (is.null(ncol)){
      ncol <- length(groups)
    }
    
    # If color_by_feature is TRUE, extract colors for each feature from the 
    # current palette
    if (color_by_feature == TRUE){
      # Use colorRampPalette to get as many colors 
      # from palette as there are features
      features_palette <- colorRampPalette(colors)(length(groups))
    }
    
    # 2. Define axes limits ----------------------------------------------------
    # The minimal and maximal of the values to make the x and y scales the same.
    # If limits are not defined (default behavior), use the min and max of cell
    # coordinates in the current object
    if (is.null(xlim) | is.null(ylim)){
      # Pull reduction coordinates if either xlim or ylim are NULL
      reduction_coords <-
        SCUBA::fetch_reduction(
          object,
          reduction = reduction,
          dims = c(1, 2)
          )
        }
    
    if (is.null(xlim)){
      xmin <- min(reduction_coords[,1])
      xmax <- max(reduction_coords[,1])
    } else {
      xmin <- xlim[1]
      xmax <- xlim[2]
    }
    
    if (is.null(ylim)){
      ymin <- min(reduction_coords[,2])
      ymax <- max(reduction_coords[,2])
    } else {
      ymin <- ylim[1]
      ymax <- ylim[2]
    }
    
    # Create one plot for each group (feature)
    plots <- list()
    
    # 3. Determine legend limits -----------------------------------------------
    # If scales are the same across plots, gather unified legend limits
    if (share_scale == TRUE){
      # Min vector: minimum data values for each feature selected
      min_vector <-
        sapply(
          groups,
          function(group, object){
            feature_data <-
              SCUBA::fetch_data(
                object,
                # Fetch data for group (feature)
                vars = group,
                # Slot: will use "data" for Seurat objects and "logcounts" 
                # for SingleCellExperiment objects
                layer = SCUBA::default_layer(object)
                )[,1]
            # Return minimum value of data
            min(feature_data)
          },
          object
        ) 
      
      # Gather max data values for each feature
      max_vector <-
        sapply(
          groups,
          function(group, object){
            feature_data <-
              SCUBA::fetch_data(
                object,
                # Fetch data for group (feature)
                vars = group,
                # Slot: will use "data" for Seurat objects and "logcounts" 
                # for SingleCellExperiment objects
                layer = SCUBA::default_layer(object)
              )[,1]
            # Return minimum value of data
            max(feature_data)
          },
          object
        ) 
      
      # Take the minimum and maximum values for 
      legend_min <- min(min_vector)
      legend_max <- max(max_vector)
    }
    
    # 4. Iterative plot creation -----------------------------------------------
    for (group in groups) {
      i <- which(groups == group)
      
      # 4.1. Define legend title for group
      # plot_legend_title, as opposed to `legend_title` (parameter passed)
      plot_legend_title <- 
        if (legend_title == "feature"){
          # Remove assay key from feature name 
          hr_name(
            machine_readable_name = group,
            assay_config = assay_config,
            # Do not use suffix for legend title
            use_suffix = FALSE
            )
        } else if (legend_title == "assay_score"){
          # Assay score: short description of what is being measured in the 
          # assay (expression, enrichment, etc.). Will be set in config file
          "Expression"
        } else if (legend_title == "none"){
          NULL
        } 
      
      # 4.2. Create plot 
      plot <- 
        cellDIVER::plot_feature(
          object, 
          features = group, 
          reduction = reduction,
          label_by = label_by,
          ...
          ) +
        # List of layers to apply to plot: depends on settings specified
        coord_cartesian(
          xlim = c(xmin, xmax),
          ylim = c(ymin, ymax)
        ) +
        theme(
          plot.title = 
            element_text(size = 14, face = "bold")
        ) 
      
      # Legend scale: applied to each plot in all cases (as opposed to being
      # applied to all plots after Patchwork object is created)
      suppressMessages(
        plot <-
          plot +
          scale_color_gradientn(
            plot_legend_title, 
            # colors: use `colors` argument, or the ith color of the 
            # features_palette if `color_by_feature` == TRUE
            colors = 
              if (color_by_feature == TRUE){
                # Gradient goes between light gray and the 
                # individual palette color
                c("#E1E1E1", features_palette[i])
              } else {
                colors
              }, 
            # If share_scale == FALSE, apply default
            # (min and max for current panel)
            limits = if (share_scale == TRUE) c(legend_min, legend_max)
          )
      )
      
      # Title: use custom titles if defined, otherwise use the group name
      if (show_title == TRUE){
        # If show_title is TRUE, use either default or custom titles
        if (is.null(custom_titles)){
          # custom_titles is NULL: use default behavior (group names)
          plot <-
            plot +
            ggtitle(
              # Remove assay key from feature name
              hr_name(
                # (group is the name of the individual feature)
                machine_readable_name = group,
                assay_config = assay_config
                )
              )
        } else {
          # custom_titles is not NULL: use custom titles 
          # (custom titles should be a character vector)
          plot <-
            plot +
            ggtitle(custom_titles[i])
        }
      } else {
        # If show_title is FALSE, remove titles
        plot <-
          plot +
          ggtitle(NULL)
      }
      
      plots[[group]] <- plot
    }
    
    plot <- 
      wrap_plots(
        plots, 
        ncol = ncol, 
        # `guides`:
        # collect: gather scales from all plots. 
        # keep: show a scale beside each plot.
        guides = if (share_scale == TRUE) "collect" else "keep"
      ) 
    
    if (share_scale == TRUE){
      suppressMessages(
        plot <- 
          plot &
          scale_color_gradientn(
            # Text to display for legend title
            if (legend_title == "assay_score") "expression" else NULL,
            colors = colors,
            limits = if (share_scale == TRUE) c(legend_min, legend_max)
            )
        )
    }
    
    plot <-
      suppressMessages(
        plot & 
          theme(
            legend.position =
              # Place legend at right if show_legend == TRUE,
              # remove legend otherwise
              if (show_legend == TRUE) "right" else "none"
          )
      )
    
    # If super_title is TRUE, add feature name above all panels.
    # Can add different behavior for super_title in the future
    
    # if (super_title == TRUE){
    #   plot <-
    #     plot +
    #     plot_annotation(
    #       title = feature,
    #       theme = 
    #         theme(
    #           plot.title = 
    #             element_text(
    #               face = "bold",
    #               hjust = 0.5,
    #               size = 16
    #             )
    #         )
    #     ) 
    # }
    
    return(plot)
  }