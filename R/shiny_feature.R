#' Interactive Feature Plots in cellDIVER
#'
#' Accepts inputs from plots_selections module and outputs a Seurat FeaturePlot 
#' from the Seurat object passed to it. 
#'
#' @param object A Seurat object. This can be either the full object or a subset.
#' @param features_entered  a character vector giving the features entered by 
#' the user.
#' @param assay_config the assays section of the config file, loaded at app 
#' startup and upon changing datasets.
#' @param split_by user-specified metadata variable for splitting plots.
#' @param raw_feature_names a reactive variable indicating whether to remove 
#' the modality key in the config file from feature names. When FALSE, the 
#' modality key is removed, and when TRUE, it is retained. For example, a 
#' feature from an assay/modality named "protein" will display as "protein_CD4" 
#' instead of "CD4 (Surface Protein)" when raw_feature_names is TRUE.
#' @param group_by the metadata column to use for showing labels on the plot, 
#' if labels are enabled.
#' @param blend Whether to use blended feature plots for feature co-expression. 
#' Must enter exactly two features to use this setting.
#' @param order When TRUE, plot each cell in order of expression, so the cells 
#' with the highest expression are plotted last and appear in front of cells 
#' with lower expression.
#' @param show_label User choice as to whether labels should be shown on the plot.
#' @param show_legend User choice as to whether a legend should be shown.
#' @param is_subset Reactive boolean value stating whether the object is a subset
#' @param original_limits User choice as to whether original axes limits should 
#' be used
#' @param xlim_orig The original x limits for the plot, computed from the full 
#' object for each reduction enabled. 
#' @param ylim_orig The original x limits for the plot, computed from the full 
#' object for each reduction enabled. 
#' @param show_title Whether to show a title above the plot
#' @param super_title A string passed to this variable will appear above all 
#' panels in the plot, when plotting a feature with a split_by category.
#' @param share_scale For plots with multiple features, whether to keep the 
#' limits on the color scale for plotting expression consistent across features.
#' @param color_by_feature For plots with multiple features. when TRUE, use 
#' separate colors to plot each feature (derived from the supplied palette).
#' @param custom_titles A character vector used for the title of a single 
#' feature plot with no split by category, or the titles of a single feature 
#' plot with a split by category, or a multi-feature plot with no split by 
#' category. This currently does not work for multi-feature plots with a 
#' split_by category and blended feature plots.
#' @param legend_title A string to display as the title of the legend. Does not 
#' work for blended feature plots.
#' @param ncol Number of columns to use for displaying a single-feature plot. 
#' This does not work for multi-feature plots with a split by category.
#' @param palette A color palette to use for plotting expression values. This 
#' is ignored for blended feature plots.
#' @param blend_palette a two-color palette to use for blended feature plots.
#' @param blend_layout for blended plots, sets whether the plot displays in a 
#' two-column or four-column layout.
#' @param reduction the reduction to show for feature expression.
#'
#' @return a ggplot2 object with a feature plot created according to user
#' specifications.
#' 
#' @noRd
shiny_feature <- function(object, 
                          features_entered, 
                          assay_config, 
                          split_by, 
                          raw_feature_names,
                          group_by = NULL,
                          blend = FALSE, 
                          order = FALSE, 
                          show_label = FALSE, 
                          show_legend = TRUE, 
                          is_subset = FALSE, 
                          original_limits = FALSE, 
                          xlim_orig = NULL, 
                          ylim_orig = NULL,
                          show_title = TRUE, 
                          super_title = FALSE,
                          share_scale = FALSE,
                          color_by_feature = FALSE,
                          custom_titles = NULL,
                          legend_title = NULL,
                          ncol = NULL, 
                          palette = NULL,
                          blend_palette = NULL,
                          blend_layout = NULL,
                          reduction = NULL
){
  # Validate will keep plot code from running if the object is undefined
  validate(
    need(
      object,
      # A notification is already displayed in the app if the object is NULL
      message = ""
    )
  )
  
  # Also throw an error when a subset only contains one cell
  validate(
    need(
      length(Cells(object)) != 1,
      message = "Feature plot error: can't plot subsets with only one cell."
      )
    )
  
  # Throw a silent exception if both share_scale and color_by_feature are TRUE
  # This combination produces an undesirable plot
  # The combination is automatically corrected in the plot_module server, but 
  # the plot produced by these settings will display briefly before this happens
  validate(
    need(
      !(share_scale == TRUE & color_by_feature == TRUE),
      # Display loading message until settings are corrected
      message = "Loading plot, please wait..."
    )
  )
  
  if (length(features_entered) == 1){
    # Single-feature plots ####
    # Use FeaturePlotSingle.R 
    feature_plot <-
      FeaturePlotSingle(
        object,
        feature = features_entered,
        split_by = if (split_by != "none") split_by else NULL,
        # Labels for groups: passed to plot_feature
        label_by = group_by,
        # Colors: set to colors passed in the palette. 
        # If NULL, Seurat defaults are used
        colors = if (!is.null(palette)) palette,
        show_title = show_title,
        # Use custom titles if defined
        custom_titles = if (!is.null(custom_titles)) custom_titles else NULL,
        legend_title = legend_title,
        super_title = super_title,
        ncol = ncol,
        reduction = 
          if (!is.null(reduction)) reduction else NULL,
        xlim = 
          if (isTruthy(original_limits)){
            xlim_orig
          } else NULL,
        ylim = 
          if (isTruthy(original_limits)){
            ylim_orig
          } else NULL,
        show_legend = show_legend,
        # `...` arguments passed to plot_feature
        label = show_label,
        order = order,
        assay_config = assay_config
        )
    
    # cellDIVER-specific modification of ggtitle for single feature plots
    # with a split-by variable 
    # *** DO NOT MOVE THIS TO scPLOTS ***
    if (split_by != "none" & super_title == TRUE){
      # In the case where a split-by variable is defined, custom_titles modifies
      # the facet labels for the split-by levels, not the overall title with the
      # feature name (super_title). If displayed, the overall title is currently 
      # always set to the "raw" feature name with the modality key. The modality 
      # key must be removed if raw_feature_names is FALSE.
      
      # For the plotting package, we should consider setting the super_title to
      # a value instead of TRUE/FALSE, so the overall title can be modified.
      if (raw_feature_names == FALSE){
        feature_display_name <-
          hr_name(
            machine_readable_name = features_entered,
            assay_config = assay_config,
            use_suffix = TRUE
            )
        
        feature_plot <-
          feature_plot +
          plot_annotation(
            title = feature_display_name
            )
      } else {
        # Do nothing if raw_feature_names is TRUE
      }
    }
    
    # Return single-feature feature plot
    feature_plot
  } else if (
    # Multi-feature plots ####
    # (when there are multiple features, or 2 with blend not enabled)
    (length(features_entered) == 2 & !isTruthy(blend))|
    (length(features_entered) > 2)
    ){
    # Function used for plot depends on split_by choice
    if (split_by == "none"){
      ## Multi-feature plots, no split by variable ####
      # Use MultiFeatureSimple from featurePlotWrapper.R
      MultiFeatureSimple(
        object,
        features = features_entered,
        label_by = group_by,
        # Colors: set to colors passed in the palette. 
        # If NULL, Seurat defaults are used
        colors = if (!is.null(palette)) palette,
        color_by_feature = color_by_feature,
        show_title = show_title,
        # Use custom titles if defined
        custom_titles = if (!is.null(custom_titles)) custom_titles else NULL,
        share_scale = share_scale,
        legend_title = legend_title,
        ncol = ncol,
        reduction = 
          if (!is.null(reduction)) reduction else NULL,
        xlim = 
          if (isTruthy(original_limits)){
            xlim_orig
          } else NULL,
        ylim = 
          if (isTruthy(original_limits)){
            ylim_orig
          } else NULL,
        show_legend = show_legend,
        # `...` arguments passed to FeaturePlot
        label = show_label,
        order = order,
        assay_config = assay_config
      )
    } else {
      ## Multi-feature plots, with split_by variable ####
      # Use FeaturePlot function (SCUBA/scPlots)
      feature_plot <-
        cellDIVER::plot_feature(
          # Object or subset
          object,
          features = features_entered,
          split_by = if (split_by != "none") split_by else NULL,
          # Use the group_by variable for labeling
          label_by = group_by,
          # ncol: valid when split_by is not defined
          ncol = ncol,
          # Order: whether to plot cells in order by expression
          # Set to FALSE if undefined
          order = if (is.null(order)) FALSE else order,
          # Show/hide cluster labels
          label = show_label,
          # Reduction: uses the input for reduction if it exists, otherwise
          # it is set to NULL and will use default settings.
          reduction = if (!is.null(reduction)) reduction else NULL
          )
      
      #+
      # Clean up title: this changes the feature names on each plot
      # to a human-readable format
      # (used when split_by == NULL)
      # Determine number of plots created
      #n_patches <- n_patches(feature_plot)
      # Iterate through each plot, correcting the title
      # feature_plot <-
      #   hr_title(
      #     feature_plot,
      #     n_patches,
      #     assay_config
      #     )
      
      # Modify plot after creation with ggplot layers according
      # to user input
      # 'layers' is a list of layers that is applied to the plot
      layers <-
        c(list(
          # Element A
          # Legend position: "right" if a legend is desired,
          # and "none" if not
          theme(
            legend.position =
              if (show_legend == TRUE) {
                "right"
              } else "none")),
          
          # Elements B-C. Axis limits:
          # Use limits from full dataset if specified.
          # The conditional is tied to a reactive value
          # instead of the input to avoid an error that
          # occurs when this function is evaluated
          # before the input is #defined.
          # First, simultaneously test if subset is
          # present and if the corresponding
          # original_limits reactive is truthy
          # (i.e. both present and checked).
          if (is_subset & isTruthy(original_limits)){
            # If so, add original limits to the list
            list(
              scale_x_continuous(limits = xlim_orig),
              scale_y_continuous(limits = ylim_orig)
            )
          },
          
          # Element D: Custom colors
          # If a palette is entered, use the palette for the plot.
          # The palette may be a continuous palette selected in the plot tab,
          # or it may be a pair of user-defined custom colors.
          if (isTruthy(palette)){
            scale_color_gradientn(
              colors = c(palette)
            )
          }
        )
      
      # Modify the plot using the layers defined above
      suppressMessages(
        feature_plot <-
          feature_plot &
          layers
      )
      
      # Return finished plot for display
      feature_plot
    }
  } else if (length(features_entered) == 2 & isTruthy(blend)){
    # Blended feature plot ####
    
    # Palette for blending: fill to red and blue if NULL
    # "lightgrey" is the Seurat default color used when neither feature is expressed 
    if (!isTruthy(blend_palette)){
      blend_palette <- c("lightgrey", "#FF0000", "#0000FF")
    }
    
    # Use Seurat::FeaturePlot
    feature_plot <-
      cellDIVER::plot_feature(
        # Object or subset
        object,
        features = features_entered,
        cols = blend_palette,
        blend = blend,
        split_by = if (split_by != "none") split_by else NULL,
        # Use the group_by variable for labeling groups
        label_by = group_by,
        # ncol: valid when split_by is not defined
        ncol = ncol,
        # Order: whether to plot cells in order by expression
        # Set to FALSE if undefined
        order = if (is.null(order)) FALSE else order,
        # Show/hide cluster labels
        label = show_label,
        # Reduction: uses the input for reduction if it exists, otherwise
        # it is set to NULL and will use default settings.
        reduction = if(!is.null(reduction)) reduction else NULL
        )

    # Modify plot after creation with ggplot layers according
    # to user input
    # For blended feature plots, the only layer to add is original axes limits.
    
    # There is no legend on blended feature plots, so legend position element 
    # is not shown
    
    ## A. Axis limits: use limits from full dataset if specified. ####
    # Test if subset is present and if the corresponding original_limits 
    # reactive is truthy (i.e. both present and checked).
    if (is_subset & isTruthy(original_limits)){
      # If so, adjust plot to use x- and y- axis limits from the full object
      feature_plot <-
        feature_plot &
        theme(
          scale_x_continuous(limits = xlim_orig),
          scale_y_continuous(limits = ylim_orig)
        )
    }
    
    ## B. split plot Layout: 2x2 or the Seurat default (4 column layout) ####
    if (isTruthy(blend_layout)){
      if (blend_layout == "2col"){
        # Design: bounds of first three panels and legend
        feature_plot <-
          feature_plot +
          plot_layout(
            nrow = 2,
            ncol = 2
          )
        }
      # For a four column layout, no change is necessary.
    }
    
    ## C. Default titles for blended feature plots ####
    # Feature title formula for the four facets created: 
    # first feature, second feature, "coexpression", and "coexpression scale"
    
    # Form display names for features based on whether 
    # raw_feature_names is selected
    # feature_display_names <-
    #   if (raw_feature_names == FALSE){
    #     
    #   } else {
    #     features_entered
    #   }
    
    feature_display_names <-
      sapply(
        1:length(features_entered),
        function(i){
          if (raw_feature_names == FALSE){
            hr_name(
              machine_readable_name =
                features_entered[i],
              assay_config = assay_config,
              # Use the assay label if provided in
              # config app
              use_suffix = TRUE
            )
          } else {
            features_entered[i]
          }
        }
      )
    
    # Apply titles 
    for (i in 1:4){
      if (i < 3){
        # Panel 1, 2: features entered (human-readable name)
        feature_plot[[i]] <-
          feature_plot[[i]] +
          ggtitle(
            feature_display_names[i]
          )
      } else if (i == 3){
        # Panel 3: blend panel
        feature_plot[[i]] <-
          feature_plot[[i]] +
          ggtitle("Coexpression")
      } else if (i == 4){
        # Panel 4: legend title 
        # X- and Y- axis labels: human-readable name of features entered
        feature_plot[[i]] <-
          feature_plot[[i]] +
          ggtitle("Coexpression Scale") +
          labs(subtitle = "(Blend threshold: 0.5)") +
          xlab(
            feature_display_names[1]
            ) +
          ylab(
            feature_display_names[2]
          ) +
          theme(
            plot.title = 
              element_text(
                hjust = 0.5, 
                size = 12,
                margin = margin(0, 0, 0, 0, unit = "pt")
                ),
            plot.subtitle = 
              element_text(
                hjust = 0.5, 
                size = 9, 
                face = "italic", 
                margin = margin(0, 0, 5, 0, unit = "pt")
                )
          )
      }
    }
    
    # layers <- c()
    # Modify the plot using the layers defined above
    # suppressMessages(
    #   feature_plot <-
    #     feature_plot &
    #     layers
    # )
    
    # Return finished plot for display
    feature_plot
  }
}
