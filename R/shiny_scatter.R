#' shiny_scatter
#'
#' Creates a flow-like scatterplot using the Seurat FeatureScatter function and 
#' modifies the plot post-creation according to user selections.
#'
#' @param object A Seurat object. May be the full object or a subset. This is 
#' a reactive-agnostic parameter (can be either reactive or non-reactive).
#' @param feature_1 The feature to plot on the x-axis. This should be a 
#' single-length character vector. If a named vector, the name will be used as 
#' the display name of the feature on the plot.
#' @param feature_2 The feature to plot on the y-axis. This should be a 
#' single-length character vector. If a named vector, the name will be used as 
#' the display name of the feature on the plot.
#' @param group_by Metadata category to be passed to the group_by argument 
#' (affecting how cells are colored on the plot).
#' @param display_coeff If TRUE, show the correlation coefficient 
#' as the title of the plot (default behavior).
#' @param assay_config the assays section of the config file, loaded at app 
#' startup and upon changing datasets.
#' @param legend_ncol The number of columns for keys in the legend 
#' (uses ggplot2 defaults if NULL).
#' @param legend_font_size The font size to use for legend keys 
#' (uses ggplot2 defaults if NULL).
#' @param legend_key_size The size of the key glpyhs in the legend 
#' (uses ggplot2 defaults if NULL).
#'
#' @return a ggplot2 object with a scatterplot created from the Seurat object according to user specifications.
#' 
#' @noRd
shiny_scatter <- function(object,
                          feature_1,
                          feature_2,
                          group_by,
                          show_legend,
                          display_coeff,
                          palette,
                          assay_config = NULL,
                          legend_ncol = NULL,
                          legend_font_size = NULL,
                          legend_key_size = NULL
                          ){
  # Palette: must determine number of colors to create from provided palette
  # The number of colors is equal to the number of unique values in 
  # the group.by category
  n_colors <- 
    cellDIVER:::n_unique(
      object,
      meta_var = group_by
      )
  
  # Plot: uses FeatureScatter
  plot <- 
    cellDIVER::plot_scatter(
      object, 
      feature_1 = feature_1,
      feature_2 = feature_2,
      group_by = group_by,
      # Cols: use user defined palette, or the defaults if palette() == NULL 
      cols = 
        if (!is.null(palette)){
          # colorRampPalette() extends or contracts the given palette to 
          # produce exactly the required number of colors
          colorRampPalette(palette)(n_colors)
          # Use ggplot2 defaults if palette() is unspecified
        } else NULL, 
      )
  
  # Display names for features on plot 
  # Set display names if feature_1 and feature_2 are named. If not, the raw 
  # feature name with the key of the associated modality will be used
  if (!is.null(names(feature_1))){
    plot <- 
      plot +
      xlab(names(feature_1))
    }
  
  if (!is.null(names(feature_2))){
    plot <- 
      plot +
      ylab(names(feature_2))
    }
  
  # List of layers to be applied to plot after creation
  layers <- 
    # c() will combine lists of elements into a single list
    # (use of individual lists works best with conditional framework)
    c(
      # Elements A-C: theme() elements
      list(
        do.call(
          theme,
          args = 
            c(
              list(
                # Element A: Legend position
                # "right" if a legend is desired, and "none" if not
                legend.position = 
                  if (show_legend == TRUE) {
                    "right"
                  } else "none"
              ),
              
              # Element B: Legend font size 
              if (isTruthy(legend_font_size)){
                list(
                  legend.text = 
                    element_text(
                      size = legend_font_size
                    )
                )
              },
              
              # Element C: Legend key size (passed here as well as in
              # guides())
              if (isTruthy(legend_key_size)){
                list(
                  legend.key.size =
                    unit(legend_key_size, "points")
                )
              }
            )
        )
      ),
      
      # Elements D-E: specified with guides()
      list(
        guides(
          # Guide for scatterplot is "color"
          color = 
            do.call(
              guide_legend,
              # List of arguments to call
              args =
                c(
                  # Empty list: passes no arguments if none are specified
                  list(),
                  # Element D: Number of columns in legend
                  if (isTruthy(legend_ncol)){
                    list(
                      ncol = legend_ncol
                    )
                  },
                  # Element E: Legend key size (specified here and in theme())
                  if (isTruthy(legend_key_size)){
                    list(
                      override.aes =
                        list(
                          size = legend_key_size
                        )
                    )
                  }
                )
            )
        )
      ),
      
      # Element F: Remove title if requested
      if (display_coeff == FALSE){
        list(
          labs(title = NULL)
        )
      }
    )
  
  # Add layers to plot
  plot <- 
    plot & 
    layers
  
  # Return finished plot
  plot
}
