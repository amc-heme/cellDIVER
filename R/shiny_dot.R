#' cellDIVER Dot Plots
#'
#' @param object a Seurat object. This can be either the full object or a subset.
#' @param features_entered a character vector giving the features to use in the
#' plot.
#' @param group_by user specified group_by metadata variable
#' @param show_legend user choice as to whether a legend should be shown (default is TRUE)
#' @param palette the color palette to use for the plot (plot uses a continuous color palette)
#' @param sort_groups the order with which to sort groups on the dot plot. This may be set to "ascending", "descending", or "custom". If ascending, groups will be sorted in increasing alphabetical order. If descending, they will be sorted in decreasing alphabetical order. If custom, groups will be sorted according to how they appear in `custom_factor_levels`.
#' @param custom_factor_levels A character vector giving the order of groups if `sort_groups` is set to "custom".
#' @param rename_feature_labels a character vector giving alternate names to use
#' for the features plotted on the x-axis. The length of this vector must match
#'  the length of features_entered.
#'
#' @return a ggplot2 object with a dot plot created according to user specifications.
#'
#' @noRd
shiny_dot <-
  function(
    object,
    features_entered,
    group_by,
    show_legend = TRUE,
    palette = NULL,
    sort_groups = NULL,
    custom_factor_levels = NULL,
    rename_feature_labels = NULL
    ){
    # Default value of sort_groups: set to "ascending" if groups is NULL
    if (is.null(sort_groups)){
      sort_groups <- "ascending"
    }

    if (length(features_entered) > 0){
      # Ordering of groups in group_by category
      # Plot groups in ascending or descending order by group name

      # Pull metadata table, then modify levels in factor for group_by data
      meta_table <-
        SCUBA::fetch_metadata(
          object,
          full_table = TRUE
          )

      meta_table[[group_by]] <-
        # factor() creates a factor if the metadata category is not a factor
        # already, and re-orders a factor if it already exists.
        factor(
          meta_table[[group_by]],
          levels =
            # If sort_groups is "ascending" or "descending", re-factor based on
            # an alphanumeric order
            if (sort_groups %in% c("ascending", "descending")){
              meta_table[[group_by]] |>
                unique() |>
                str_sort(
                  numeric = TRUE,
                  # DotPlot plots levels in inverse order to their order in the
                  # factor (first level appears on the bottom of the plot, and
                  # last level appears on the top)
                  decreasing =
                    if (sort_groups == "ascending"){
                      TRUE
                    } else if (sort_groups == "descending"){
                      FALSE
                    }
                )
            } else if (sort_groups %in% c("custom", "expression")){
              # If sort_groups is "custom", use the user-defined levels,
              # If "expression", the custom levels will be the computed
              # levels by ascending or descending feature expression

              # Error message when custom_factor_levels is not defined (error
              # returned by factor() is too generic)
              if (is.null(custom_factor_levels)){
                stop(
                  '`custom_factor_levels` is undefined, and sort_groups ',
                  'is set to either "custom" or "expression". When `sort_groups` ',
                  'is equal to "custom", `custom_factor_levels` ',
                  'must be defined. When set to "expression", (in cellDIVER) ',
                  '`custom_factor_levels` is supposed be defined in the parent ',
                  'plot module.'
                  )
                }

              custom_factor_levels |>
                # reverse order for dot plots (DotPlot uses the inverse
                # of the order)
                rev()
            }
          )

      # Save refactored metadata table to object
      object <-
        cellDIVER:::update_object_metadata(
          object,
          table = meta_table
          )

      # Create plot if at least one feature is passed to shiny_dot()
      plot <-
        cellDIVER::plot_dot(
          # Seurat object or subset
          object,
          features = features_entered,
          group_by = group_by
          ) +
        RotatedAxis() +
        #Legend position: "right" if a legend is desired, and "none" if not
        theme(
          legend.position = if (show_legend == TRUE) "right" else "none"
          ) +
        # Rename colorbar legend to "Mean-centered average expression"
        suppressMessages(
          guides(
            `color` = guide_colorbar(title = 'Mean-centered\nAverage Expression')
          )
        )

      # If a palette is defined, apply it to the plot
      if (!is.null(palette)){
        plot <-
          #suppressMessages()
          plot +
          scale_color_gradientn(
            colors = palette
          )
      }

      # If custom feature labels are defined, apply them
      if (isTruthy(rename_feature_labels)){
        # Construct named vector with rename targets as values, and features
        # entered as names
        names(rename_feature_labels) <- features_entered

        # Rename x-axis tick labels according to named vector
        plot <-
          plot +
          scale_x_discrete(
            labels = rename_feature_labels
          )
        }

      # Return plot
      plot
    }
  }
