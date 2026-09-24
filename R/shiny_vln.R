#' shiny_vln
#'
#' Outputs a violin plot based on inputs defined in the plot_module.
#'
#' @param object a Seurat object. This can be either the full object or a subset.
#' @param features_entered Features to plot.
#' @param group_by the group by metadata variable defined by the user.
#' @param split_by the split by metadata variable defined by the user.
#' @param show_legend if TRUE, a legend is shown to the right of the plot. If 
#' FALSE, the legend is hidden (default is TRUE).
#' @param ncol number of columns, as specified by user
#' @param palette The palette to use for coloring groups. If the palette 
#' passed to this function is NULL, the default (hue_pal()) is used.
#' @param sort_groups the order with which to sort groups on the dot plot. This 
#' may be set to "ascending", "descending", or "custom". If ascending, groups 
#' will be sorted in increasing alphabetical order. If descending, they will 
#' be sorted in decreasing alphabetical order. If custom, groups will be 
#' sorted according to how they appear in `custom_factor_levels`.
#' @param set_title If defined, the titles of each facet on the plot will be
#' set to the values defined in the character vector passed. If NULL, the 
#' defaults according to cellDIVER::plot_ridge will be used. 
#' @param custom_factor_levels A character vector giving the order of groups 
#' if `sort_groups` is set to "custom".
#' @param legend_ncol The number of columns for keys in the legend (uses 
#' ggplot2 defaults if NULL).
#' @param legend_font_size The font size to use for legend keys (uses 
#' ggplot2 defaults if NULL).
#' @param legend_key_size The size of the key glpyhs in the legend (uses 
#' ggplot2 defaults if NULL).
#'
#' @return a ggplot2 object with a violin plot created according to user 
#' specifications.
#'
#' @noRd
shiny_vln <-
  function(
    object,
    features_entered,
    group_by,
    split_by,
    show_legend,
    ncol,
    palette,
    sort_groups = NULL,
    set_title = NULL,
    custom_factor_levels = NULL,
    legend_ncol = NULL,
    legend_font_size = NULL,
    legend_key_size = NULL
  ){
    # Default value of sort_groups: set to "ascending" if groups is NULL
    if (is.null(sort_groups)){
      sort_groups <- "ascending"
    }

    # At least one feature must be entered to avoid errors when computing plot
    if (length(features_entered) > 0){

      # validate will keep plot code from running if the subset
      # is NULL (no cells in subset)
      validate(
        need(
          object,
          # No message displayed (a notification is already displayed)
          # (*was displayed*)
          message = ""
        )
      )

      # Pull metadata table, then modify levels in factor for group_by data
      meta_table <-
        SCUBA::fetch_metadata(
          object,
          full_table = TRUE
        )

      # Sort group_by levels by default
      # Plot groups in ascending or descending order by group name
      meta_table[[group_by]] <-
        # factor() creates a factor if the metadata category is not a factor
        # already, and re-orders a factor if it already exists.
        factor(
          meta_table[[group_by]],
          levels =
            # If sort_groups is "ascending" or "descending", re-factor based on
            # an alphanumeric order
            if (sort_groups %in% c("ascending", "descending")){
              SCUBA::unique_values(
                object,
                var = group_by
                ) |>
                str_sort(
                  numeric = TRUE,
                  # For violin plots, groups plot from left to right in same order
                  # vector levels appear (therefore ascending should use
                  # deceasing = FALSE)
                  decreasing =
                    if (sort_groups == "ascending"){
                      FALSE
                    } else if (sort_groups == "descending"){
                      TRUE
                    }
                )
            } else if (sort_groups %in% c("custom", "expression")){
              # If sort_groups is "custom", use the user-defined levels,
              # If "expression", the custom levels will be the computed
              # levels by ascending or descending feature expression

              # If sort_groups is custom but custom_factor_levels is not
              # defined, throw an informative error message (error returned
              # by by factor() is too generic)
              if (is.null(custom_factor_levels)){
                stop(
                  'When `sort_groups` is equal to "custom",
                    `custom_factor_levels` must be defined.'
                )
              }

              # If sort_groups is "custom", use the user-defined levels
              custom_factor_levels
            }
        )

      # Save modified metadata table to object
      object <-
        cellDIVER:::update_object_metadata(
          object,
          table = meta_table
        )

      # Palette: must determine number of colors to create from provided palette
      # The number of colors is equal to the number of unique values in
      # the group.by category
      n_colors <-
        n_unique(
          object,
          meta_var = group_by
          )

      vln_plot <-
        cellDIVER::plot_violin(
          # Object or subset
          object,
          features = features_entered,
          group_by = group_by,
          # Split.by: NULL if user selects "none", otherwise equal
          # to user selection
          split_by = if (split_by == "none") NULL else split_by,
          # Cols: use user defined palette, or the defaults if palette() == NULL
          cols =
            if (!is.null(palette)){
              # colorRampPalette() extends or contracts the given palette to
              # produce exactly the required number of colors
              colorRampPalette(palette)(n_colors)
              # Use ggplot2 defaults if palette() is unspecified
            } else NULL,
          # ncol: use value of ncol defined in plot_module when more than
          # one feature is entered
          ncol = if (length(features_entered)==1) NULL else ncol
          )

      # Additional layers
      # legend font size, key size, and number of columns
      layers <-
        c(
          list(
            guides(
              # Guide for violin plot is fill
              fill =
                do.call(
                  guide_legend,
                  # List of arguments to call
                  args =
                    c(
                      # Empty list: passes no arguments if none are specified
                      list(),
                      # Element A: Number of columns in legend
                      if (isTruthy(legend_ncol)){
                        list(
                          ncol = legend_ncol
                        )
                      },
                      # Element B: Legend key size
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

          list(
            do.call(
              theme,
              # List of arguments to call with theme
              args =
                # Arguments are included in list conditionally. If no elements
                # are included, the list() call will return an empty list
                # instead of NULL (NULL will cause errors with do.call)
                c(
                  list(),
                  # Element C: Show/hide legend
                  # Legend position: "right" if a legend is desired,
                  # and "none" if not
                  list(
                    legend.position =
                      if (show_legend == TRUE) {
                        "right"
                      } else "none"
                  ),
                  # Element D: Legend font size
                  if (isTruthy(legend_font_size)){
                    list(
                      legend.text =
                        element_text(
                          size = legend_font_size
                        )
                    )
                  },

                  # Element E: Legend key size (passed here as well as guides())
                  if (isTruthy(legend_key_size)){
                    list(
                      legend.key.size =
                        unit(legend_key_size, "points")
                    )
                  }
                )
            )
          )
        )

      # Add layers to plot
      vln_plot <-
        vln_plot +
        layers
      
      # Apply plot titles, if provided
      if (!is.null(set_title)){
        for (i in 1:length(set_title)){
          suppressMessages(
            vln_plot[[i]] <-
              vln_plot[[i]] +
              ggtitle(set_title[i])
          )
        }
      }

      # Return the plot
      vln_plot
    }
  }
