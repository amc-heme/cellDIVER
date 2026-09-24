#' cellDIVER ridge plots
#'
#' @param object a Seurat object or subset.
#' @param features_entered a character vector giving the features to use in the
#' plot.
#' @param group_by user specified group_by metadata variable
#' @param show_legend user choice as to whether a legend should be shown 
#' (default is TRUE)
#' @param palette the color palette to use for the plot (plot uses a continuous 
#' color palette)
#' @param center_x_axis_title if TRUE, the title on the x_axis is centered 
#' (default is FALSE).
#' @param xlim a 2-element character vector giving the lower and upper bounds
#' of the x-axis.
#' @param sort_groups the order with which to sort groups on the dot plot. This 
#' may be set to "ascending" or "descending". If ascending, groups will be 
#' sorted in increasing alphabetical order. If descending, they will be sorted 
#' in decreasing alphabetical order.
#' @param custom_factor_levels A character vector giving the order of groups 
#' if `sort_groups` is set to "custom".
#' @param set_title If defined, the titles of each facet on the plot will be
#' set to the values defined in the character vector passed. If NULL, the 
#' defaults according to cellDIVER::plot_ridge will be used. 
#' @param legend_ncol The number of columns for keys in the legend (uses 
#' ggplot2 defaults if NULL).
#' @param legend_font_size The font size to use for legend keys (uses ggplot2 
#' defaults if NULL).
#' @param legend_key_size The size of the key glpyhs in the legend (uses ggplot2 
#' defaults if NULL).
#'
#' @return a ggplot2 object with a ridge plot created according to 
#' user specifications.
#'
#' @noRd
shiny_ridge <-
  function(
   object,
   features_entered,
   group_by = "none",
   show_legend = TRUE,
   palette = NULL,
   center_x_axis_title = FALSE,
   xlim = NULL,
   sort_groups = NULL,
   custom_factor_levels = NULL,
   set_title = NULL,
   custom_titles = NULL,
   assay_config = NULL,
   legend_ncol = NULL,
   legend_font_size = NULL,
   legend_key_size = NULL
  ){
    # validate will keep plot code from running if the subset
    # is NULL (no cells in subset), or if the group by selection and features
    # are not yet defined.
    validate(
      need(
        isTruthy(object) & isTruthy(group_by) & isTruthy(features_entered),
        # No message displayed (a notification is already
        # displayed) (*was displayed*)
        message = ""
      )
    )

    # If group_by is equal to none, add a dummy metadata column that labels
    # all cells with... "All Cells"
    if (group_by == "none"){
      # Pull metadata table, modify, then save to object
      meta_table <-
        SCUBA::fetch_metadata(
          object,
          full_table = TRUE
          )

      meta_table$allcells <- "All Cells"

      object <-
        cellDIVER:::update_object_metadata(
          object,
          table = meta_table
          )
    }

    # n_colors (equal to number of groups, determines how palette is applied)
    if (group_by != "none"){
      n_colors <-
        cellDIVER:::n_unique(
          object,
          meta_var = group_by
          )
    } else {
      # Only one group exists when group_by is equal to none
      n_colors <- 1
    }

    # Factor/refactor group by metadata, but only if sort_groups is defined,
    # and group by is not "none"
    if (isTruthy(sort_groups) && group_by != "none"){
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
            # Levels based on ascending or descending order
            if (sort_groups %in% c("ascending", "descending")){
              meta_table[[group_by]] |>
                unique() |>
                str_sort(
                  numeric = TRUE,
                  decreasing =
                    # Order of plotting for ridge plots is inverted from the
                    # expected order (decreasing == TRUE will plot in ascending
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
                  'When `sort_groups` is equal to "custom",
                  `custom_factor_levels` must be defined.'
                )
              }

              # Must reverse order for ridge plots
              custom_factor_levels |>
                rev()
            }
        )

      # Save modified metadata table to object
      object <-
        cellDIVER:::update_object_metadata(
          object,
          table = meta_table
        )
    }

    # If there is at least one feature entered, create the ridge plot
    if (length(features_entered) > 0){
      plot <-
        cellDIVER::plot_ridge(
          object,
          # cols: uses user-defined categorical palette,
          # or default palette if not provided
          cols =
            if (!is.null(palette)){
              # colorRampPalette() extends or contracts the given palette to
              # produce exactly the required number of colors
              colorRampPalette(palette)(n_colors)
              # Use ggplot2 defaults if palette() is unspecified
              } else NULL,
          features = features_entered,
          group_by = if (group_by == "none") "allcells" else group_by,
          # Add toggle for same_y_lims
          #same_y_lims = same_y_lims
        )

      # If group_by is "none", remove y-axis labels (which appear redundant
      # when there is only one group for all cells)
      if (group_by == "none"){
        suppressMessages(
          plot <-
            # Use & in the event multiple plots are drawn
            # (when there are two or more features)
            plot &
            theme(
              # Disables "All Cells" on y-axis
              axis.text.y = element_blank()
            )
        )
      }

      # Additional layers
      layers <-
        c(
          # Elements A-F: arguments called via theme()
          list(
            do.call(
              theme,
              # List of arguments to call with theme
              args =
                c(
                  # Element A: Toggle legend
                  list(
                    legend.position =
                      if (show_legend == TRUE) {
                        "right"
                      } else "none",
                    # Element B: Disables "identity" label on Y-axis
                    axis.title.y = element_blank(),
                    # Element C: Centers title on plot
                    plot.title =
                      element_text(
                        hjust = 0.5
                      )
                  ),
                  # Element D: Center X-axis label
                  if (center_x_axis_title == TRUE){
                    list(
                      axis.title.x =
                        element_text(
                          hjust = 0.5
                        )
                    )
                  },

                  # Element E: Legend font size
                  if (isTruthy(legend_font_size)){
                    list(
                      legend.text =
                        element_text(
                          size = legend_font_size
                        )
                    )
                  },

                  # Element F: Legend key size (passed here as well as in
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

          # Element G: Number of columns in legend
          # Called via guides()
          list(
            guides(
              # Guide for ridgeplot is fill
              fill =
                do.call(
                  guide_legend,
                  # List of arguments to call
                  args =
                    c(
                      # Empty list: passes no arguments if
                      # below values are NULL
                      list(),
                      # Number of columns in legend
                      if (isTruthy(legend_ncol)){
                        list(
                          ncol = legend_ncol
                        )
                      },
                      # Legend key size (set here as well as with theme())
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

          # Element I: Manually defined x-axis limits
          if (!is.null(xlim)){
            # scale_x_continuous(
            #   limits = xlim
            #   )
            coord_cartesian(
              xlim = xlim
              )
          }
          )

      # Apply layers
      suppressMessages(
        plot <-
          plot &
          layers
      )

      # Apply plot titles, if provided
      if (!is.null(set_title)){
        for (i in 1:length(set_title)){
          suppressMessages(
            plot[[i]] <-
              plot[[i]] +
              ggtitle(set_title[i])
            )
          }
        }

    } else {
      # Return nothing if no features are entered
      plot <- NULL
    }

    # Return plot
    plot
  }
