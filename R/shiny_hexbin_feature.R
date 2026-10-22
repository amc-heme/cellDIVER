#' Shiny Wrapper for Hexbin Feature Plots
#'
#' Creates hexbin feature plots using hextant::plot_hextant_feature(), with
#' styling and parameters consistent with cellDIVER's plotting system. This
#' function visualizes feature expression aggregated within hexagonal bins of
#' dimension reduction space, reducing visual clutter from overlapping cells
#' in dense datasets.
#'
#' @param object A SCUBA-supported single-cell object (Seurat,
#'   SingleCellExperiment, or other supported format). This can be either the
#'   full object or a subset. This is a reactive-agnostic parameter (can be
#'   either reactive or non-reactive).
#' @param features_entered A character vector giving the features (genes,
#'   proteins, etc.) to plot. Can be a single feature or multiple features.
#'   When multiple features are provided, separate plots are created for each.
#' @param nbins The number of bins partitioning the range of the first
#'   component of the chosen dimension reduction. Higher values create more
#'   granular hexagons. Default is 80.
#' @param action Aggregation method for expression within bins. One of "mean"
#'   (default), "median", or "sum". This determines how expression values from
#'   multiple cells within a bin are combined.
#' @param reduction A string indicating the reduced dimension result to use
#'   (e.g., "UMAP", "PCA", "TSNE"). If NULL, uses the object's default
#'   reduction.
#' @param split_by A string naming a metadata variable to split the plot by.
#'   If "none" or NULL, no splitting is performed. When provided, creates
#'   separate faceted plots for each unique value in the metadata variable.
#' @param ncol Number of columns for layout. Used for both faceting (when
#'   split_by is provided) and for arranging multiple feature plots. If NULL,
#'   ggplot2 determines the layout automatically.
#' @param scales Should scales be "fixed" (default, same for all facets),
#'   "free" (vary across facets), "free_x", or "free_y"? Only applies when
#'   split_by is used.
#' @param show_title Logical indicating whether to display titles. If FALSE,
#'   titles are removed regardless of custom_titles value.
#' @param custom_titles A character vector used for the title of a single
#'   feature plot with no split by category, or the titles of a single feature
#'   plot with a split by category, or a multi-feature plot with no split by
#'   category. When NULL, default titles are generated as "Feature (action)".
#' @param show_legend Logical indicating whether to display the color scale
#'   legend. Default is TRUE.
#' @param is_subset Logical value stating whether the object is a subset.
#'   Used to determine if original axis limits should be applied.
#' @param original_limits Logical indicating whether original axis limits
#'   from the full dataset should be used when plotting a subset.
#' @param xlim_orig The original x-axis limits for the plot, computed from the
#'   full object at app startup. Only used when is_subset = TRUE and
#'   original_limits = TRUE.
#' @param ylim_orig The original y-axis limits for the plot, computed from the
#'   full object at app startup. Only used when is_subset = TRUE and
#'   original_limits = TRUE.
#' @param assay Name of assay to use. If NULL, uses the object's default/
#'   active assay.
#' @param layer Name of assay layer to use (e.g., "data", "counts",
#'   "scale.data"). If NULL, uses the object's default layer for the specified
#'   assay.
#' @param palette A character vector of hex color codes for the continuous 
#'   color scale. When NULL, cellDIVER's default continuous palette
#'   (lightgrey to blue, matching Feature and Dot plots) is used. hextant's
#'   viridis scale is always replaced.
#' @param diverging_palette A 3-color diverging palette (low, mid, high) for 
#'   use with scale_fill_gradient2() with a fixed midpoint at 0. Takes 
#'   precedence over palette when provided.
#' @param data_cutoffs optional vector of length two giving minimum and
#'   maximum cutoffs applied to each cell's value *before* cells are
#'   aggregated into bins. Elements may be numbers or quantile strings such as
#'   "q99" (quantiles over all cells). Use NA for no cutoff on one side.
#' @param color_limits_quantile if TRUE, the elements of `color_limits`
#'   are probabilities (0 to 1) evaluated against the plotted values rather
#'   than values in data units.
#' @param color_limits optional numeric vector of length two giving the
#'   values at which the lowest and highest colors are placed. Values outside
#'   the limits are shown in the end colors. When NULL, the range of the
#'   plotted values is used.
#'
#' @return A ggplot2 object (or patchwork object for multiple features) with
#'   hexbin feature plot(s) created according to user specifications. The
#'   "plotted_range" attribute gives the range of the per-bin values mapped
#'   to the fill scale, and the "cell_range" attribute the range of the
#'   underlying per-cell values before any cutoffs.
#'
#' 
#' @noRd
shiny_hexbin_feature <- function(
    object,
    features_entered,
    nbins = 80,
    action = "mean",
    reduction = NULL,
    split_by = NULL,
    ncol = NULL,
    scales = "fixed",
    show_title = TRUE,
    custom_titles = NULL,
    show_legend = TRUE,
    is_subset = FALSE,
    original_limits = FALSE,
    xlim_orig = NULL,
    ylim_orig = NULL,
    assay = NULL,
    layer = NULL,
    palette = NULL,
    diverging_palette = NULL,
    color_limits = NULL,
    color_limits_quantile = FALSE,
    data_cutoffs = NULL) {
  
  # Validate that object is not NULL (will keep plot code from running if
  # the subset is NULL, meaning no cells in subset)
  validate(
    need(
      object,
      message = ""
    )
  )
  
  # Also throw an error when a subset only contains one cell
  validate(
    need(
      cellDIVER:::n_cells(object) != 1,
      message = 
        "Hexbin feature plot error: cannot plot subsets with only one cell."
    )
  )
  
  # Validate that features were entered
  validate(
    need(
      length(features_entered) > 0,
      message = "Please enter at least one feature to plot."
    )
  )
  
  # Set default reduction if not specified: the object's default reduction
  # (e.g. "umap"), since reduction names are case-sensitive
  if (is.null(reduction)) {
    reduction <- tryCatch(
      SCUBA::default_reduction(object),
      error = function(e) NULL
    )
  }
  
  validate(
    need(
      !is.null(reduction),
      message = "No dimensional reduction is available in this object."
    )
  )
  
  # Use cellDIVER's default continuous palette when none is selected, so
  # hexbin plots match the Feature and Dot plot defaults instead of
  # hextant's viridis scale
  if (is.null(palette)) {
    palette <- c("lightgrey", "blue")
  }
  
  # Range of the underlying per-cell values (before any cutoffs), reported
  # for the color scale limits slider in "data values" mode
  cell_range <-
    tryCatch(
      {
        values <-
          SCUBA::fetch_data(
            object,
            vars = features_entered,
            layer = SCUBA::default_layer(object)
          )
        values <- unlist(values[, vapply(values, is.numeric, logical(1)), drop = FALSE])
        values <- values[is.finite(values)]
        if (length(values) > 0) range(values) else NULL
      },
      error = function(e) NULL
    )
  
  # Per-cell cutoffs passed to hextant (NA means no cutoff on that side)
  cell_min_cutoff <-
    if (!is.null(data_cutoffs) && !is.na(data_cutoffs[1])) data_cutoffs[1] else NULL
  cell_max_cutoff <-
    if (!is.null(data_cutoffs) && !is.na(data_cutoffs[2])) data_cutoffs[2] else NULL
  
  # Validate reduction exists in object
  available_reductions <- tryCatch({
    reduction_names(object)
  }, error = function(e) {
    NULL
  })
  
  if (!is.null(available_reductions) && 
      !(reduction %in% available_reductions)) {
    validate(
      need(
        FALSE,
        message = 
          paste0(
            "Reduction '", 
            reduction, 
            "' not found in object. Available reductions: ",
            paste(available_reductions, collapse = ", ")
          )
      )
    )
  }
  
  # Convert "none" to NULL for split_by
  if (!is.null(split_by) && split_by == "none") {
    split_by <- NULL
  }
  
  # Create dimension reduction dimnames for use_dims parameter
  # hextant expects dimension indices, always use first two dimensions
  use_dims <- c(1, 2)
  
  #### Single Feature ####
  # If plotting a single feature, create a single plot
  if (length(features_entered) == 1) {
    
    # Determine title to use
    # If custom_titles is provided, use it; otherwise generate default
    if (isTruthy(custom_titles)) {
      plot_title <- custom_titles
    } else if (show_title) {
      # Generate default title: "Feature (action)"
      plot_title <- paste0(features_entered[1], " (", action, ")")
    } else {
      plot_title <- NULL
    }
    
    # Create the hexbin feature plot using hextant
    plot <- hextant::plot_hextant_feature(
      obj = object,
      feature = features_entered[1],
      assay = assay,
      layer = layer,
      action = action,
      nbins = nbins,
      dimension_reduction = reduction,
      use_dims = use_dims,
      split_by = split_by,
      ncol = ncol,
      scales = scales,
      cell_min_cutoff = cell_min_cutoff,
      cell_max_cutoff = cell_max_cutoff,
      title = plot_title,
      xlab = paste0(reduction, "_1"),
      ylab = paste0(reduction, "_2")
    )
    
    # Color scale limits given as quantiles are evaluated against the
    # per-bin values shown
    scale_limits <-
      resolve_color_limits(
        color_limits,
        quantile = color_limits_quantile,
        values = plotted_values(plot, "feature_value")
      )
    
    # Apply additional styling layers to match cellDIVER aesthetics
    layers <- c(
      # Legend position: "right" to match cellDIVER standard, or "none" if not
      # desired
      list(
        theme(
          legend.position = if (show_legend) "right" else "none"
        )
      ),
      
      # Replace hextant's viridis fill scale with the diverging palette (if
      # provided, it takes precedence) or the global/default continuous
      # palette, applying any user-set color scale limits
      list(
        color_scale(
          "fill",
          palette =
            if (!is.null(diverging_palette)) diverging_palette else palette,
          limits = scale_limits,
          diverging = !is.null(diverging_palette)
        )
      ),
      
      # Apply original axis limits if this is a subset and user has selected
      # to use original limits
      if (is_subset & isTruthy(original_limits)) {
        list(
          scale_x_continuous(limits = xlim_orig),
          scale_y_continuous(limits = ylim_orig)
        )
      }
    )
    
    # Apply the layers to the plot. Replacing hextant's fill scale emits a
    # "Scale for fill is already present" message, which is expected.
    suppressMessages(
      plot <- plot & layers
    )
    
    # Record the range of the per-bin values shown, for the color scale
    # limits slider
    attr(plot, "plotted_range") <- plotted_range(plot, "feature_value")
    attr(plot, "cell_range") <- cell_range
    
    return(plot)
    
  } else {
    # Handle multiple feature plots
    
    # Create a list to store individual plots
    plot_list <- list()
    
    # Create a plot for each feature
    for (i in seq_along(features_entered)) {
      feature <- features_entered[i]
      
      # Determine title to use for this feature
      # If custom_titles is provided and long enough, use it; otherwise generate
      # default
      if (isTruthy(custom_titles) && length(custom_titles) >= i) {
        individual_title <- custom_titles[i]
      } else if (show_title) {
        # Generate default title: "Feature (action)"
        individual_title <- paste0(feature, " (", action, ")")
      } else {
        individual_title <- NULL
      }
      
      # Create the hexbin feature plot using hextant
      individual_plot <- hextant::plot_hextant_feature(
        obj = object,
        feature = feature,
        assay = assay,
        layer = layer,
        action = action,
        nbins = nbins,
        dimension_reduction = reduction,
        use_dims = use_dims,
        split_by = split_by,
        ncol = NULL,  # Will handle layout with patchwork
        scales = scales,
        cell_min_cutoff = cell_min_cutoff,
        cell_max_cutoff = cell_max_cutoff,
        title = individual_title,
        xlab = paste0(reduction, "_1"),
        ylab = paste0(reduction, "_2")
      )
      
      # Apply additional styling layers (the fill scale is added after all
      # panels are built, so quantile limits can be taken over every panel)
      layers <- c(
        list(
          theme(
            legend.position = if (show_legend) "right" else "none"
          )
        ),
        
        if (is_subset & isTruthy(original_limits)) {
          list(
            scale_x_continuous(limits = xlim_orig),
            scale_y_continuous(limits = ylim_orig)
          )
        }
      )
      
      # Apply the layers to the plot (see note above on the fill scale message)
      suppressMessages(
        individual_plot <- individual_plot & layers
      )
      
      # Add to plot list
      plot_list[[i]] <- individual_plot
    }
    
    # Combine plots using patchwork
    # Determine number of columns for layout
    layout_ncol <- if (!is.null(ncol)) {
      ncol
    } else {
      # Default: use 2 columns for multiple features
      2
    }
    
    # Color scale limits given as quantiles are evaluated against the
    # per-bin values of every panel together
    scale_limits <-
      resolve_color_limits(
        color_limits,
        quantile = color_limits_quantile,
        values = unlist(lapply(plot_list, plotted_values, "feature_value"))
      )
    
    # Replace hextant's viridis fill scale on each panel with the diverging
    # palette (if provided, it takes precedence) or the global/default
    # continuous palette, applying any user-set color scale limits
    plot_list <-
      lapply(
        plot_list,
        function(panel){
          suppressMessages(
            panel <- panel &
              color_scale(
                "fill",
                palette =
                  if (!is.null(diverging_palette)) diverging_palette else palette,
                limits = scale_limits,
                diverging = !is.null(diverging_palette)
              )
          )
          panel
        }
      )
    
    combined_plot <- patchwork::wrap_plots(
      plot_list,
      ncol = layout_ncol
    )
    
    # Record the range of the per-bin values shown across all features, for
    # the color scale limits slider, and the per-cell range before cutoffs
    attr(combined_plot, "plotted_range") <-
      plotted_range(combined_plot, "feature_value")
    attr(combined_plot, "cell_range") <- cell_range
    
    return(combined_plot)
  }
}
