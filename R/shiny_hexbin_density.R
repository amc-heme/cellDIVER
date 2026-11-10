#' Shiny Wrapper for Hexbin Density Plots
#'
#' Creates a hexbin density plot using hextant::plot_hextant_density(),
#' with styling and parameters consistent with cellDIVER's plotting system.
#' This function visualizes cell density across dimension reduction space
#' using hexagonal binning, which reduces overplotting in dense datasets.
#'
#' @param object A SCUBA-supported single-cell object (Seurat,
#'   SingleCellExperiment, or other supported format). This can be either the
#'   full object or a subset. This is a reactive-agnostic parameter (can be
#'   either reactive or non-reactive).
#' @param nbins The number of bins partitioning the range of the first
#'   component of the chosen dimension reduction. Higher values create more
#'   granular hexagons. Default is 80.
#' @param reduction A string indicating the reduced dimension result to use
#'   (e.g., "UMAP", "PCA", "TSNE"). If NULL, uses the object's default
#'   reduction.
#' @param split_by A string naming a metadata variable to split the plot by.
#'   If "none" or NULL, no splitting is performed. When provided, creates
#'   separate faceted plots for each unique value in the metadata variable.
#' @param scale_density Logical. If TRUE and split_by is used, each facet
#'   will have an independent color scale showing relative density within that
#'   group (rescaled 0-1). If FALSE (default), all facets share a single color
#'   scale showing absolute cell counts. Only applies when split_by is used.
#' @param ncol Number of columns for facet layout when split_by is used.
#'   If NULL, ggplot2 determines the layout automatically.
#' @param scales Should scales be "fixed" (default, same for all facets),
#'   "free" (vary across facets), "free_x", or "free_y"? Only applies when
#'   split_by is used.
#' @param show_title Logical indicating whether to display a title. This 
#'   parameter is maintained for consistency with other plot wrappers but is not
#'   used internally. Title handling is managed by the plot_title reactive in
#'   plot_module.
#' @param plot_title A string containing the title of the plot. This should be
#'   passed from the plot_title reactive expression in plot_module, which 
#'   handles default titles ("Density"), custom titles, and no title (NULL) 
#'   based on user selections.
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
#' @param palette A character vector of hex color codes for the continuous 
#'   color scale. When NULL, cellDIVER's default continuous palette
#'   (lightgrey to blue, matching Feature and Dot plots) is used. hextant's
#'   viridis scale is always replaced.
#' @param color_limits_quantile if TRUE, the elements of `color_limits`
#'   are probabilities (0 to 1) evaluated against the plotted values rather
#'   than values in data units.
#' @param color_limits optional numeric vector of length two giving the
#'   values at which the lowest and highest colors are placed. Values outside
#'   the limits are shown in the end colors. When NULL, the range of the
#'   plotted values is used.
#'
#' @return A ggplot2 object with a hexbin density plot created according to
#'   user specifications. The "plotted_range" attribute gives the range of
#'   the per-bin values (cell counts, or scaled density) mapped to the fill
#'   scale.
#'
#' 
#' @noRd
shiny_hexbin_density <- function(
    object,
    nbins = 80,
    reduction = NULL,
    split_by = NULL,
    scale_density = FALSE,
    ncol = NULL,
    scales = "fixed",
    show_title = TRUE,
    plot_title = NULL,
    show_legend = TRUE,
    is_subset = FALSE,
    original_limits = FALSE,
    xlim_orig = NULL,
    ylim_orig = NULL,
    palette = NULL,
    color_limits = NULL,
    color_limits_quantile = FALSE) {
  
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
        "Hexbin density plot error: cannot plot subsets with only one cell."
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
  
  # Use cellDIVER's default continuous palette when none is selected, so
  # hexbin plots match the Feature and Dot plot defaults instead of
  # hextant's viridis scale
  if (is.null(palette)) {
    palette <- c("lightgrey", "blue")
  }
  
  # Create the hexbin density plot using hextant
  # Title is passed directly from the caller (already evaluated)
  plot <- hextant::plot_hextant_density(
    obj = object,
    nbins = nbins,
    dimension_reduction = reduction,
    use_dims = use_dims,
    split_by = split_by,
    scale_density = scale_density,
    ncol = ncol,
    scales = scales,
    title = plot_title,
    xlab = paste0(reduction, "_1"),
    ylab = paste0(reduction, "_2")
  )

  # Remove hextant's default title when the user disables titles.
  if (!isTRUE(show_title)) {
    plot <- plot + labs(title = NULL)
  }
  
  # Column hextant mapped to fill: scaled density (0-1) when scaling per
  # facet, otherwise the number of cells per bin
  fill_column <-
    if (!is.null(split_by) && isTRUE(scale_density)){
      "scaled_density"
    } else {
      "number_of_cells"
    }
  
  # Color scale limits given as quantiles are evaluated against the per-bin
  # values shown
  scale_limits <-
    resolve_color_limits(
      color_limits,
      quantile = color_limits_quantile,
      values = plotted_values(plot, fill_column)
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
    
    # Override hextant's viridis color scale with the global (or default)
    # continuous palette, applying any user-set color scale limits
    list(
      color_scale("fill", palette = palette, limits = scale_limits)
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
  
  # Record the range of the per-bin values shown, for the color scale limits
  # slider. hextant fills by scaled density (0-1) when scaling per facet,
  # and by the number of cells otherwise.
  attr(plot, "plotted_range") <- plotted_range(plot, fill_column)
  
  # Return the finished plot
  return(plot)
}
