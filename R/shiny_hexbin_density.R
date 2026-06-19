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
#'   color scale. When NULL, viridis is used (hextant default). When 
#'   provided, overrides the default viridis scale with the specified palette.
#'
#' @return A ggplot2 object with a hexbin density plot created according to
#'   user specifications.
#'
#' @importFrom hextant plot_hextant_density
#' @import ggplot2
#' @import SCUBA
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
    palette = NULL) {
  
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
  
  # Set default reduction if not specified
  if (is.null(reduction)) {
    reduction <- "UMAP"
  }
  
  # Validate reduction exists in object
  available_reductions <- tryCatch({
    SCUBA::all_keys(object)$reductions
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
  
  # Apply additional styling layers to match cellDIVER aesthetics
  layers <- c(
    # Legend position: "right" to match cellDIVER standard, or "none" if not
    # desired
    list(
      theme(
        legend.position = if (show_legend) "right" else "none"
      )
    ),
    
    # Override viridis color scale with global palette if a palette is provided
    if (!is.null(palette)) {
      list(
        scale_fill_gradientn(colors = palette)
      )
    },
    
    # Apply original axis limits if this is a subset and user has selected
    # to use original limits
    if (is_subset & isTruthy(original_limits)) {
      list(
        scale_x_continuous(limits = xlim_orig),
        scale_y_continuous(limits = ylim_orig)
      )
    }
  )
  
  # Apply the layers to the plot
  plot <- plot & layers
  
  # Return the finished plot
  return(plot)
}
