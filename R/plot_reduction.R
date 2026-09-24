#------- Function copied and adapted from Seurat package -----------------------
# https://github.com/satijalab/seurat

#' Dimensional reduction plot
#'
#' Graphs the output of a dimensional reduction technique on a 2D scatter plot where each point is a
#' cell and it's positioned based on the cell embeddings determined by the reduction technique. The function accepts both Seurat and SingleCellExperiment objects.
#' For Seurat objects, cells are colored by their identity class by default, and for
#' SingleCellExperiment objects, cells are colored by the first metadata column in
#' colData(). The metadata variable used for coloring cells can be changed with the group_by parameter).
#'
#' The code for this function was from the [Seurat Package](https://github.com/satijalab/seurat/blob/master/R/visualization.R) and adapted for use with
#' [SingleCellExperiment](https://www.bioconductor.org/packages/release/bioc/html/SingleCellExperiment.html) objects.
#'
#' @param object a Seurat object or a SingleCellExperiment object
#' @param dims Dimensions to plot, must be a two-length numeric vector specifying x- and y-dimensions i.e. c(1,2) to plot the first and second dimensions from the
#' reduction results.
#' @param cells Vector of cells to plot (default is all cells)
#' @param cols Vector of colors, each color corresponds to an identity class. This may also be a single character
#' or numeric value corresponding to a palette as specified by \code{\link[RColorBrewer]{brewer.pal.info}}.
#' By default, ggplot2 assigns colors. We also include a number of palettes from the pals package.
#' See \code{\link{DiscretePalette}} for details.
#' @param pt_size Adjust point size for plotting
#' @param reduction Which dimensionality reduction to use. If not specified, first searches for umap, then tsne, then pca
#' @param group_by Name of one or more metadata columns to group (color) cells by
#' (for example, orig.ident). Unlike \code{Seurat::DimPlot}, "ident" may not be
#' passed since the ident functionality is not supported by SingleCellExperiment
#' objects. A metadata column name must be passed.
#' @param split_by Name of a metadata column to split plot by. Unlike \code{Seurat::DimPlot},
#' "ident" may not be passed since the ident functionality is not supported by SingleCellExperiment
#' objects. A metadata column name must be passed, or \code{NULL} to disable split plots.
#' @param shape_by If NULL, all points are circles (default). You can specify any
#' cell attribute (that can be pulled with fetch_data) allowing for both
#' different colors and different shapes on cells.  Only applicable if \code{raster = FALSE}.
#' @param order Specify the order of plotting for the idents. This can be
#' useful for crowded plots if points of interest are being buried. Provide
#' either a full list of valid idents or a subset to be plotted last (on top)
#' @param shuffle Whether to randomly shuffle the order of points. This can be
#' useful for crowded plots if points of interest are being buried. (default is FALSE)
#' @param seed Sets the seed if randomly shuffling the order of points.
#' @param label Whether to label the clusters
#' @param label_size Sets size of labels
#' @param label_color Sets the color of the label text
#' @param label_box Whether to put a box around the label text (geom_text vs
#' geom_label)
#' @param repel Repel labels
#' @param cells_highlight A list of character or numeric vectors of cells to
#' highlight. If only one group of cells desired, can simply
#' pass a vector instead of a list. If set, colors selected cells to the color(s)
#' in \code{cols_highlight} and other cells black (white if dark.theme = TRUE);
#' will also resize to the size(s) passed to \code{sizes_highlight}
#' @param cols_highlight A vector of colors to highlight the cells as; will
#' repeat to the length groups in cells_highlight
#' @param sizes_highlight Size of highlighted cells; will repeat to the length
#' groups in cells_highlight
#' @param na_value Color value for NA points when using custom scale
#' @param ncol Number of columns for display when combining plots
#' @param combine Combine plots into a single \code{\link[patchwork]{patchwork}ed}
#' ggplot object. If \code{FALSE}, return a list of ggplot objects
#' @param raster Convert points to raster format, default is \code{NULL} which
#' automatically rasterizes if plotting more than 100,000 cells
#' @param raster_dpi Pixel resolution for rasterized plots, passed to geom_scattermore().
#' Default is c(512, 512).
#'
#' @return A \code{\link[patchwork]{patchwork}ed} ggplot object if
#' \code{combine = TRUE}; otherwise, a list of ggplot objects
#'
#' @import rlang
#' @import Seurat
#' @importFrom ggplot2 facet_wrap vars sym labs
#' @importFrom SeuratObject DefaultDimReduc
#' @importFrom patchwork wrap_plots
#' @importFrom SingleCellExperiment reducedDimNames reducedDims colData
#'
#' @keywords internal
#'
#' @export
#'
plot_reduction <- function(
    object,
    dims = c(1, 2),
    cells = NULL,
    cols = NULL,
    pt_size = NULL,
    reduction = NULL,
    group_by = NULL,
    split_by = NULL,
    shape_by = NULL,
    order = NULL,
    shuffle = FALSE,
    seed = 1,
    label = FALSE,
    label_size = 4,
    label_color = 'black',
    label_box = FALSE,
    repel = FALSE,
    cells_highlight = NULL,
    cols_highlight = '#DE2D26',
    sizes_highlight = 1,
    na_value = 'grey50',
    ncol = NULL,
    combine = TRUE,
    raster = NULL,
    raster_dpi = c(512, 512)
) {
  if (length(x = dims) != 2) {
    stop("'dims' must be a two-length vector")
  }
  
  # Require group_by to be defined for SingleCellExperiment objects
  if (is(object, "SingleCellExperiment")){
    if (is.null(group_by)){
      stop("For SingleCellExperiment objects, `group_by` must be defined.")
    }
  }
  
  # 1. Define reduction (defaults to the first reduction stored for sce objects)
  # Uses rlang %||% infix
  reduction <- reduction %||% SCUBA::default_reduction(object)
  # 2. Define cells to include in plot
  ## Same as for Seurat object ##
  cells <- cells %||% SCUBA::get_all_cells(object)
  
  # Fetch dimensional reduction data from object
  # 3. Convert dims to format readable by fetch_data (<reduction>_<dim>)
  dim_names <-
    SCUBA::reduction_dimnames(
      object,
      reduction = reduction,
      dims = dims
    )
  
  # 4. Identify group_by variable, store in orig_groups
  # orig_groups is used to test whether the group_by was set by the user
  orig_groups <- group_by
  # Ident does not exist for SingleCellExperiment objects, but this will never
  # be applied since group_by will always be defined.
  group_by <- group_by %||% 'ident'
  
  # 5. Fetch reduction coordinates
  data <-
    SCUBA::fetch_reduction(
      object = object,
      reduction = reduction,
      cells = cells,
      dims = dims
    )
  
  # 6. Fetch group by metadata and append
  data <-
    cbind(
      data,
      SCUBA::fetch_metadata(
        object = object,
        vars = group_by,
        cells = cells
      )
    )
  
  # Throw an error if reduction coordinates or group_by data were not
  # properly returned
  if (!all(dim_names %in% colnames(data))) {
    stop("The dimensions requested were not found.", call. = FALSE)
  } else if (!all(group_by %in% colnames(data))){
    stop("The group_by variable(s) requested were not found.", call. = FALSE)
  }
  
  # 7. Define group_by variables to iterate through
  group_by_cols <-
    colnames(data)[3:ncol(data)]
  
  # 8. Convert group by columns to factors if they are not already
  for (group in group_by_cols) {
    if (!is.factor(data[, group])) {
      data[, group] <- factor(data[, group])
    }
  }
  
  # 9. Add shape_by data if it exists
  if (!is.null(x = shape_by)) {
    data[, shape_by] <-
      SCUBA::fetch_metadata(
        object = object,
        vars = shape_by,
        cells = cells
      )
  }
  
  # 10. Same for split_by data
  if (!is.null(x = split_by)) {
    data[, split_by] <-
      SCUBA::fetch_metadata(
        object = object,
        vars = split_by,
        cells = cells
      )
  }
  
  # 11. If shufffle is TRUE, randomly shuffle cells
  if (isTRUE(shuffle)) {
    set.seed(seed = seed)
    data <- data[sample(x = 1:nrow(x = data)), ]
  }
  
  # For each group by variable, create a DimPlot of cells grouped
  # by that variable.
  plots <- lapply(
    X = group_by_cols,
    FUN = function(group) {
      plot <- Seurat::SingleDimPlot(
        data = data[, c(dim_names, group, split_by, shape_by)],
        dims = dim_names,
        col.by = group,
        # cols: a categorical pallete in this case
        cols = cols,
        pt.size = pt_size,
        shape.by = shape_by,
        order = order,
        label = FALSE,
        # Cells to highlight
        cells.highlight = cells_highlight,
        cols.highlight = cols_highlight,
        sizes.highlight = sizes_highlight,
        na.value = na_value,
        raster = raster,
        raster.dpi = raster_dpi
      )
      if (label) {
        plot <-
          Seurat::LabelClusters(
            plot = plot,
            id = group,
            repel = repel,
            size = label_size,
            split.by = split_by,
            box = label_box,
            color = label_color
          )
      }
      if (!is.null(x = split_by)) {
        plot <-
          plot +
          Seurat:::FacetTheme() +
          ggplot2::facet_wrap(
            # rlang injection operator
            # ?`!!` for more information
            facets = vars(!!sym(x = split_by)),
            ncol = if (length(x = group_by) > 1 || is.null(x = ncol)) {
              length(x = unique(x = data[, split_by]))
            } else {
              ncol
            }
          )
      }
      plot <- if (is.null(x = orig_groups)) {
        plot + labs(title = NULL)
      } else {
        plot + CenterTitle()
      }
    }
  )
  if (!is.null(x = split_by)) {
    ncol <- 1
  }
  if (combine) {
    plots <-
      patchwork::wrap_plots(
        plots,
        # %iff% infix (Seurat package)
        # Uses ncol if orig_groups is not NULL, otherwise uses NULL
        ncol = orig_groups %iff% ncol
      )
  }
  return(plots)
}
