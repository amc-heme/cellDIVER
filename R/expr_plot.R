#' Plot feature expression by group
#'
#' Common codebase for violin, ridge plots.
#'
#' @param object Seurat object
#' @param type Plot type, choose from 'ridge', 'violin', or 'splitViolin'
#' @param features Features to plot (gene expression, metrics, PC scores,
#'  anything that can be retreived by fetch_data)
#' @param idents Which classes to include in the plot (default is all)
#' @param ncol Number of columns if multiple plots are displayed
#' @param sort Sort identity classes (on the x-axis) by the average expression
#' of the attribute being potted, or, if stack is True, sort both identity
#' classes and features by hierarchical clustering
#' @param y_max Maximum y axis value
#' @param same_y_lims Set all the y-axis limits to the same values
#' @param adjust Adjust parameter for geom_violin
#' @param pt_size Point size for geom_violin
#' @param cols Colors to use for plotting
#' @param group_by Group (color) cells in different ways (for example, orig.ident)
#' @param split_by A variable to split the plot by
#' @param log plot Y axis on log scale
#' @param slot Slot (Seurat objects) or assay (SingleCellExperiment objects) to
#' pull expression data from (counts/data for Seurat objects, counts/logcounts
#' for SingleCellExperiment objects)
#' @param stack Horizontally stack plots for multiple feature
#' @param combine Combine plots into a single \code{\link[patchwork]{patchwork}ed}
#' ggplot object. If \code{FALSE}, return a list of ggplot objects
#' @param fill_by Color violins/ridges based on either 'feature' or 'ident'
#' @param flip flip plot orientation (identities on x-axis)
#' @param add_noise determine if adding a small noise for plotting
#' @param raster Convert points to raster format, default is \code{NULL} which
#' automatically rasterizes if plotting more than 100,000 cells
#'
#' @return A \code{\link[patchwork]{patchwork}ed} ggplot object if
#' \code{combine = TRUE}; otherwise, a list of ggplot objects
#'
#' @importFrom scales hue_pal
#' @importFrom ggplot2 xlab ylab
#' @importFrom patchwork wrap_plots
#' 
#' @keywords internal
#' 
#' @export
expr_plot <-
  function(
    object,
    features,
    group_by,
    type = 'violin',
    idents = NULL,
    ncol = NULL,
    sort = FALSE,
    y_max = NULL,
    same_y_lims = FALSE,
    adjust = 1,
    cols = NULL,
    pt_size = 0,
    split_by = NULL,
    log = FALSE,
    slot = NULL,
    stack = FALSE,
    combine = TRUE,
    fill_by = NULL,
    flip = FALSE,
    add_noise = TRUE,
    raster = NULL
  ){
    # 1. Set defaults
    ## 1.1. Set default number of columns if undefined, and if stack is FALSE
    if (stack == TRUE) {
      if (!is.null(x = ncol)) {
        warning(
          "'ncol' is ignored with 'stack' is TRUE",
          call. = FALSE,
          immediate. = TRUE
        )
      }
      if (!is.null(x = y_max)) {
        warning(
          "'y_max' is ignored when 'stack' is TRUE",
          call. = FALSE,
          immediate. = TRUE
        )
      }
    } else {
      ncol <- ncol %||% ifelse(
        test = length(x = features) > 9,
        yes = 4,
        # If less than 9 features, use number of features or 3,
        # whichever is lower
        no = min(length(x = features), 3)
      )
    }
    
    ## 1.2. Define the default slot
    slot <-
      slot %||% SCUBA::default_layer(object)
    
    # 2. Fetch data
    ## 2.1. Fetch feature expression data
    data <-
      SCUBA::fetch_data(
        object = object,
        vars = features,
        layer = slot
      )
    
    # Set `features` equal to the colnames of the data
    # returned (features requested)
    features <- colnames(x = data)
    
    ## 2.2. Fetch group_by metadata (stored as a separate variable instead of
    # in `data`)
    group <-
      SCUBA::fetch_metadata(
        object = object,
        vars = group_by,
        # Get data for all cells (subsetting will happen later based on `idents`)
        cells = SCUBA::get_all_cells(object),
        return_class = "vector"
      )
    
    # Convert to factor if it is not already
    if (!is.factor(x = group)) {
      group <- factor(x = group)
    }
    
    # 3. Settings based on data fetched in 2.
    ## 3.1. AutoPointSize: Seurat function, may break on other object types
    pt_size <-
      pt_size %||% AutoPointSize(data = object)
    
    ## 3.2. Cells included in plot (based on idents)
    if (is.null(x = idents)) {
      cells <- SCUBA::get_all_cells(object)
    } else {
      # Fetch metadata for group_by category, then subset for cells that
      # are in idents
      cells <- names(group[group %in% idents])
      
      # Code from original Seurat::ExIPlot
      # cells <- names(Idents(object)[Idents(object) %in% idents])
    }
    
    # 4. Subset for cells included
    ## 4.1. Subset expression data
    data <- data[cells, , drop = FALSE]
    
    ## 4.2. Subset group by data
    group <- group[cells]
    
    # 5. Process split_by settings (data is also stored as a separate variable)
    if (is.null(x = split_by)) {
      split <- NULL
    } else {
      # Fetch split_by metadata, subsetted for the cells in the object.
      split <-
        SCUBA::fetch_metadata(
          object = object,
          vars = split_by,
          cells = cells,
          return_class = "vector"
        )
      
      # Convert split_by metadata to a factor
      if (!is.factor(x = split)) {
        split <- factor(x = split)
      }
      
      # Set up colors for split plot
      if (is.null(x = cols)) {
        cols <- hue_pal()(length(x = levels(x = group)))
        cols <- Seurat:::Interleave(cols, Seurat:::InvertHex(hexadecimal = cols))
      } else if (length(x = cols) == 1 && cols == 'interaction') {
        # Splits each split_by group into secondary violins by ident class and
        # colors each ident-split_by combination separately
        # base interaction(): shows interaction of the group and split factors
        split <- interaction(group, split)
        cols <- hue_pal()(length(x = levels(x = group)))
      } else {
        cols <- Seurat:::Col2Hex(cols)
      }
      
      if (length(x = cols) < length(x = levels(x = split))) {
        cols <- Seurat:::Interleave(cols, Seurat:::InvertHex(hexadecimal = cols))
      }
      
      cols <- rep_len(x = cols, length.out = length(x = levels(x = split)))
      
      names(x = cols) <- levels(x = split)
      
      if ((length(x = cols) > 2) & (type == "splitViolin")) {
        warning("Split violin is only supported for <3 groups, using multi-violin.")
        type <- "violin"
      }
    }
    
    if (same_y_lims && is.null(x = y_max)) {
      y_max <- max(data)
    }
    
    # 6. Create individual violin/ridge plots
    if (isTRUE(x = stack)) {
      return(Seurat:::MultiExIPlot(
        type = type,
        data = data,
        # Idents: group_by data for each cell
        idents = group,
        split = split,
        sort = sort,
        same.y.lims = same_y_lims,
        adjust = adjust,
        cols = cols,
        pt.size = pt_size,
        log = log,
        fill.by = fill_by,
        add.noise = add_noise,
        flip = flip
      ))
    }
    plots <- lapply(
      X = features,
      FUN = function(x) {
        return(Seurat:::SingleExIPlot(
          type = type,
          data = data[, x, drop = FALSE],
          # Idents: group_by data for each cell
          idents = group,
          split = split,
          sort = sort,
          y.max = y_max,
          adjust = adjust,
          cols = cols,
          pt.size = pt_size,
          log = log,
          add.noise = add_noise,
          raster = raster
        ))
      }
    )
    
    # 7. Modify axis labels based on type features plotted
    # (default label is "Expression Level")
    
    ## 7.1. Define label function to apply based on plot type
    label.fxn <- switch(
      EXPR = type,
      'violin' = if (stack) {
        xlab
      } else {
        ylab
      },
      "splitViolin" = if (stack) {
        xlab
      } else {
        ylab
      },
      'ridge' = xlab,
      stop("Unknown ExIPlot type ", type, call. = FALSE)
    )
    
    ## 7.2. Apply label function to each plot
    # Determine if the key matches an assay or a reduction, and change labels
    # based on the result
    for (i in 1:length(x = plots)) {
      relabel_value <-
        relabel_axis(
          object = object,
          feature = features[i]
        )
      
      if (!is.null(relabel_value)){
        if (relabel_value == ""){
          # If relabel_axis returns "", remove label from plot
          plots[[i]] <-
            plots[[i]] +
            label.fxn(label = NULL)
        } else {
          # Otherwise, relabel according to relabel_axis output
          plots[[i]] <-
            plots[[i]] +
            label.fxn(label = relabel_value)
        }
      }
    }
    
    # 8. Combine plots and return
    if (combine) {
      plots <- wrap_plots(plots, ncol = ncol)
      if (length(x = features) > 1) {
        plots <- plots & NoLegend()
      }
    }
    
    return(plots)
  }
