#' Visualize 'features' on a dimensional reduction plot
#'
#' Colors single cells on a dimensional reduction plot according to a 'feature'
#' (i.e. gene expression, PC scores, number of genes detected, etc.)
#'
#' @inheritParams plot_reduction
#' @param order Boolean determining whether to plot cells in order of expression. Can be useful if
#' cells expressing given feature are getting buried.
#' @param features Vector of features to plot. Features can come from:  
#'     - An `Assay` feature (e.g. a gene name - "MS4A1")  
#'     - A column name from meta.data (e.g. mitochondrial percentage - "percent.mito")  
#'     - A column name from a `DimReduc` object corresponding to the cell embedding values
#'     (e.g. the PC 1 scores - "PC_1")  
#' @param label_by A metadata column used for labeling groups on the featute plot,
#' if label is \code{TRUE}.
#' @param cols The two colors to form the gradient over. Provide as string vector with
#' the first color corresponding to low values, the second to high. Also accepts a Brewer
#' color scale or vector of colors. Note: this will bin the data into number of colors provided.
#' When blend is \code{TRUE}, takes anywhere from 1-3 colors:  
#' 
#'   - 1 color:  
#'     + Treated as color for double-negatives, will use default colors 2 and 3 for per-feature expression  
#'   - 2 colors:  
#'     + Treated as colors for per-feature expression, will use default color 1 for double-negatives  
#'   - 3+ colors:  
#'     + First color used for double-negatives, colors 2 and 3 used for per-feature expression, all others ignored  
#'
#' @param min_cutoff,max_cutoff Vector of minimum and maximum cutoff values for each feature,
#'  may specify quantile in the form of 'q##' where '##' is the quantile (eg, 'q1', 'q10')
#' @param split_by A metadata column to split the feature plot by. Unlike \code{Seurat::FeaturePlot},
#' "ident" may not be passed since the ident functionality is not supported by SingleCellExperiment
#' objects. A metadata column name must be passed, or \code{NULL} to disable split plots.
#' @param keep_scale How to handle the color scale across multiple plots. Options are:  
#' 
#'   - `feature` (default; by row/feature scaling):  
#'     + The plots for each individual feature are scaled to the maximum expression of the feature across the conditions provided to 'split_by'.  
#'   - `all` (universal scaling):  
#'     +  The plots for all features and conditions are scaled to the maximum expression value for the feature with the highest overall expression.  
#'   - `NULL` (no scaling):  
#'     + Each individual plot is scaled to the maximum expression value of the feature in the condition provided to 'split_by'. Be aware setting NULL will result in color scales that are not comparable between plots.  
#' 
#' @param slot Which slot to pull expression data from? If \code{NULL}, defaults to "data" for Seurat objects, and "logcounts" for SingleCellExperiment objects.
#' @param blend Scale and blend expression values to visualize co-expression of two features
#' @param blend_threshold The color cutoff from weak signal to strong signal; ranges from 0 to 1.
#' @param ncol Number of columns to combine multiple feature plots to, ignored if \code{split_by} is not \code{NULL}
#' @param coord_fixed Plot cartesian coordinates with fixed aspect ratio
#' @param by_col If splitting by a factor, plot the splits per column with the features as rows; ignored if \code{blend = TRUE}
#' @param combine Combine plots into a single \code{\link[patchwork]{patchwork}ed}
#' ggplot object. If \code{FALSE}, return a list of ggplot objects
#'
#' @return A \code{\link[patchwork]{patchwork}ed} ggplot object if
#' \code{combine = TRUE}; otherwise, a list of ggplot objects
#'
#' @importFrom grDevices rgb
#' @importFrom patchwork wrap_plots
#' @importFrom cowplot theme_cowplot
#' @importFrom RColorBrewer brewer.pal.info
#' @importFrom ggplot2 labs scale_x_continuous scale_y_continuous theme element_rect
#' dup_axis guides element_blank element_text margin scale_color_brewer scale_color_gradientn
#' scale_color_manual coord_fixed ggtitle
#'
#' @export
#' @concept visualization
#'
#' @note For the old \code{do.hover} and \code{do.identify} functionality, please see
#' \code{HoverLocator} and \code{CellSelector}, respectively.
#'
#' @aliases FeatureHeatmap
#' @seealso \code{\link{DimPlot}} \code{\link{HoverLocator}}
#' \code{\link{CellSelector}}
#' 
#' @keywords internal
#' 
#' @examples
#' plot_feature(AML_Seurat, features = "PC_1")
#'
plot_feature <- function(
    object,
    features,
    label_by = NULL,
    dims = c(1, 2),
    cells = NULL,
    cols = if (blend) {
      c('lightgrey', '#ff0000', '#00ff00')
    } else {
      c('lightgrey', 'blue')
    },
    pt_size = NULL,
    order = FALSE,
    min_cutoff = NA,
    max_cutoff = NA,
    reduction = NULL,
    split_by = NULL,
    keep_scale = "feature",
    shape_by = NULL,
    slot = NULL,
    blend = FALSE,
    blend_threshold = 0.5,
    label = FALSE,
    label_size = 4,
    label_color = "black",
    repel = FALSE,
    ncol = NULL,
    coord_fixed = FALSE,
    by_col = TRUE,
    combine = TRUE,
    raster = NULL,
    raster_dpi = c(512, 512)
) {
  # 1. Check arguments, set defaults
  # Check keep_scale param for valid entries
  if (!(is.null(x = keep_scale)) && !(keep_scale %in% c("feature", "all"))) {
    stop("`keep_scale` must be set to either `feature`, `all`, or NULL")
  }
  
  # Throw an error if label is TRUE, but no label_by metadata is provided
  if (label == TRUE && is.null(label_by)){
    stop("if `label` is TRUE, `label_by` must be defined.")
  }
  
  # Set a theme to remove right-hand Y axis lines
  # Also sets right-hand Y axis text label formatting
  # (Convert to a function?)
  no_right <- theme(
    axis.line.y.right = element_blank(),
    axis.ticks.y.right = element_blank(),
    axis.text.y.right = element_blank(),
    axis.title.y.right = element_text(
      face = "bold",
      size = 14,
      margin = margin(r = 7)
    )
  )
  
  # Reduction: if undefined, use default reduction for the object (fetched
  # via the default_reduction S3 method)
  reduction <- reduction %||% SCUBA::default_reduction(object)
  # Set default slot if not defined
  slot <- slot %||% SCUBA::default_layer(object)
  # Select all cells if cells is unspecified
  cells <- cells %||% SCUBA::get_all_cells(object)
  
  if (length(x = dims) != 2 || !is.numeric(x = dims)) {
    stop("'dims' must be a two-length integer vector")
  }
  
  # If blending is enabled, verify there are exactly two features entered
  if (blend && length(x = features) != 2) {
    stop("Blending feature plots only works with two features")
  }
  
  # Set color scheme for blended FeaturePlots
  if (blend) {
    default_colors <- eval(expr = formals(fun = FeaturePlot)$cols)
    cols <- switch(
      EXPR = as.character(x = length(x = cols)),
      '0' = {
        warning(
          "No colors provided, using default colors",
          call. = FALSE,
          immediate. = TRUE
        )
        default_colors
      },
      '1' = {
        warning(
          "Only one color provided, assuming specified is double-negative and augmenting with default colors",
          call. = FALSE,
          immediate. = TRUE
        )
        c(cols, default_colors[2:3])
      },
      '2' = {
        warning(
          "Only two colors provided, assuming specified are for features and agumenting with '",
          default_colors[1],
          "' for double-negatives",
          call. = FALSE,
          immediate. = TRUE
        )
        c(default_colors[1], cols)
      },
      '3' = cols,
      {
        warning(
          "More than three colors provided, using only first three",
          call. = FALSE,
          immediate. = TRUE
        )
        cols[1:3]
      }
    )
  }
  if (blend && length(x = cols) != 3) {
    stop("Blending feature plots only works with three colors; first one for negative cells")
  }
  
  # Define names for reductions, for plotting
  dim_names <-
    SCUBA::reduction_dimnames(
      object,
      reduction = reduction,
      dims = dims
    )
  
  # 2. Get plotting data
  ## 2.1. Fetch reduction coordinates
  data <-
    SCUBA::fetch_reduction(
      object = object,
      reduction = reduction,
      cells = cells,
      dims = dims
    )
  
  ## 2.2. Fetch metadata for label_by column, if provided
  if (!is.null(label_by)){
    data <-
      cbind(
        data,
        SCUBA::fetch_metadata(
          object = object,
          vars = label_by,
          cells = cells
        )
      )
    
    # Convert label_by data to a factor unless it is already
    if (!is.factor(x = data[[label_by]])) {
      data[[label_by]] <- factor(x = data[[label_by]])
    }
  }
  
  ## 2.3. Fetch expression data for the requested features
  data <-
    cbind(
      data,
      SCUBA::fetch_data(
        object = object,
        vars = features,
        layer = slot,
        cells = cells
      )
    )
  
  ## 2.4. Check that at least one of the features was properly
  # fetched from the object.
  if (!is.null(label_by)){
    # If label_by is provided and at least one feature is found, data will have
    # at least 4 columns.
    if (ncol(x = data) < 4) {
      stop(
        "None of the requested features were found: ",
        paste(features, collapse = ', '),
        " in slot ",
        slot,
        call. = FALSE
      )
    }
  } else {
    # Without a label_by variable, data will have 3 or more columns if features
    # were properly fetched
    if (ncol(x = data) < 3) {
      stop(
        "None of the requested features were found: ",
        paste(features, collapse = ', '),
        " in slot ",
        slot,
        call. = FALSE
      )
    }
  }
  
  ## 2.5. Check that reduction data was properly fetched
  if (!all(dim_names %in% colnames(x = data))) {
    stop("The dimensions requested were not found", call. = FALSE)
  }
  
  ## 2.6. Define names of features returned
  # Feature_idx: index of first feature in data. If label_by is defined, this
  # is equal to 4, otherwise it is equal to 3.
  feature_idx <-
    if (!is.null(label_by)){
      4
    } else {
      3
    }
  
  features <-
    colnames(x = data)[feature_idx:ncol(x = data)]
  
  # 3. Min/max cutoffs
  ## 3.1. Define cutoffs
  # For each feature and cutoff, if the cutoff is NULL, use the minimum
  # expression value for the feature. Otherwise, use the supplied
  # cutoff (either an expression value or a percentile)
  min_cutoff <-
    mapply(
      FUN = function(cutoff, feature) {
        return(
          ifelse(
            test = is.na(x = cutoff),
            yes = min(data[, feature]),
            no = cutoff
          )
        )
      },
      cutoff = min_cutoff,
      feature = features
    )
  
  max_cutoff <-
    mapply(
      FUN = function(cutoff, feature) {
        return(ifelse(
          test = is.na(x = cutoff),
          yes = max(data[, feature]),
          no = cutoff
        ))
      },
      cutoff = max_cutoff,
      feature = features
    )
  
  ## 3.2. Ensure the number of values entered for min_cutoff and max_cutoff
  # equals the number of features
  check_lengths <-
    unique(
      x = vapply(
        X = list(features, min_cutoff, max_cutoff),
        FUN = length,
        FUN.VALUE = numeric(length = 1)
      )
    )
  if (length(x = check_lengths) != 1) {
    stop("There must be the same number of minimum and maximum cuttoffs as there are features")
  }
  
  ## 3.3. brewer_gran
  # (From Seurat::FeaturePlot, appears to apply only if an R Color Brewer
  # palette is passed to cols)
  brewer_gran <-
    ifelse(
      test = length(x = cols) == 1,
      yes = brewer.pal.info[cols, ]$maxcolors,
      no = length(x = cols)
    )
  
  ## 3.4. Apply cutoffs to feature data
  data[, feature_idx:ncol(x = data)] <-
    sapply(
      X = feature_idx:ncol(x = data),
      FUN = function(index) {
        data_feature <- as.vector(x = data[, index])
        min_use <-
          # min_cutoff index: equal to the index of the feature in the data,
          # minus the index of the first feature, plus 1. (if label_by is TRUE,
          # the first feature will have an index of (4 - 4 + 1) = 1 in the
          # min_cutoff vector, since the fourth column in `data` corresponds to
          # the first feature. If label_by is FALSE, the third column corresponds
          # to the first feature.
          Seurat::SetQuantile(
            cutoff = min_cutoff[index - feature_idx + 1],
            data_feature
          )
        max_use <-
          Seurat::SetQuantile(
            cutoff = max_cutoff[index - feature_idx + 1],
            data_feature
          )
        data_feature[data_feature < min_use] <- min_use
        data_feature[data_feature > max_use] <- max_use
        if (brewer_gran == 2) {
          return(data_feature)
        }
        data_cut <- if (all(data_feature == 0)) {
          0
        }
        else {
          as.numeric(x = as.factor(x = cut(
            x = as.numeric(x = data_feature),
            breaks = brewer_gran
          )))
        }
        return(data_cut)
      }
    )
  colnames(x = data)[feature_idx:ncol(x = data)] <- features
  rownames(x = data) <- cells
  
  # 4. Add split_by metadata
  data$split <-
    if (is.null(x = split_by)) {
      SeuratObject::RandomName()
    } else {
      SCUBA::fetch_metadata(
        object = object,
        vars = split_by,
        cells = cells,
        return_class = "vector"
      )
    }
  
  # Convert split_by metadata to a factor if it is not one already
  if (!is.factor(x = data$split)) {
    data$split <- factor(x = data$split)
  }
  
  # 5. Add shape by variable if defined
  if (!is.null(x = shape_by)) {
    data[, shape_by] <-
      SCUBA::fetch_metadata(
        object = object,
        vars = shape_by,
        cells = cells,
        return_class = "vector"
      )
  }
  
  # 6. Make list of plots
  ## 6.1. Create empty list
  plots <-
    vector(
      mode = "list",
      length =
        # Number of elements: 4 for blended feature plots, features * split_by
        # levels for regular plots
        ifelse(
          test = blend,
          yes = 4,
          no = length(x = features) * length(x = levels(x = data$split))
        )
    )
  
  ## 6.2. Define limits to use for all plots, based on range of reduction coords
  xlims <-
    c(floor(x = min(data[, dim_names[1]])),
      ceiling(x = max(data[, dim_names[1]])))
  ylims <-
    c(floor(min(data[, dim_names[2]])),
      ceiling(x = max(data[, dim_names[2]])))
  
  ## 6.3. If blended feature plot, set bend colors
  if (blend) {
    ncol <- 4
    color_matrix <-
      Seurat:::BlendMatrix(
        two.colors = cols[2:3],
        col.threshold = blend_threshold,
        negative.color = cols[1]
      )
    cols <- cols[2:3]
    colors <-
      list(
        color_matrix[, 1],
        color_matrix[1, ],
        as.vector(x = color_matrix)
      )
  }
  
  # 7. Create plots
  for (i in 1:length(x = levels(x = data$split))) {
    # Fetch split i
    ident <- levels(x = data$split)[i]
    data_plot <- data[as.character(x = data$split) == ident, , drop = FALSE]
    # Blend expression values
    if (blend) {
      features <- features[1:2]
      no_expression <- features[colMeans(x = data_plot[, features]) == 0]
      if (length(x = no_expression) != 0) {
        stop(
          "The following features have no value: ",
          paste(no_expression, collapse = ', '),
          call. = FALSE
        )
      }
      data_plot <-
        cbind(
          data_plot[, c(dim_names, label_by)],
          Seurat:::BlendExpression(data = data_plot[, features[1:2]])
        )
      #Account for missing label_by column 
      if(!is.null(label_by)){
        features <- colnames(x = data_plot)[4:ncol(x = data_plot)]
      }
      else{
        features <- colnames(x = data_plot)[3:ncol(x = data_plot)]
      }
    }
    # Make per-feature plots
    for (j in 1:length(x = features)) {
      feature <- features[j]
      # Get blended colors
      if (blend) {
        cols_use <- as.numeric(x = as.character(x = data_plot[, feature])) + 1
        cols_use <- colors[[j]][sort(x = unique(x = cols_use))]
      } else {
        cols_use <- NULL
      }
      data_single <- data_plot[, c(dim_names, label_by, feature, shape_by)]
      # Make the plot
      plot <- SingleDimPlot(
        data = data_single,
        dims = dim_names,
        col.by = feature,
        order = order,
        pt.size = pt_size,
        cols = cols_use,
        shape.by = shape_by,
        label = FALSE,
        raster = raster,
        raster.dpi = raster_dpi
      ) +
        scale_x_continuous(limits = xlims) +
        scale_y_continuous(limits = ylims) +
        theme_cowplot() +
        CenterTitle()
      # theme(plot.title = element_text(hjust = 0.5))
      # Add labels
      if (label) {
        plot <- LabelClusters(
          plot = plot,
          id = label_by,
          repel = repel,
          size = label_size,
          color = label_color
        )
      }
      # Make FeatureHeatmaps look nice(ish)
      if (length(x = levels(x = data$split)) > 1) {
        plot <- plot + theme(panel.border = element_rect(fill = NA, colour = 'black'))
        # Add title
        plot <- plot + if (i == 1) {
          labs(title = feature)
        } else {
          labs(title = NULL)
        }
        # Add second axis
        if (j == length(x = features) && !blend) {
          suppressMessages(
            expr = plot <- plot +
              scale_y_continuous(
                sec.axis = dup_axis(name = ident),
                limits = ylims
              ) +
              no_right
          )
        }
        # Remove left Y axis
        if (j != 1) {
          plot <- plot + theme(
            axis.line.y = element_blank(),
            axis.ticks.y = element_blank(),
            axis.text.y = element_blank(),
            axis.title.y.left = element_blank()
          )
        }
        # Remove bottom X axis
        if (i != length(x = levels(x = data$split))) {
          plot <- plot + theme(
            axis.line.x = element_blank(),
            axis.ticks.x = element_blank(),
            axis.text.x = element_blank(),
            axis.title.x = element_blank()
          )
        }
      } else {
        plot <- plot + labs(title = feature)
      }
      # Add colors scale for normal FeaturePlots
      if (!blend) {
        plot <- plot + guides(color = NULL)
        cols_grad <- cols
        if (length(x = cols) == 1) {
          plot <- plot + scale_color_brewer(palette = cols)
        } else if (length(x = cols) > 1) {
          unique_feature_exp <- unique(data_plot[, feature])
          if (length(unique_feature_exp) == 1) {
            warning("All cells have the same value (", unique_feature_exp, ") of ", feature, ".")
            if (unique_feature_exp == 0) {
              cols_grad <- cols[1]
            } else{
              cols_grad <- cols
            }
          }
          plot <- suppressMessages(
            expr = plot + scale_color_gradientn(
              colors = cols_grad,
              guide = "colorbar"
            )
          )
        }
      }
      if (!(is.null(x = keep_scale)) && keep_scale == "feature" && !blend) {
        max_feature_value <- max(data[, feature])
        min_feature_value <- min(data[, feature])
        plot <- suppressMessages(plot & scale_color_gradientn(colors = cols, limits = c(min_feature_value, max_feature_value)))
      }
      # Add coord_fixed
      if (coord_fixed) {
        plot <- plot + coord_fixed()
      }
      # I'm not sure why, but sometimes the damn thing fails without this
      # Thanks ggplot2
      plot <- plot
      # Place the plot
      plots[[(length(x = features) * (i - 1)) + j]] <- plot
    }
  }
  # Add blended color key
  if (blend) {
    blend_legend <- Seurat:::BlendMap(color.matrix = color_matrix)
    for (ii in 1:length(x = levels(x = data$split))) {
      suppressMessages(expr = plots <- append(
        x = plots,
        values = list(
          blend_legend +
            scale_y_continuous(
              sec.axis = dup_axis(name = ifelse(
                test = length(x = levels(x = data$split)) > 1,
                yes = levels(x = data$split)[ii],
                no = ''
              )),
              expand = c(0, 0)
            ) +
            labs(
              x = features[1],
              y = features[2],
              title = if (ii == 1) {
                paste('Color threshold:', blend_threshold)
              } else {
                NULL
              }
            ) +
            no_right
        ),
        after = 4 * ii - 1
      ))
    }
  }
  # Remove NULL plots
  plots <- Filter(f = Negate(f = is.null), x = plots)
  # Combine the plots
  if (is.null(x = ncol)) {
    ncol <- 2
    if (length(x = features) == 1) {
      ncol <- 1
    }
    if (length(x = features) > 6) {
      ncol <- 3
    }
    if (length(x = features) > 9) {
      ncol <- 4
    }
  }
  ncol <- ifelse(
    test = is.null(x = split_by) || blend,
    yes = ncol,
    no = length(x = features)
  )
  legend <- if (blend) {
    'none'
  } else {
    split_by %iff% 'none'
  }
  # Transpose the FeatureHeatmap matrix (not applicable for blended FeaturePlots)
  if (combine) {
    if (by_col && !is.null(x = split_by) && !blend) {
      plots <- lapply(
        X = plots,
        FUN = function(x) {
          return(suppressMessages(
            expr = x +
              theme_cowplot() +
              ggtitle("") +
              scale_y_continuous(sec.axis = dup_axis(name = ""), limits = ylims) +
              no_right
          ))
        }
      )
      nsplits <- length(x = levels(x = data$split))
      idx <- 1
      for (i in (length(x = features) * (nsplits - 1) + 1):(length(x = features) * nsplits)) {
        plots[[i]] <- suppressMessages(
          expr = plots[[i]] +
            scale_y_continuous(
              sec.axis = dup_axis(name = features[[idx]]),
              limits = ylims
            ) +
            no_right
        )
        idx <- idx + 1
      }
      idx <- 1
      for (i in which(x = 1:length(x = plots) %% length(x = features) == 1)) {
        plots[[i]] <- plots[[i]] +
          ggtitle(levels(x = data$split)[[idx]]) +
          theme(plot.title = element_text(hjust = 0.5))
        idx <- idx + 1
      }
      idx <- 1
      if (length(x = features) == 1) {
        for (i in 1:length(x = plots)) {
          plots[[i]] <- plots[[i]] +
            ggtitle(levels(x = data$split)[[idx]]) +
            theme(plot.title = element_text(hjust = 0.5))
          idx <- idx + 1
        }
        ncol <- 1
        nrow <- nsplits
      } else {
        nrow <- split_by %iff% length(x = levels(x = data$split))
      }
      plots <- plots[c(do.call(
        what = rbind,
        args = split(x = 1:length(x = plots), f = ceiling(x = seq_along(along.with = 1:length(x = plots)) / length(x = features)))
      ))]
      # Set ncol to number of splits (nrow) and nrow to number of features (ncol)
      plots <- wrap_plots(plots, ncol = nrow, nrow = ncol)
      if (!is.null(x = legend) && legend == 'none') {
        plots <- plots & NoLegend()
      }
    } else {
      plots <- wrap_plots(plots, ncol = ncol, nrow = split_by %iff% length(x = levels(x = data$split)))
    }
    if (!is.null(x = legend) && legend == 'none') {
      plots <- plots & NoLegend()
    }
    if (!(is.null(x = keep_scale)) && keep_scale == "all" && !blend) {
      max_feature_value <- max(data[, features])
      min_feature_value <- min(data[, features])
      plots <- suppressMessages(plots & scale_color_gradientn(colors = cols, limits = c(min_feature_value, max_feature_value)))
    }
  }
  return(plots)
}
