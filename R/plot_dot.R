#' Dot plot visualization
#'
#' Intuitive way of visualizing how feature expression changes across different
#' identity classes (clusters). The size of the dot encodes the percentage of
#' cells within a class, while the color encodes the AverageExpression level
#' across all cells within a class (blue is high).
#'
#' @param object a Seurat object or a SingleCellExperiment object
#' @param group_by The name of a metadata variable to group the cells by. Unlike
#'  \code{Seurat::DotPlot()}, this must be defined.
#' @param features Input vector of features, or named list of feature vectors
#' if feature-grouped panels are desired (replicates the functionality of the
#' old SplitDotPlotGG)
#' @param split_by The name of a metadata variable to split groups by. Each
#' combination of unique values in the group_by and split_by variables for
#' which cells exist will appear on the y-axis of the plot.
#' @param cols Colors to plot. May be the name of a palette from
#' \code{RColorBrewer::brewer.pal.info}, a pair of colors defining a gradient,
#' or 3+ colors defining multiple gradients (if split_by is set).
#' @param col_min Minimum scaled average expression threshold (everything
#' smaller will be set to this)
#' @param col_max Maximum scaled average expression threshold (everything larger
#' will be set to this)
#' @param dot_min The fraction of cells at which to draw the smallest dot
#' (default is 0). All cell groups with less than this expressing the given
#' gene will have no dot drawn.
#' @param dot_scale Scale the size of the points, similar to cex
#' @param idents Identity classes to include in plot (default is all)
#' @param cluster_idents Whether to order identities by hierarchical clusters
#' based on given features, default is FALSE
#' @param scale Determine whether the data is scaled, TRUE for default
#' @param scale_by Scale the size of the points by 'size' or by 'radius'
#' @param scale_min Set lower limit for scaling, use NA for default
#' @param scale_max Set upper limit for scaling, use NA for default
#'
#' @return A ggplot object
#'
#' @importFrom grDevices colorRampPalette
#' @importFrom cowplot theme_cowplot
#' @importFrom ggplot2 ggplot aes_string geom_point scale_size scale_radius
#' theme element_blank labs scale_color_identity scale_color_distiller
#' scale_color_gradient guides guide_legend guide_colorbar
#' facet_grid unit
#' @importFrom scattermore geom_scattermore
#' @importFrom stats dist hclust
#' @importFrom RColorBrewer brewer.pal.info
#'
#' @keywords internal
#'
#' @export
#'
#' @seealso \code{RColorBrewer::brewer.pal.info}
#'
#' @examples
#' plot_dot(
#'      AML_h5ad(), 
#'      group_by = "condensed_cell_type", 
#'      features = c("X_UNG", "X_GAPDH", "X_CCR5")
#'    )
#'
plot_dot <-
  function(
    object,
    group_by,
    features,
    cols = c("lightgrey", "blue"),
    col_min = -2.5,
    col_max = 2.5,
    dot_min = 0,
    dot_scale = 6,
    idents = NULL,
    split_by = NULL,
    cluster_idents = FALSE,
    scale = TRUE,
    scale_by = 'radius',
    scale_min = NA,
    scale_max = NA
  ){
    # 1. Process Input Parameters
    ## 1.1. split_colors based on split_by, cols entry
    # colors each split separately (only works for categorical
    # palettes; does not work with RColorBrewer palettes)
    split_colors <-
      !is.null(x = split_by) && !any(cols %in% rownames(x = brewer.pal.info))
    
    ## 1.2. ggplot2 scale_* function
    # Determine which ggplot2 scale_* function to use for scaling the dots
    scale_func <-
      switch(
        EXPR = scale_by,
        'size' = scale_size,
        'radius' = scale_radius,
        stop("'scale_by' must be either 'size' or 'radius'")
      )
    
    ## 1.3. Split features into groups if a named list is passed to `features`
    feature_groups <- NULL
    if (is.list(features) | any(!is.na(names(features)))) {
      feature_groups <-
        unlist(
          sapply(
            X = 1:length(features),
            FUN = function(x) {
              return(
                rep(
                  x = names(x = features)[x],
                  each = length(features[[x]])
                )
              )
            }
          )
        )
      
      if (any(is.na(x = feature_groups))) {
        warning(
          "Some feature groups are unnamed.",
          call. = FALSE,
          immediate. = TRUE
        )
      }
      features <- unlist(x = features)
      names(x = feature_groups) <- features
    }
    
    # 2. Fetch data
    ## 2.1. Determine cells to pull data for based on `idents`
    cells <-
      if (is.null(idents)){
        # If `idents` is NULL, pull all cells
        SCUBA::get_all_cells(object)
      } else {
        # Otherwise, use fetch_cells method to specify included cells
        fetch_cells(
          object = object,
          meta_var = group_by,
          meta_levels = idents
        )
      }
    
    ## 2.2. group_by data
    group_by_data <-
      fetch_metadata(
        object,
        vars = group_by,
        cells = cells,
        return_class = "vector"
      )
    
    # Convert data to factor if it is not already
    if (!is.factor(x = group_by_data)) {
      group_by_data <- factor(x = group_by_data)
    }
    
    ## 2.3. Expression data for each feature
    expr_data <-
      fetch_data(
        object = object,
        vars = features,
        cells = cells,
        # Seurat::DotPlot always pulls from the "data" layer. plot_dot exhibits
        # similar behavior by pulling the layer equivalent to "data" using
        # `default_layer`.
        layer = default_layer(object)
      )
    
    # 3. Add group_by metadata to table
    expr_data$id <- group_by_data
    
    # Store the group_by factor levels in `id_levels` and convert back
    # to a vector
    id_levels <- levels(x = expr_data$id)
    expr_data$id <- as.vector(x = expr_data$id)
    
    # 4. Handle split_by metadata if defined
    if (!is.null(x = split_by)) {
      ## 4.1. Pull split_by metadata
      splits <-
        fetch_metadata(
          object = object,
          vars = split_by,
          cells = cells,
          return_class = "vector"
        )
      
      ## 4.2. Assign colors to each unique split
      if (split_colors) {
        if (length(x = unique(x = splits)) > length(x = cols)) {
          stop("Not enough colors for the number of groups")
        }
        cols <- cols[1:length(x = unique(x = splits))]
        names(x = cols) <- unique(x = splits)
      }
      
      ## 4.3. Combine splits with group by levels
      expr_data$id <- paste(expr_data$id, splits, sep = '_')
      unique_splits <- unique(x = splits)
      # Create labels for each group_by and split_by level with an underscore
      # (each combo reads group_by[i]_split_by[j] for all combinations of i
      # group_by and j split_by levels), *for which there are cells*
      id_levels <-
        paste0(
          rep(x = id_levels, each = length(x = unique_splits)),
          "_",
          rep(x = unique(x = splits), times = length(x = id_levels))
        )
    }
    
    # Replace any literal NA values with string NA
    # Downstream code will throw an error if NAs exist in group by or split
    # by metadata
    expr_data$id <-
      case_when(
        is.na(expr_data$id) ~ "NA",
        TRUE ~ expr_data$id
      )
    
    # 5. Form statistics for Avg. expression, pct. expressed
    ## 5.1. Compute statistics for each group
    # Groups may be levels of the group_by variable, or group_by-split_by
    # combinations if split_by is enabled. Results are stored in a list
    # If there are multiple features, each element will be a vector with the
    # expression of each feature for the given group
    plot_data <- lapply(
      X = unique(x = expr_data$id),
      FUN = function(ident){
        # Pull data for computing statistics
        # Exclude last column of expr_data ("id" column)
        data_use <-
          expr_data[
            expr_data$id == ident,
            1:(ncol(x = expr_data) - 1),
            drop = FALSE
          ]
        
        ## 5.1.1. Average expression
        avg_exp <-
          apply(
            X = data_use,
            # Apply function by column
            MARGIN = 2,
            FUN = function(x) {
              return(mean(x = expm1(x = x)))
            }
          )
        
        ### 5.1.2 Percent of cells in group expressing feature
        pct_exp <-
          apply(
            X = data_use,
            MARGIN = 2,
            # Uses Seurat::PercentAbove()
            FUN = PercentAbove,
            # "threshold" is an argument of PercentAbove
            threshold = 0
          )
        
        return(
          list(
            avg_exp = avg_exp,
            pct_exp = pct_exp
          )
        )
      }
    )
    names(x = plot_data) <- unique(x = expr_data$id)
    
    ## 5.2. Cluster groups if `cluster_idents` is TRUE
    # Clustering is performed based on expression statistics for the currently
    # plotted set of features
    if (cluster_idents) {
      mat <- do.call(
        what = rbind,
        args = lapply(X = plot_data, FUN = unlist)
      )
      mat <- scale(x = mat)
      id_levels <- id_levels[hclust(d = dist(x = mat))$order]
    }
    
    ## 5.3. Construct dataframe from list of expression statistics
    # Return is a list of dataframes, one dataframe per group
    plot_data <- lapply(
      X = names(x = plot_data),
      FUN = function(x) {
        data_use <- as.data.frame(x = plot_data[[x]])
        # Add feature name as a column
        data_use$features_plot <- rownames(x = data_use)
        # Add column for the group ID
        data_use$id <- x
        return(data_use)
      }
    )
    
    ## 5.4. Combine list of dataframes produced above
    plot_data <- do.call(what = 'rbind', args = plot_data)
    
    # Convert the new "id" column in the dataframe to a factor
    if (!is.null(x = id_levels)) {
      plot_data$id <- factor(x = plot_data$id, levels = id_levels)
    }
    
    ## 5.5. Compute number of groups in table
    # Warn if the number of groups is 1, or less than 5
    ngroup <- length(x = levels(x = plot_data$id))
    
    if (ngroup == 1) {
      scale <- FALSE
      warning(
        "Only one identity present, the expression values will be not scaled",
        call. = FALSE,
        immediate. = TRUE
      )
    } else if (ngroup < 5 & scale) {
      warning(
        "Scaling data with a low number of groups may produce misleading results",
        call. = FALSE,
        immediate. = TRUE
      )
    }
    
    ## 5.6. Scale expression across features
    avg_exp_scaled <-
      sapply(
        X = unique(x = plot_data$features_plot),
        FUN = function(x) {
          # Pull average expression column and subset to feature x
          data_use <- plot_data[plot_data$features_plot == x, 'avg_exp']
          if (scale) {
            # Scale data (z-score)
            data_use <- scale(x = data_use)
            # Any values with a z-score outside of col_min and col_max will be
            # normalized to those values
            data_use <- MinMax(data = data_use, min = col_min, max = col_max)
          } else {
            # If `scale` is FALSE, perform a log(x+1) transformation
            data_use <- log1p(x = data_use)
          }
          return(data_use)
        }
      )
    
    ## 5.7. Transform average expression to a vector
    avg_exp_scaled <- as.vector(x = t(x = avg_exp_scaled))
    
    ## 5.8. If split_colors, "bin" average expression into 20 bins
    if (split_colors) {
      # Max value will be 20, min value will be 1, and all other values will be
      # binned based on their value
      avg_exp_scaled <- as.numeric(x = cut(x = avg_exp_scaled, breaks = 20))
    }
    
    ## 5.9. Add scaled data to the plot_data dataframe and convert to factor
    plot_data$avg_exp_scaled <- avg_exp_scaled
    plot_data$features_plot <- factor(
      x = plot_data$features_plot,
      levels = colnames(expr_data)
    )
    
    ## 5.10. Form final percent expression data
    # Add NAs for percent expression values beneath dot_min
    plot_data$pct_exp[plot_data$pct_exp < dot_min] <- NA
    # Convert percent expression from fraction to percentage
    plot_data$pct_exp <- plot_data$pct_exp * 100
    
    # 6. Apply color scale
    ## 6.1. Color by splits, if split_colors is TRUE
    if (split_colors) {
      splits_use <- vapply(
        X = as.character(x = plot_data$id),
        FUN = gsub,
        # Return template: character vector of the same length as X
        FUN.VALUE = character(length = 1L),
        # Parameters of gsub()
        pattern =  paste0(
          '^((',
          paste(sort(x = levels(x = object), decreasing = TRUE), collapse = '|'),
          ')_)'
        ),
        replacement = '',
        USE.NAMES = FALSE
      )
      
      # Color for each dot: use colorRampPalette to determine the color (using
      # a continuous scale between grey and the color entered, with 20 steps)
      plot_data$colors <- mapply(
        FUN = function(color, value) {
          return(
            colorRampPalette(colors = c('grey', color))(20)[value]
          )
        },
        color = cols[splits_use],
        value = avg_exp_scaled
      )
    }
    
    ## 6.2. Determine color scale
    # If there are splits, use 20-level binned scale based on average
    # expression (feature-wide).
    # If not (default behavior), apply colors using ggplot2 scales
    color_by <-
      ifelse(
        test = split_colors,
        yes = 'colors',
        no = 'avg_exp_scaled'
      )
    
    # 7. Apply min/max cutoffs for *percent expression*
    if (!is.na(x = scale_min)) {
      plot_data[plot_data$pct_exp < scale_min, 'pct_exp'] <- scale_min
    }
    if (!is.na(x = scale_max)) {
      plot_data[plot_data$pct_exp > scale_max, 'pct_exp'] <- scale_max
    }
    
    # 8. Add data for feature groups to the table (if defined by the user)
    if (!is.null(x = feature_groups)) {
      plot_data$feature_groups <- factor(
        x = feature_groups[plot_data$features_plot],
        levels = unique(x = feature_groups)
      )
    }
    
    # 9. Plot data
    plot <-
      ggplot(
        data = plot_data,
        # aes_string is deprecated in the current ggplot2 version
        mapping = aes_string(x = 'features_plot', y = 'id')
      ) +
      geom_point(mapping = aes_string(size = 'pct_exp', color = color_by)) +
      scale_func(range = c(0, dot_scale), limits = c(scale_min, scale_max)) +
      theme(axis.title.x = element_blank(), axis.title.y = element_blank()) +
      guides(size = guide_legend(title = 'Percent Expressed')) +
      labs(
        x = 'Features',
        y = ifelse(test = is.null(x = split_by), yes = 'Identity', no = 'Split Identity')
      ) +
      theme_cowplot()
    
    # Create facets for feature groups, if present
    if (!is.null(x = feature_groups)) {
      plot <- plot + facet_grid(
        facets = ~feature_groups,
        scales = "free_x",
        space = "free_x",
        switch = "y"
      ) + theme(
        panel.spacing = unit(x = 1, units = "lines"),
        strip.background = element_blank()
      )
    }
    if (split_colors) {
      # scale_color_identity applies colors that were defined based on the
      # 20-bin scale
      plot <- plot + scale_color_identity()
    } else if (length(x = cols) == 1) {
      plot <- plot + scale_color_distiller(palette = cols)
    } else {
      plot <- plot + scale_color_gradient(low = cols[1], high = cols[2])
    }
    if (!split_colors) {
      plot <- plot + guides(color = guide_colorbar(title = 'Average Expression'))
    }
    return(plot)
  }
