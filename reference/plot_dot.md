# Dot plot visualization

Intuitive way of visualizing how feature expression changes across
different identity classes (clusters). The size of the dot encodes the
percentage of cells within a class, while the color encodes the
AverageExpression level across all cells within a class (blue is high).

## Usage

``` r
plot_dot(
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
  scale_by = "radius",
  scale_min = NA,
  scale_max = NA
)
```

## Arguments

- object:

  a Seurat object or a SingleCellExperiment object

- group_by:

  The name of a metadata variable to group the cells by. Unlike
  [`Seurat::DotPlot()`](https://satijalab.org/seurat/reference/DotPlot.html),
  this must be defined.

- features:

  Input vector of features, or named list of feature vectors if
  feature-grouped panels are desired (replicates the functionality of
  the old SplitDotPlotGG)

- cols:

  Colors to plot. May be the name of a palette from
  [`RColorBrewer::brewer.pal.info`](https://rdrr.io/pkg/RColorBrewer/man/ColorBrewer.html),
  a pair of colors defining a gradient, or 3+ colors defining multiple
  gradients (if split_by is set).

- col_min:

  Minimum scaled average expression threshold (everything smaller will
  be set to this)

- col_max:

  Maximum scaled average expression threshold (everything larger will be
  set to this)

- dot_min:

  The fraction of cells at which to draw the smallest dot (default is
  0). All cell groups with less than this expressing the given gene will
  have no dot drawn.

- dot_scale:

  Scale the size of the points, similar to cex

- idents:

  Identity classes to include in plot (default is all)

- split_by:

  The name of a metadata variable to split groups by. Each combination
  of unique values in the group_by and split_by variables for which
  cells exist will appear on the y-axis of the plot.

- cluster_idents:

  Whether to order identities by hierarchical clusters based on given
  features, default is FALSE

- scale:

  Determine whether the data is scaled, TRUE for default

- scale_by:

  Scale the size of the points by 'size' or by 'radius'

- scale_min:

  Set lower limit for scaling, use NA for default

- scale_max:

  Set upper limit for scaling, use NA for default

## Value

A ggplot object

## See also

[`RColorBrewer::brewer.pal.info`](https://rdrr.io/pkg/RColorBrewer/man/ColorBrewer.html)

## Examples

``` r
plot_dot(
     AML_h5ad(), 
     group_by = "condensed_cell_type", 
     features = c("X_UNG", "X_GAPDH", "X_CCR5")
   )
#> Error in AML_h5ad(): could not find function "AML_h5ad"
```
