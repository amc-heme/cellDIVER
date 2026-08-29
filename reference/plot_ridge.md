# Single cell ridge plot

Draws a ridge plot of single cell data (gene expression, metrics, PC
scores, etc.)

## Usage

``` r
plot_ridge(
  object,
  features,
  group_by,
  cols = NULL,
  idents = NULL,
  sort = FALSE,
  assay = NULL,
  y_max = NULL,
  same_y_lims = FALSE,
  log = FALSE,
  ncol = NULL,
  slot = NULL,
  stack = FALSE,
  combine = TRUE,
  fill_by = "feature"
)
```

## Arguments

- object:

  A Seurat or SingleCellExperiment object

- features:

  Features to plot (gene expression, metrics, PC scores, anything that
  can be retreived by fetch_data).

- group_by:

  Group (color) cells in different ways. Unlike
  [`Seurat::RidgePlot()`](https://satijalab.org/seurat/reference/RidgePlot.html)
  or
  [`Seurat::VlnPlot()`](https://satijalab.org/seurat/reference/VlnPlot.html),
  this must be defined (SingleCellExperiment objects don't have
  `Idents()` functionality).

- cols:

  Colors to use for plotting

- idents:

  Which levels of the group by variable to include in the plot (default
  is all)

- sort:

  Sort group by levels (on the x-axis) by the average expression of the
  attribute being potted, can also pass 'increasing' or 'decreasing' to
  change sort direction

- assay:

  Name of assay to use, defaults to the active assay

- y_max:

  Maximum y axis value

- same_y_lims:

  Set all the y-axis limits to the same values

- log:

  plot the feature axis on log scale

- ncol:

  Number of columns if multiple plots are displayed

- slot:

  Slot (Seurat objects) or assay (SingleCellExperiment objects) to pull
  expression data from (counts/data for Seurat objects, counts/logcounts
  for SingleCellExperiment objects)

- stack:

  Horizontally stack plots for each feature

- combine:

  Combine plots into a single
  [`patchwork`](https://patchwork.data-imaginist.com/reference/patchwork-package.html)`ed`
  ggplot object. If `FALSE`, return a list of ggplot

- fill_by:

  Color violins/ridges based on either 'feature' or 'ident'

## Value

A
[`patchwork`](https://patchwork.data-imaginist.com/reference/patchwork-package.html)`ed`
ggplot object if `combine = TRUE`; otherwise, a list of ggplot objects

## Examples

``` r
plot_ridge(AML_Seurat, features = "PC_1", group_by = "condensed_cell_type")
#> Error: object 'AML_Seurat' not found
```
