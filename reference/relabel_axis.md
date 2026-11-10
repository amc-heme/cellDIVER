# Re-label axis behavior

Code to relabel the axis in the expr_plot function. The default axis
text is "Feature Expression", and it changes or is removed based on the
type of data being plotted. If the data is a reduction, the label
changes to "Embeddings Value". If the data comes from a source that is
not an assay or a reduction, the label is removed. The label changed
(x-axis or y-axis) depends on the type of plot being created by
expr_plot (violin, dot, etc.).

## Usage

``` r
relabel_axis(object, feature, ...)

# S3 method for class 'Seurat'
relabel_axis(object, feature)

# S3 method for class 'SingleCellExperiment'
relabel_axis(object, feature)

# S3 method for class 'AnnDataR6'
relabel_axis(object, feature)

# S3 method for class 'md._core.mudata.MuData'
relabel_axis(object, feature)

# S3 method for class 'scuba_source'
relabel_axis(object, feature, ...)
```

## Arguments

- object:

  a single-cell object. Currently, Seurat, SingleCellExperiment,
  Anndata, and MuData objects are supported.

- feature:

  the feature being plotted. Only one feature should be passed to this
  generic at once. For multi-feature plot, this generic should be ran
  separately on each feature, and the changes in labeling applied to
  each individual plot (patchwork object) iteratively.

## Details

For Anndata objects, the label will always show as "Feature Expression",
unless the feature plotted is a metadata variable, in which case no
label will be drawn.

Labels can be manually added after plot creation with ggplot2::labs() in
the event a different label is more appropriate for the feature being
plotted.

## Methods (by class)

- `relabel_axis(Seurat)`: Seurat objects

- `relabel_axis(SingleCellExperiment)`: SingleCellExperiment objects

- `relabel_axis(AnnDataR6)`: Anndata objects

- `relabel_axis(md._core.mudata.MuData)`: MuData objects

- `relabel_axis(scuba_source)`: File-backed SCUBA objects: delegated to
  SCUBA, which knows the modalities and reductions of the file
