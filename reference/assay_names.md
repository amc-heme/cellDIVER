# Get names of assays/experiments in object

Returns the names of all assays in a single-cell object. For Seurat
objects, returns assay names, and for SingleCellExperiment objects
"experiments", the equivalent of assays, are returned.

## Usage

``` r
assay_names(object, ...)

# S3 method for class 'Seurat'
assay_names(object)

# S3 method for class 'SingleCellExperiment'
assay_names(object)

# S3 method for class 'AnnDataR6'
assay_names(object)

# S3 method for class 'md._core.mudata.MuData'
assay_names(object)
```

## Arguments

- object:

  a single-cell object. Currently, Seurat and SingleCellExperiment
  objects are supported.

- ...:

  Currently unused.

## Methods (by class)

- `assay_names(Seurat)`: Seurat objects

- `assay_names(SingleCellExperiment)`: SingleCellExperiment objects

- `assay_names(AnnDataR6)`: Anndata objects

  Anndata objects do not have an exclusive structure for
  modalities/assays. Additional modalities are stored in obsm, but this
  slot is not specific to modalities. To use assay_names with Anndata
  objects, a vector of assays (python list) must be stored in
  `object.uns["cellDIVER_assays"]`. This method will return an error if
  this data is not present in the object.

- `assay_names(md._core.mudata.MuData)`: MuData objects
