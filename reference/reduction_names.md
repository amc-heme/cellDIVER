# Get names of reductions in object

Returns the names of all reductions in a single-cell object.

## Usage

``` r
reduction_names(object, ...)

# S3 method for class 'Seurat'
reduction_names(object)

# S3 method for class 'SingleCellExperiment'
reduction_names(object)

# S3 method for class 'AnnDataR6'
reduction_names(object)

# S3 method for class 'md._core.mudata.MuData'
reduction_names(object)
```

## Arguments

- object:

  a single-cell object. Currently, Seurat and SingleCellExperiment
  objects are supported.

- ...:

  Currently unused.

## Methods (by class)

- `reduction_names(Seurat)`: Seurat objects

- `reduction_names(SingleCellExperiment)`: SingleCellExperiment objects

- `reduction_names(AnnDataR6)`: Anndata objects

  Anndata objects do not have an exclusive structure for reductions.
  Reductions are stored in obsm, but matrices that are not reductions
  may exist in this slot. To use reduction_names with Anndata objects, a
  vector of reductions (python list) must be stored in
  `object.uns["cellDIVER_reductions"]`. This method will return an error
  if this data is not present in the object.

- `reduction_names(md._core.mudata.MuData)`: MuData objects

  For MuData objects, the keys of reductions are the keys of obsm
  matrices in each modality (mod-obsm keys). obsm matrices in each
  modality are not necesarially reductions. It is left to the admin to
  decide which reductions to include.
