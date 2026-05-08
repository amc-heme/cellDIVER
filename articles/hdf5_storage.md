# HDF5 Storage Formats

The size of single cell objects can make hosting in scExploreR difficult
or impossible on local computing resources. Objects can take tens or
even hundreds of gigabytes of RAM when loaded into memory, depending on
the number of cells. Fortunately, scExploreR supports storage formats
for multiple object types that require only fractions of the data to be
loaded into RAM at once, considerably decreasing the RAM requirements.
Guidelines are provided below for each common object formats to allow
the analysis of datasets with hundreds of thousands or even millions of
cells on a personal computer.

## Anndata Objects

Anndata is the preferred object format for HDF5 disk-based storage, and
it is the easiest to set up. To use HDF5 formatting for an Anndata
object, simply save it via the
[`write()`](https://rdrr.io/r/base/write.html) method in python. If
using the Anndata package in R, use `write_h5ad()`.

``` python
# adata: an Anndata object preprocessed and clustered using Scanpy
adata.write(filename = "output_path.h5ad")
```

``` r

# If saving via the R anndata package
anndata::write_h5ad(adata, filename = "output_path.h5ad")
```

## Seurat Objects

With the release of [Seurat v5](https://satijalab.org/seurat/), Seurat
objects may employ HDF5 storage via BPcells. The differential gene
expression tab can’t be used for Seurat objects with BPcells arrays, but
the plots tab can still be used.

Objects processed with Seurat version 5 should automatically use
BPCells-based storage. Seurat must be updated at least to version 5.0.0
to use this format.

Seurat v5 objects also allow the user to create a “sketch” assay with a
representative subsample of cells. The sketch assay is stored in memory
to optimize for performance, while the full assay is stored on disk.
[See
here](https://satijalab.org/seurat/articles/seurat5_sketch_analysis) for
more information.

## SingleCellExperiment Objects

To use HDF5 storage for SingleCellExperiment objects, save the object
using `saveHDF5SummarizedExperiment()` from the
[HDF5Array](https://bioconductor.org/packages/release/bioc/html/HDF5Array.html)
package.

The `level` parameter determines the compression level when creating the
disk-based matrices. Higher compression levels decrease the amount of
disk space used, but decrease performance. We recommend a compression
level of 3.

``` r

saveHDF5SummarizedExperiment(
    # sce: SingleCellExperiment object
    sce,
    dir = "output_directory/",
    level = 3
    )
```

The output of saveHDF5SummarizedExperiment is a directory rather than a
single file. Make sure to reference the directory when loading the
object into scExploreR, rather than the files in the directory.

## Converting Between Object Formats

See the [scEasy](https://github.com/cellgeni/sceasy/tree/master) package
for efficient means of converting between different SingleCellExperiment
object formats.
