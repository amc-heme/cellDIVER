# Guidelines for Preparing objects for scExploreR

This vignette details the requirements and guidelines for loading
single-cell data in scExploreR.

## What objects can I upload to scExploreR?

scExploreR works with many common object formats for single-cell data.
Functionality is largely similar between object classes, but there are
some differences. A table with the supported object classes summarizing
the differences in functionality between each class is given below.

|  | **Seurat** | **Seurat v5 + BPCells** | **SingleCellExperiment** | **Anndata** |
|----|----|----|----|----|
| **Plotting** |  |  |  |  |
| Full plotting capabilities |  |  |  |  |
| Consistent Style |  |  |  |  |
| **Subsetting** |  |  |  |  |
| Based on categorical Metadata |  |  |  |  |
| Based on feature expression |  |  |  |  |
| Advanced code-based subsetting |  |  |  |  |
| **Differential Gene Expression** |  |  |  |  |
| Can perform DGE |  |  |  |  |
| Results show AUC value |  |  |  |  |

##### Note

The function used to perform DGE varies by object class. The functions
used for each object class are listed below.

|  |  |
|----|----|
| **Seurat** | [presto::wilcoxauc](https://immunogenomics.github.io/presto/reference/wilcoxauc.html) |
| **Seurat v5 + BPCells** | [BPcells::marker_features](https://bnprks.github.io/BPCells/reference/marker_features.html) |
| **anndata** | [scanpy.tl.rank_genes_groups](https://scanpy.readthedocs.io/en/stable/generated/scanpy.tl.rank_genes_groups.html) |

scExploreR uses the most performant wilcoxon rank sum function for each
object class. DGE is not available for SingleCellExperiment objects
because there is no known package that can process both the standard
sparse matrices and the HDF5-enabled DelayedArray matrices in these
objects.

## How should objects be processed?

Generally speaking, objects should be be pre-processed before loading
into the browser. Pre-processing steps encompass operations such as
those outlined in the
[Seurat](https://satijalab.org/seurat/articles/pbmc3k_tutorial) and
[Scanpy](https://scanpy.readthedocs.io/en/stable/tutorials/basics/clustering.html)
docs.

Objects should have the have the following characteristics below before
loading into the browser. Objects lacking these properties may load in
scExploreR, but key functionality may be limited, and unexpected
behavior may be observed.

Objects should have:

- At least one reduction.
- Feature expression data for at least one modality (assay in Seurat).
  The browser is designed for use with both unimodal or multimodal data.
- Normalized count data for each modality included.
- No `NA` values in the count/normalized count data (or `NaN` values in
  anndata object format).

##### Note

If `NA` literals exist in feature expression data, the config app will
stop loading the object and return an error. We did this because `NA`
values in expression data are known to cause issues in the app. If you
are using software that outputs count data that evaluates to `NA` in R,
or `NaN` in Python, we recommend changing those values to `0`. If this
causes issues with the software you are using, please [file an
issue](https://github.com/amc-heme/scExploreR/issues) and mention the
software name in the issue title and text.

- No `NA` values in metadata. `NA` values for categorical metadata
  should be renamed from literal `NA` values to character values
  (`"NA"`, `"Undefined"`, `"Unspecified"`, etc.) before loading the
  object into the app.

If you wish to load an object that doesn’t meet the conditions above,
feel free to [file an
issue](https://github.com/amc-heme/scExploreR/issues) describing your
situation. Please search the issue board for similar situations to yours
before filing an issue.

### Additional Preprocessing for anndata objects

For anndata objects, scExploreR requires additional information to
locate matrices corresponding to reductions and multimodal data. Unlike
Seurat and SingleCellExperment objects, the anndata class does not
specify storage locations exclusive to modalities and reductions.
Instead, both are stored in `obsm`.

For scExploreR to properly locate additional modalities and reductions,
you must specify the names of the matrices in `obsm` corresponding to
these data types. This is done by adding two python lists to `uns`:
`uns.scExploreR_assays` for additional modalities, and
`uns.scExploreR_reductions` for reductions.

For example, if you have an object with surface protein measurements in
`obsm.protein`, and UMAP and PCA reductions in `obsm.X_uamp` and
`obsm.X_pca`, respectively, you would enter the following in uns
(replacing `object` with the name of the variable you assigned to your
object):

    object.uns["scExploreR_reductions"] = ["X_umap", "X_pca"]

    object.uns["scExploreR_assays"] = ["protein"]

For an additional example, see the [app setup walkthrough
vignette](https://amc-heme.github.io/scExploreR/articles/dataset_setup_walkthrough.html#anndata_uns_requirements).
