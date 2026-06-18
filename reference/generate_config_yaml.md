# Auto-Generate Config File

This function will automatically generate a basic config file from a
single-cell object. This can greatly speed up setup for large objects
with many metadata variables, but we recommend editing the file in the
config app or manually afterward for the following reasons:

- All metadata variables will be added, in the order they appear in the
  object metadata table. A separate display order may be desired to
  highight the most important metadata variables to end users.

- Display names for metadata are un-set

- Display settings for modalities/assays, including how they appear in
  feature dropdown menus and whether to display a suffix after the assay
  name in plots, are unset.

- Descriptions for individual modalities/assays, metadata, and
  reductions are unset. To load the config file in the config app for
  further editing, supply the file path to `config_path` in
  `run_config`. After loading the config app, go to the "..." menu on
  the top right, and select "Load Config File". Save changes via "Save
  Config File" when you are finished editing.

## Usage

``` r
auto_config(
 # Replace with path to your object
 object = path_to_your_object,
 output_yaml = output_config_path,
 object_name = "Test Single-Cell Object",
 object_description = "This is a test object",
 genes_assay = "RNA",
 include_numeric_metadata = TRUE,
 is_HDF5SummarizedExperiment = FALSE,
 sample_level_var = NULL
 )
```

## Arguments

- object:

  a single-cell object to be configured for use in cellDIVER (not the
  path to the object). Currently, Seurat, SingleCellExperiment, and
  anndata objects are supported.

- output_yaml:

  path specifying where the output config file should be saved. This
  should end in ".yaml".

- object_name:

  a single-length character vector with the display name of the object.
  This will be displayed to end users in the app as entered.

- object_description:

  a single-length character vector with a short description of the
  object. This will be displayed to end users in the dataset selection
  menu. This should be a few sentences long, and include object details
  relevant to end-users (i.e. brief descripton of methods, number of
  cells/samples in object, etc.).

- is_HDF5SummarizedExperiment:

  Set this to TRUE to load an HDF5-enabled SingleCellExperiment object
  saved via saveHDF5SummarizedExperiment. When loading an HDF5-enabled
  object, set the object_path to the directory of the HDF5-enabled
  object, created when saving the object via
  HDF5Array:saveHDF5SummarizedExperiment.

- HDF5_prefix:

  When loading an HDF5-backed SingleCellExperiment HDF5_prefix is passed
  to the prefix parameter of HDF5Array::loadHDF5SummarizedExperiment to
  specify the prefixes for the se.rds and assays.h5 files.

- include_numeric_metadata:

  If `TRUE`, numeric metadata in the object will searchable in cellDIVER
  for plotting and subsetting. The default setting is `TRUE`.

- genes_assay:

  single-length character vector giving the name of the genes
  assay/experiment/modality in the object. If undefined, the first assay
  in the object will be used. The name should be entered using the
  following convention:

  - Seurat objects: use the name of the assay as it appears in
    `names(object@assays)`.

  - SingleCellExperiment objects: use the name of the experiment as it
    appears in
    [`SingleCellExperiment::altExpNames()`](https://rdrr.io/pkg/SingleCellExperiment/man/altExps.html)
    or
    [`SingleCellExperiment::mainExpName()`](https://rdrr.io/pkg/SingleCellExperiment/man/altExps.html)

  - anndata objects: use either "X" to specify the modality measured in
    X, or enter the name of a matrix stored in the `obsm` slot.

- sample_level_var:

  a sample-level metadata variable used to construct sample-level pie
  charts in the plots tab. This should be a sample or patient ID, for
  example. If this is not provided (the default), pie charts will not
  appear in cellDIVER.

## Value

A config file is generated at the path provided to `output_yaml`.
Nothing is returned from the function.
