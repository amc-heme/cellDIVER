# cellDIVER config app

run_config() will launch a Shiny app used to configure datasets for use
in the main browser.

## Usage

``` r
run_config(
  object_path,
  config_path = NULL,
  is_HDF5SummarizedExperiment = FALSE,
  HDF5_prefix = "",
  dev_mode = FALSE
)
```

## Arguments

- object_path:

  path to a single-cell object to be configured. Currently, Seurat,
  SingleCellExperiment, and anndata objects are supported. For
  SingleCellExperiment objects with using HDF5 disk-backed storage via
  HDF5Array, `object_path` should be a path to the directory containing
  the se.rds and assays.h5 files for the object.

- config_path:

  optional: if provided, the data from this file will be loaded when the
  user selects "load config file" in the config app. This should be a
  YAML file, though .rds files from versions 0.4.0 and earlier will also
  be accepted.

- is_HDF5SummarizedExperiment:

  Set this to TRUE to load an HDF5-enabled SingleCellExperiment object
  saved via
  [HDF5Array::saveHDF5SummarizedExperiment](https://rdrr.io/pkg/HDF5Array/man/saveHDF5SummarizedExperiment.html).
  When loading an HDF5-enabled object, set the object_path to the
  directory of the HDF5-enabled object, created when saving the object
  via `HDF5Array:saveHDF5SummarizedExperiment`.

- HDF5_prefix:

  When loading an HDF5-backed SingleCellExperiment HDF5_prefix is passed
  to the prefix parameter of
  [HDF5Array::loadHDF5SummarizedExperiment](https://rdrr.io/pkg/HDF5Array/man/saveHDF5SummarizedExperiment.html)
  to specify the prefixes for the se.rds and assays.h5 files.

- dev_mode:

  Used only for development. If TRUE, the server values for each option
  chosen by the user will be printed at the bottom of the "general" tab.
