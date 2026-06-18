# cellDIVER app

Initializes the main cellDIVER app.

## Usage

``` r
# Option 1: Single-object deployment with 
# an object and an object config file
run_cellDIVER(
 object = "path_to_object",
 config_file = "path_to_config_file.yaml"
 )

# Option 2: Multi-object deployment with
# Browser config file with paths to object, 
# config files for any number of objects
#  
run_cellDIVER(
 browser_config = "path_to_browser_config_file.yaml"
 )
```

## Arguments

- browser_config:

  path to a YAML config file giving browser specific settings. For more
  info on generating this config file, see
  [here](https://amc-heme.github.io/cellDIVER/articles/dataset_setup_walkthrough.html#create-an-app-config-file).

- object_path:

  path to a single-cell object to be configured. Currently, Seurat,
  SingleCellExperiment, and anndata objects are supported. For
  SingleCellExperiment objects with using HDF5 disk-backed storage via
  HDF5Array, `object_path` should be a path to the directory containing
  the se.rds and assays.h5 files for the object.

- config_path:

  path to the config file for the single-cell object. This is generated
  either in the config app via `run_config_app`, or by auto-generating a
  config file via `generate_config_yaml` and editing it by hand. For
  more information on using the config app, see
  [here](https://amc-heme.github.io/cellDIVER/articles/docker.html#step-4).

- enable_metadata_addition:

  when TRUE, users may interactively add metadata to objects in the app.
  This feature is currently unstable, so it must be opted in to by
  setting this value to TRUE.

- port:

  specify a port for launching the browser. This is optional for a
  single deployment but required to run several instances of the browser
  at the same IP address. The port can be any number between 3000:8000,
  except for ports blocked by Google Chrome (for more information on
  this, see [runApp](https://rdrr.io/pkg/shiny/man/runApp.html)).

- host:

  This is passed to
  [`shiny::runApp`](https://rdrr.io/pkg/shiny/man/runApp.html). See the
  documentation of `host` in
  [runApp](https://rdrr.io/pkg/shiny/man/runApp.html) for more info.

- launch_browser:

  This is passed to
  [`shiny::runApp`](https://rdrr.io/pkg/shiny/man/runApp.html) as
  `launch.browser`. See the documentation of `launch.browser` in
  [runApp](https://rdrr.io/pkg/shiny/man/runApp.html) for more info.

- full_stack_trace:

  when TRUE, the full stack trace of errors is logged to the console
  (FALSE by default).

- dev_mode:

  used for development and debugging. When this is TRUE, additional
  logging occurrs, and the status of the app is printed in the UI. This
  is not reccomended to be used outside of development.

## Details

For more information on setting up an cellDIVER deployment, see the
[dataset setup
guide](https://amc-heme.github.io/cellDIVER/articles/dataset_setup_walkthrough.html)
on our website.
