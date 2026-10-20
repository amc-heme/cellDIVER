# cellDIVER 1.0.0.9000

## New features

* Hexbin plots. Two new plot types, HexDensity and HexFeature, bin cells on
  the projection into hexagons colored by cell count or by the mean, median or
  sum of a feature's values (via the `hextant` package, now in `Imports`).
  They use the app's continuous palette, with the lightgrey-to-blue default.

* Zero-centered diverging palettes for feature, dot and hexbin feature plots:
  a three-color scale with its midpoint at zero.

* Color scale limits for every plot with a continuous color scale (feature,
  hexbin feature, hexbin density and dot plots): a two-handle slider sets the
  values at which the end colors are placed, bounded by the values shown on
  the plot; values outside the limits are drawn in the end colors. Diverging
  palettes get a "symmetric about zero" option.

* Every continuous palette is also offered in a reversed form, listed next to
  the original.

* Differential expression can be run on several assays at once. The DGE tab
  gains an "Assays to Analyze" selector; results are combined into one table
  with an `assay` column that can be filtered.

* "Choose Projection" menus now have an info tooltip linking to the
  documentation.

## Breaking change

* `run_cellDIVER()` and `run_config()` no longer attach their dependencies to
  the caller's search path. They previously called `library()` on roughly 25
  packages (shiny, Seurat, dplyr, ggplot2 and others) as a side effect of being
  called, which changed the global environment of whoever launched the app.
  Code that relied on that — for example calling `DimPlot()` at the console
  after closing the app — must now attach the package itself, or qualify the
  call as `Seurat::DimPlot()`. The apps themselves are unaffected.

## Bug fixes

* Package code now declares its imports in `NAMESPACE`. 28 of the 40 packages
  in `Imports` had no import directive, so every cellDIVER function depended on
  `run_cellDIVER()` having attached its dependencies first. Calling anything
  outside a running app failed with `could not find function`; for example
  `cellDIVER:::load_config(path)` could not run at all.

* `adt_threshold_assay()` and `update_object_metadata()` now work for
  `SingleCellExperiment` objects. They called `assay()`, `colData<-` and
  `altExp<-` unqualified, so both failed outside a running app.

* Plot download buttons now produce a file. The download control sits inside a
  collapsed dropdown, and Shiny suspends outputs whose element is hidden, so
  the server never issued a download URL and the button stayed disabled.

* `make_subset()` raised `could not find function "error"` instead of reporting
  an unrecognised numeric filter mode.

* `two_column_layout()` raised `object 'left_column' not found` on every call,
  from a misspelled parameter, and emitted an invalid `width: ,50,%;` style.

* Reading `.h5ad` and `.h5mu` objects now reports clearly when `reticulate` or
  `anndata` is missing, instead of failing with a bare
  `there is no package called 'anndata'`. Both are declared under `Suggests`.

## Testing

* Added a consolidated `testthat` suite — unit, module and browser tests — and
  a `Tests` GitHub Actions workflow that gates merges on it.
