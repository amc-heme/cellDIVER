# Changelog

## cellDIVER 1.0.0.9000

### Breaking change

- [`run_cellDIVER()`](https://amc-heme.github.io/cellDIVER/reference/run_cellDIVER.md)
  and
  [`run_config()`](https://amc-heme.github.io/cellDIVER/reference/run_config.md)
  no longer attach their dependencies to the caller’s search path. They
  previously called [`library()`](https://rdrr.io/r/base/library.html)
  on roughly 25 packages (shiny, Seurat, dplyr, ggplot2 and others) as a
  side effect of being called, which changed the global environment of
  whoever launched the app. Code that relied on that — for example
  calling `DimPlot()` at the console after closing the app — must now
  attach the package itself, or qualify the call as
  [`Seurat::DimPlot()`](https://satijalab.org/seurat/reference/DimPlot.html).
  The apps themselves are unaffected.

### Bug fixes

- Package code now declares its imports in `NAMESPACE`. 28 of the 40
  packages in `Imports` had no import directive, so every cellDIVER
  function depended on
  [`run_cellDIVER()`](https://amc-heme.github.io/cellDIVER/reference/run_cellDIVER.md)
  having attached its dependencies first. Calling anything outside a
  running app failed with `could not find function`; for example
  `cellDIVER:::load_config(path)` could not run at all.

- `adt_threshold_assay()` and `update_object_metadata()` now work for
  `SingleCellExperiment` objects. They called `assay()`, `colData<-` and
  `altExp<-` unqualified, so both failed outside a running app.

- Plot download buttons now produce a file. The download control sits
  inside a collapsed dropdown, and Shiny suspends outputs whose element
  is hidden, so the server never issued a download URL and the button
  stayed disabled.

- [`make_subset()`](https://amc-heme.github.io/cellDIVER/reference/make_subset.md)
  raised `could not find function "error"` instead of reporting an
  unrecognised numeric filter mode.

- `two_column_layout()` raised `object 'left_column' not found` on every
  call, from a misspelled parameter, and emitted an invalid
  `width: ,50,%;` style.

- Reading `.h5ad` and `.h5mu` objects now reports clearly when
  `reticulate` or `anndata` is missing, instead of failing with a bare
  `there is no package called 'anndata'`. Both are declared under
  `Suggests`.

### Testing

- Added a consolidated `testthat` suite — unit, module and browser tests
  — and a `Tests` GitHub Actions workflow that gates merges on it.
