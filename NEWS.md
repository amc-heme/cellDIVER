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
  palettes get a "symmetric about zero" option. The limits can also be
  expressed as quantiles (percentiles of the plotted values), and on feature
  and hexbin feature plots can be applied to the underlying per-cell values
  before binning or plotting instead of to the color scale (requires hextant
  0.1.0 for hexbin plots).

* Every continuous palette is also offered in a reversed form, listed next to
  the original.

* Differential expression can be run on several assays at once. The DGE tab
  gains an "Assays to Analyze" selector; results are combined into one table
  with an `assay` column that can be filtered.

* "Choose Projection" menus now have an info tooltip linking to the
  documentation.

## Dependencies

* Seurat 5.5.0 or newer is now required. Seurat 5.5.0 added ggplot2 4
  compatibility to its plotting code, which is why earlier releases pinned
  `ggplot2 (<= 3.5.2)`. The cellDIVER test suites (unit, module and browser)
  pass on Seurat 5.6.0 with ggplot2 4.0.3.

* `presto` is now declared as a dependency with `github::amc-heme/presto` in
  `Remotes`. scDE calls presto for differential expression, and upstream
  presto still calls `GetAssayData(slot =)`, which SeuratObject 5.4.0 rejects
  as defunct, so every DGE test fails on current Seurat. The fork uses
  `layer =` instead. Install from the fork (or let `Remotes` do it) until that
  change is merged upstream.

## Breaking change

* `run_cellDIVER()` and `run_config()` no longer attach their dependencies to
  the caller's search path. They previously called `library()` on roughly 25
  packages (shiny, Seurat, dplyr, ggplot2 and others) as a side effect of being
  called, which changed the global environment of whoever launched the app.
  Code that relied on that — for example calling `DimPlot()` at the console
  after closing the app — must now attach the package itself, or qualify the
  call as `Seurat::DimPlot()`. The apps themselves are unaffected.

## Bug fixes

* Dot plots no longer trigger the `aes_string()` deprecation warning from
  ggplot2; the mappings use the `.data` pronoun.

* The feature picker (and other server-side selectize inputs) stayed empty on
  Shiny 1.12 and later. The plots and DGE tab bodies were rendered lazily on
  the tab switch that also sends their `updateSelectizeInput(server = TRUE)`
  choices, and newer Shiny delivers that update before the body exists. The
  tab bodies are now rendered up front.

* Tooltips did not work: shinyBS only registers its assets when attached with
  `library()`, which cellDIVER no longer does, so every `bsTooltip()` failed
  in the browser with "shinyBS is not defined". The resource path is now
  registered at startup.

* Plot download handlers logged "argument is of length zero" for every plot
  on startup, because their content type was computed from the file type
  input before it existed. The type is now inferred from the filename and the
  file type defaults to png.

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
