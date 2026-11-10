#' Load a single-cell object for the browser or the config app
#'
#' One loader for every object format cellDIVER serves, shared by
#' `run_cellDIVER()` and `run_config()`:
#'
#' - `.rds` files are read with `readRDS()` (Seurat or SingleCellExperiment).
#' - HDF5-backed SingleCellExperiment directories are read with
#'   `HDF5Array::loadHDF5SummarizedExperiment()`.
#' - `.h5ad` and `.h5mu` files are opened as file-backed SCUBA objects
#'   (`SCUBA::open_h5ad()` / `SCUBA::open_h5mu()`, served by SCUBA's Julia
#'   engine) when that engine is available, and otherwise read through
#'   reticulate (`anndata::read_h5ad()`, `mudata.read_h5mu()`). The choice can
#'   be forced with `options(cellDIVER.h5_engine = "julia" | "reticulate")`;
#'   the default `"auto"` prefers the file-backed loader.
#'
#' @param path Path to the object (a directory for HDF5-backed objects).
#' @param is_HDF5SummarizedExperiment Whether `path` is a directory written by
#'   `HDF5Array::saveHDF5SummarizedExperiment()`.
#' @param HDF5_prefix Prefix of the files in that directory.
#' @param return_error Passed to `check_dataset()`: stop (TRUE) or warn
#'   (FALSE) when the loaded object is of an unsupported class.
#'
#' @return The loaded object.
#' @noRd
load_single_cell_object <- function(path,
                                    is_HDF5SummarizedExperiment = FALSE,
                                    HDF5_prefix = "",
                                    return_error = FALSE){
  if (isTRUE(is_HDF5SummarizedExperiment)){
    object <- HDF5Array::loadHDF5SummarizedExperiment(path, prefix = HDF5_prefix)
    check_dataset(object, path = path, return_error = return_error)
    return(object)
  }

  # Lower case so .Rds and .rds are treated alike
  extension <- tolower(tools::file_ext(path))
  if (extension == "rds"){
    object <- readRDS(path)
  } else if (extension %in% c("h5ad", "h5mu")){
    loader <- h5_object_loader()
    log_info(glue("Loading .{extension} object with the {loader} loader: {path}"))
    if (loader == "scuba_source"){
      object <-
        if (extension == "h5ad") SCUBA::open_h5ad(path) else SCUBA::open_h5mu(path)
    } else if (extension == "h5ad"){
      require_python_support("h5ad")
      object <- anndata::read_h5ad(path)
    } else {
      require_python_support("h5mu")
      reticulate::py_require("mudata>=0.3.1")
      md <- reticulate::import("mudata", as = "md", convert = TRUE)
      object <- md$read_h5mu(path)
    }
  } else if (extension == ""){
    stop(
      "No file extension detected for object at path ", path,
      ". Please provide a path to an object with an extension of ",
      ".rds, .h5ad or .h5mu."
    )
  } else {
    stop(
      "Unrecognized file extension (.", extension,
      "). Currently supported extensions: .rds, .h5ad and .h5mu."
    )
  }

  check_dataset(object, path = path, return_error = return_error)
  object
}

#' Which loader serves .h5ad / .h5mu files
#'
#' @param preference `"auto"` (file-backed SCUBA object when the Julia engine
#'   is available, reticulate otherwise), `"julia"` (require the file-backed
#'   loader) or `"reticulate"`.
#'
#' @return `"scuba_source"` or `"reticulate"`.
#' @noRd
h5_object_loader <- function(preference = getOption("cellDIVER.h5_engine", "auto")){
  if (!preference %in% c("auto", "julia", "reticulate")){
    stop(
      'options(cellDIVER.h5_engine) must be "auto", "julia" or "reticulate", ',
      'not "', preference, '".'
    )
  }
  if (preference == "reticulate") return("reticulate")
  julia_available <-
    isTRUE(tryCatch(SCUBA::scuba_julia_available(quiet = TRUE), error = function(err_cnd) FALSE)) &&
    identical(tryCatch(SCUBA::scuba_engine(), error = function(err_cnd) "r"), "julia")
  if (julia_available) return("scuba_source")
  if (preference == "julia"){
    stop(
      "options(cellDIVER.h5_engine = \"julia\") but SCUBA's Julia engine is not ",
      "available. Install Julia and the JuliaCall package, or set the option to ",
      "\"reticulate\" / \"auto\"."
    )
  }
  "reticulate"
}
