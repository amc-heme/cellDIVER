#' Should .h5ad / .h5mu files be opened through SCUBA's Julia engine?
#'
#' cellDIVER can load AnnData files either through reticulate (`anndata::read_h5ad()`, the
#' legacy path, which needs a Python environment) or, when the SCUBA package provides its Julia
#' engine, through `SCUBA::open_h5ad()`, which reads the file natively and serves expression
#' fetches at native speed. `.h5mu` files follow the same switch (`mudata::read_h5mu()` through
#' reticulate, or `SCUBA::open_h5mu()`, where modalities are addressed by their MuData names,
#' e.g. `rna_` / `prot_`). The choice is controlled by `options(cellDIVER.h5ad_engine = )`:
#' `"auto"` (default; Julia when available), `"julia"` (error when unavailable) or
#' `"reticulate"`. Differential expression on file-backed objects needs scDE >= 0.3.1
#' (`run_dge.scuba_source`, Presto on `SCUBA::fetch_matrix()`); the DGE tab stays hidden for
#' `.h5mu` datasets, as it is for MuData objects loaded through reticulate.
#'
#' @returns `TRUE` when files should be opened with `SCUBA::open_h5ad()`.
#' @noRd
use_julia_h5ad <- function() {
  mode <- getOption("cellDIVER.h5ad_engine", "auto")
  if (!mode %in% c("auto", "julia", "reticulate")) {
    stop("options(cellDIVER.h5ad_engine) must be \"auto\", \"julia\" or \"reticulate\" (got \"", mode, "\")", call. = FALSE)
  }
  if (mode == "reticulate") return(FALSE)
  ns <- asNamespace("SCUBA")
  available <- exists("open_h5ad", envir = ns) && exists("scuba_engine", envir = ns) &&
    isTRUE(tryCatch(identical(get("scuba_engine", envir = ns)(), "julia"), error = function(e) FALSE))
  if (mode == "julia" && !available) {
    stop("options(cellDIVER.h5ad_engine = \"julia\") but SCUBA's Julia engine is not available ",
         "(SCUBA >= 2.0 with Julia and JuliaCall is required)", call. = FALSE)
  }
  available
}

#' Warm the SCUBA Julia engine on the loaded datasets
#'
#' Wraps every loaded object once so the first user request of each session does not pay for
#' it; a no-op when the Julia engine is not in use or the installed SCUBA predates it.
#' @noRd
warm_scuba_engine <- function(objects) {
  ns <- asNamespace("SCUBA")
  if (!exists("scuba_julia_warmup", envir = ns) || !exists("scuba_engine", envir = ns)) return(invisible(NULL))
  if (!isTRUE(tryCatch(identical(get("scuba_engine", envir = ns)(), "julia"), error = function(e) FALSE))) return(invisible(NULL))
  invisible(get("scuba_julia_warmup", envir = ns)(objects = objects))
}
