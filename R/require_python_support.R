#' Require the optional Python stack before reading a Python-backed object
#'
#' `reticulate` and `anndata` are Suggests, not Imports: users whose objects
#' are Seurat or SingleCellExperiment should not need a Python toolchain, and
#' the Docker image ships without one. Check for them at the point of use and
#' explain what to install, rather than letting the read fail with a bare
#' "there is no package called 'anndata'" from inside a `tryCatch` whose
#' handler reports something unrelated.
#'
#' @param format Object format being loaded, used in the error message.
#'
#' @return No return value; called for its side effect of stopping early.
#'
#' @noRd
require_python_support <- function(format) {
  missing <- Filter(
    function(package) !requireNamespace(package, quietly = TRUE),
    c("reticulate", "anndata")
  )

  if (length(missing) > 0) {
    stop(
      "Reading .", format, " objects requires the ",
      paste(sQuote(missing), collapse = " and "),
      if (length(missing) > 1) " packages" else " package",
      ", which cellDIVER lists under Suggests.\n",
      "Install with install.packages(c(\"reticulate\", \"anndata\")) and ",
      "make sure a Python environment providing anndata is available.",
      call. = FALSE
    )
  }

  invisible(NULL)
}
