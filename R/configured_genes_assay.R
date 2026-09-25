#' Resolve the configured genes assay
#'
#' Reads the canonical `genes_assay` key while retaining compatibility with the
#' singular `gene_assay` key written by some older configurations.
#'
#' @param config A parsed scExploreR configuration list.
#'
#' @return One assay name, or `NULL` when no genes assay is configured.
#'
#' @noRd
configured_genes_assay <- function(config){
  options <- config$other_assay_options
  if (is.null(options)){
    return(NULL)
  }

  assay <- options$genes_assay
  if (is.null(assay)){
    assay <- options$gene_assay
  }

  if (is.null(assay) || length(assay) == 0L || is.na(assay[[1]]) ||
      !nzchar(assay[[1]]) || identical(assay[[1]], "none")){
    return(NULL)
  }

  assay[[1]]
}
