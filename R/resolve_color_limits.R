#' Resolve color scale limits given as values or quantiles
#'
#' The color scale limits slider can be set in data units or as quantiles
#' (percentiles) of the values mapped to the scale. This converts the latter
#' into values so the result can be passed to [color_scale()].
#'
#' @param limits a numeric vector of length two, or NULL. When `quantile` is
#'   TRUE these are probabilities between 0 and 1.
#' @param quantile if TRUE, interpret `limits` as probabilities and evaluate
#'   them against `values`.
#' @param values the numeric values the quantiles are taken over (ignored when
#'   `quantile` is FALSE).
#'
#' @return a numeric vector of length two, or NULL when `limits` is NULL or
#'   cannot be resolved (for example, no finite values to take quantiles of).
#'
#' @noRd
resolve_color_limits <- function(limits, quantile = FALSE, values = NULL){
  if (is.null(limits)){
    return(NULL)
  }

  if (!isTRUE(quantile)){
    return(limits)
  }

  values <- values[is.finite(values)]

  if (length(values) == 0 || !is.numeric(limits) || length(limits) != 2){
    return(NULL)
  }

  probs <- pmin(pmax(limits, 0), 1)

  unname(stats::quantile(values, probs = probs, names = FALSE))
}

#' Resolve per-cell cutoffs given as values or quantile strings
#'
#' Per-cell cutoffs reach the plot functions as numbers or as `"q<percent>"`
#' strings. The plot functions resolve such strings against each panel's own
#' values, which gives every split group or feature a different cutoff. This
#' resolves them once against the pooled per-cell values instead, so the same
#' numeric cutoff is applied to every panel.
#'
#' @param cutoffs a vector of length two (minimum, maximum): numbers, `NA` for
#'   no cutoff, or `"q<percent>"` strings; or NULL.
#' @param values the pooled per-cell values the quantiles are taken over.
#'
#' @return a numeric vector of length two with `NA` where there is no cutoff,
#'   or NULL when `cutoffs` is NULL.
#'
#' @noRd
resolve_data_cutoffs <- function(cutoffs, values = NULL){
  if (is.null(cutoffs)){
    return(NULL)
  }

  values <- values[is.finite(values)]

  resolved <- vapply(
    cutoffs[1:2],
    function(cutoff){
      if (is.na(cutoff)){
        return(NA_real_)
      }
      if (is.numeric(cutoff)){
        return(as.numeric(cutoff))
      }
      cutoff <- as.character(cutoff)
      if (grepl("^q[0-9.]+$", cutoff)){
        if (!length(values)){
          return(NA_real_)
        }
        prob <- as.numeric(sub("^q", "", cutoff)) / 100
        return(unname(stats::quantile(values, probs = min(max(prob, 0), 1), names = FALSE)))
      }
      suppressWarnings(as.numeric(cutoff))
    },
    numeric(1)
  )

  unname(resolved)
}
