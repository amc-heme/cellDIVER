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
