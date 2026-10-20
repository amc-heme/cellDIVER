#' Zero-centered diverging color scale
#'
#' Builds the ggplot2 scale used for the "custom diverging colors" option of
#' Feature, Dot and Hexbin Feature plots: a three-color gradient with its
#' midpoint fixed at zero. When `limit` is given, the scale runs from `-limit`
#' to `+limit` so the end colors sit at a chosen absolute value instead of the
#' data minimum and maximum, and values beyond the limit are clamped to the
#' end colors.
#'
#' @param aesthetic the aesthetic the scale applies to: `"colour"` (Feature and
#'   Dot plots) or `"fill"` (Hexbin Feature plots).
#' @param palette a character vector of three colors (low, mid, high).
#' @param limit optional positive number giving the absolute value at which the
#'   end colors are placed. `NULL` (the default) uses the range of the data.
#'
#' @return a ggplot2 scale object.
#'
#' @noRd
diverging_scale <- function(aesthetic = c("colour", "fill"),
                            palette,
                            limit = NULL){
  aesthetic <- match.arg(aesthetic)

  limits <-
    if (!is.null(limit) && is.finite(limit) && limit > 0){
      c(-limit, limit)
    } else NULL

  args <-
    list(
      low = palette[1],
      mid = palette[2],
      high = palette[3],
      midpoint = 0,
      limits = limits,
      # Clamp out-of-range values to the end colors rather than dropping them
      oob = scales::squish
    )

  if (aesthetic == "fill"){
    do.call(scale_fill_gradient2, args)
  } else {
    do.call(scale_color_gradient2, args)
  }
}
