#' Continuous color scale for cellDIVER plots
#'
#' Builds the ggplot2 colour or fill scale used by Feature, Dot and Hexbin
#' plots: either a gradient through the supplied palette, or a three-color
#' diverging gradient with its midpoint fixed at zero. When `limits` is given,
#' the scale runs from `limits[1]` to `limits[2]` so the end colors sit at
#' user-chosen values instead of the data minimum and maximum, and values
#' outside the limits are clamped to the end colors (the data are not
#' changed).
#'
#' @param aesthetic the aesthetic the scale applies to: `"colour"` (Feature and
#'   Dot plots) or `"fill"` (Hexbin plots).
#' @param palette a character vector of colors. For `diverging = TRUE`,
#'   exactly three colors (low, mid, high).
#' @param limits optional numeric vector of length two giving the values at
#'   which the end colors are placed. `NULL` (the default), a non-finite
#'   vector, or one whose first value is not below its second uses the range
#'   of the data.
#' @param diverging if TRUE, build a zero-centered diverging scale
#'   (`scale_*_gradient2()` with `midpoint = 0`) from a three-color palette.
#' @param name the legend title. The default keeps whatever title the plot
#'   already has.
#'
#' @return a ggplot2 scale object.
#'
#' @noRd
color_scale <- function(aesthetic = c("colour", "fill"),
                        palette,
                        limits = NULL,
                        diverging = FALSE,
                        name = waiver()){
  aesthetic <- match.arg(aesthetic)

  if (
    !is.null(limits) &&
    !(
      is.numeric(limits) &&
      length(limits) == 2 &&
      all(is.finite(limits)) &&
      limits[1] < limits[2]
    )
  ){
    limits <- NULL
  }

  if (diverging == TRUE){
    args <-
      list(
        name = name,
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
  } else {
    args <-
      list(
        name = name,
        colors = palette,
        limits = limits,
        oob = scales::squish
      )

    if (aesthetic == "fill"){
      do.call(scale_fill_gradientn, args)
    } else {
      do.call(scale_color_gradientn, args)
    }
  }
}
