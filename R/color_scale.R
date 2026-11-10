#' Mirror color scale limits about zero
#'
#' The color limits slider has a symmetric option for diverging palettes. The
#' two handles are kept at the same distance from zero, following the handle
#' the user moved: when exactly one handle differs from the previous
#' (symmetric) value, that handle's magnitude is mirrored, so the range can
#' be narrowed as well as widened. When both handles changed (a reset, or the
#' first value after enabling the option) the larger magnitude is used.
#'
#' @param value numeric vector of length two from the slider.
#' @param previous the previous symmetric value, or NULL.
#'
#' @return a numeric vector of length two, `c(-m, m)`, or `value` unchanged
#'   when it is not a finite pair.
#'
#' @noRd
symmetric_color_limits <- function(value, previous = NULL){
  if (!is.numeric(value) || length(value) != 2 || any(!is.finite(value))){
    return(value)
  }

  changed <-
    if (is.numeric(previous) && length(previous) == 2){
      which(abs(value - previous) > sqrt(.Machine$double.eps))
    } else {
      integer()
    }

  magnitude <-
    if (length(changed) == 1){
      abs(value[changed])
    } else {
      max(abs(value))
    }

  c(-1, 1) * magnitude
}

#' Replace the color scale of every panel, keeping each panel's legend title
#'
#' Adding one scale to a patchwork with `&` gives every panel the same legend
#' title (or none). When panels carry their own titles, such as feature names
#' on separate scales, replace the scale panel by panel and keep the name the
#' panel's current scale has.
#'
#' @param plot a ggplot or patchwork.
#' @param aesthetic `"colour"` or `"fill"`.
#' @param ... passed to [color_scale()] (`palette`, `limits`, `diverging`).
#'
#' @return the plot with its scales replaced.
#'
#' @noRd
replace_panel_color_scales <- function(plot, aesthetic = c("colour", "fill"), ...){
  aesthetic <- match.arg(aesthetic)

  current_name <- function(panel){
    scale <- panel$scales$get_scales(aesthetic)
    if (is.null(scale) || is.null(scale$name)) waiver() else scale$name
  }

  replace <- function(panel){
    suppressMessages(
      panel + color_scale(aesthetic, ..., name = current_name(panel))
    )
  }

  if (inherits(plot, "patchwork")){
    for (i in seq_len(length(plot))){
      if (inherits(plot[[i]], "ggplot")){
        plot[[i]] <- replace(plot[[i]])
      }
    }
    plot
  } else {
    replace(plot)
  }
}

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
