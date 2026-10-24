#' Add reversed variants of continuous palettes
#'
#' Takes the named list of continuous palettes offered in the plots tab and
#' returns a list in which every palette is followed by a reversed copy named
#' `"<name> (reversed)"`. Palettes named in `exclude` (by default the
#' lightgrey-to-blue `default` palette) are kept but not reversed.
#'
#' Entries are interleaved (`inferno`, `inferno (reversed)`, `plasma`, ...) so
#' each pair sits together in the palette picker.
#'
#' @param palettes a named list of character vectors (color codes).
#' @param exclude character vector of palette names that should not receive a
#'   reversed variant.
#'
#' @return a named list of character vectors.
#'
#' @noRd
reverse_continuous_palettes <- function(palettes, exclude = "default") {
  out <- list()

  for (name in names(palettes)) {
    out[[name]] <- palettes[[name]]

    if (!name %in% exclude) {
      out[[paste0(name, " (reversed)")]] <- rev(palettes[[name]])
    }
  }

  out
}
