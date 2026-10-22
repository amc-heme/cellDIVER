#' Range of the values plotted in a ggplot or patchwork
#'
#' Walks a ggplot, or a patchwork of ggplots (recursively), and returns the
#' range of the given data columns across every panel. Used to report the
#' values that are mapped to a plot's continuous color/fill scale, so the
#' color scale limits slider can be bounded by what is actually shown.
#'
#' @param plot a ggplot or patchwork object.
#' @param columns character vector of column names to look for in each panel's
#'   data. Columns that are absent from a panel are ignored.
#'
#' @return `plotted_values()` returns the finite values collected across panels
#'   (possibly empty); `plotted_range()` returns a numeric vector of length two
#'   (minimum, maximum), or `NULL` if none of the columns are present in any
#'   panel or contain finite values.
#'
#' @noRd
plotted_range <- function(plot, columns){
  values <- plotted_values(plot, columns)

  if (length(values) == 0){
    return(NULL)
  }

  range(values)
}

#' @rdname plotted_range
#' @noRd
plotted_values <- function(plot, columns){
  values <- numeric()

  collect <- function(p){
    if (inherits(p, "patchwork")){
      # A patchwork is itself a ggplot carrying the last panel's data, so only
      # look at its elements to avoid reading that panel twice
      for (i in seq_len(length(p))){
        collect(p[[i]])
      }
    } else if (inherits(p, "ggplot")){
      data <- p$data

      if (is.data.frame(data)){
        for (column in intersect(columns, names(data))){
          column_values <- data[[column]]

          if (is.numeric(column_values)){
            values <<- c(values, column_values[is.finite(column_values)])
          }
        }
      }
    }
  }

  collect(plot)

  values
}
