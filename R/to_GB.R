#' Converts an input in bytes to GB
#'
#' @param bytes Integer giving the size of an object in bytes
#'
#' @return A string. Gives the input value in GB (SI Units)
#'
#' @noRd
#' Returns total R memory usage in bytes
#'
#' Replacement for `pryr::mem_used()` using base R. Triggers a garbage
#' collection and sums cell memory usage: Ncells (56 bytes each) and
#' Vcells (8 bytes each), matching pryr's internal calculation.
#'
#' @return Integer. Total memory used by R in bytes.
#'
#' @noRd
mem_used <- function() {
  # gc() returns a matrix with rows for Ncells and Vcells;
  # column 2 is "used" count. Cell sizes match pryr's implementation.
  gc_stats <- gc(verbose = FALSE)
  sum(gc_stats[, 2] * c(56, 8))
}

to_GB <- function(bytes){
  # Divide by 10^9 and round to two digits
  gb <- 
    round(
      bytes/(10^9),
      digits = 3
      )
  
  # Return value with 'GB' at end
  glue("{gb} GB")
}
