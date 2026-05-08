#' Returns total R memory usage in bytes
#'
#' Replacement for `pryr::mem_used()` using base R. Triggers a garbage
#' collection and sums cell memory usage based on architecture-dependent
#' node size, matching pryr's internal calculation exactly.
#'
#' @return Numeric. Total memory used by R in bytes.
#'
#' @noRd
mem_used <- function() {
  # gc() returns a matrix with rows for Ncells and Vcells;
  # column 1 is the raw cell count (not Mb). Node size matches pryr's
  # node_size(): 56 bytes on 64-bit, 28 bytes on 32-bit systems.
  node_size <- if (8L * .Machine$sizeof.pointer == 32L) 28L else 56L
  sum(gc(verbose = FALSE)[, 1] * c(node_size, 8))
}

#' Converts an input in bytes to GB
#'
#' @param bytes Integer giving the size of an object in bytes
#'
#' @return A string. Gives the input value in GB (SI Units)
#'
#' @noRd
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
