#' Two Column Container
#'
#' Positions content side-by-side in a two column format.
#'
#' @param left_column,right_column UI to display in the left/right column. Only one element can be passed to each column, but multiple elements can be added to either column by packaging the content in a `div` or `tagList` function.
#' @param left_width,right_width The width of the left/right columns, in percentage points. The default is 50, which will cause each column to take up 50% of the container `two_column_layout()` is called within.
#' @param left_class,right_class CSS classes to apply to the left/right columns.
#'
#' @returns a shiny.tag object with the content entered in a two-column format.
#' 
#' @noRd
two_column_layout <-
  function(
    left_column,
    right_column,
    left_width = 50,
    right_width = 50,
    left_class = "",
    right_class = ""
    ){
    div(
      class = "two-column-container",
      div(
        # Affix any classes defined for the left column to the class parameter
        class = 
          paste0("two-column two-column-left", left_class),
        # Apply width of left column
        style = 
          paste0("width: ", left_width, "%;"),
        # Column content
        left_column
      ),
      div(
        # Affix any classes defined for the right column to the class parameter
        class = 
          paste0("two-column two-column-right", right_class),
        # Apply width of right column
        style = 
          paste0("width: ", right_width, "%;"),
        # Column content
        right_column
      )
    )
  }