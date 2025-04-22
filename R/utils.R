#' Null or length 0 collascing operator.
#'
#' A modification of [rlang::`%||%`] that also replaces length 0 vectors on
#' the left hand side with the right hand side.
#' @param lhs an object on the left hand side of the operator
#' @param rhs, when `lhs` is NULL or length 0, what to replace it with
#' @examples
#' 10 %|||% 5
#' #> 10
#' NULL %|||% 5
#' #> 5
#' character() %|||% "hi"
#' #> "hi"
#' @noRd
`%|||%` <- function(lhs, rhs) {
  if (is.null(lhs) | length(lhs) == 0) {
    rhs
  } else {
    lhs
  }
}
