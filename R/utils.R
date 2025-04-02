#' Null or length 0 collascing operator.
#'
#' A modification of [rlang::`%||%`] that also replaces length 0 vectors on
#' the left hand side with the right hand side.
#' @param lhs an object on the left hand side of the operator
#' @param rhs, when `lhs` is NULL or length 0, what to replace it with
`%|||%` <- function(lhs, rhs) {
  if (is.null(lhs) | length(lhs) == 0) {
    rhs
  } else {
    lhs
  }
}
