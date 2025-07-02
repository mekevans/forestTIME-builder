#' Read in needed tables
#'
#' Wrapper for [rFIA::readFIA] that reads in the necessary tables
#' @inheritParams rFIA::readFIA
#'
#' @export
#' @returns a list of data frames
fia_load <- function(states, dir = "fia") {
  rFIA::readFIA(dir = dir, states = states, tables = tables) |>
    purrr::map(dplyr::as_tibble)
}