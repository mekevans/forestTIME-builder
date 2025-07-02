
#' Add composite ID columns to data
#'
#' Creates a `tree_ID` and/or a `plot_ID` column that contain unique tree and
#' plot identifiers, respectively.  These are created by pasting together the
#' values for `STATECD`, `UNITCD`, `COUNTYCD`, `PLOT` and in the case of trees
#' `SUBP` and `TREE`.
#'
#' @param data A tibble or data frame with at least the `STATECD`, `UNITCD`,
#'   `COUNTYCD` and `PLOT` columns
#'
#' @seealso See [fia_split_composite_ids()] for "undoing" this.
#' @returns The input tibble with a `plot_ID` and possibly also a `tree_ID`
#'   column added
#' @export
fia_add_composite_ids <- function(data) {
  cols <- colnames(data)
  if (
    all(c("STATECD", "UNITCD", "COUNTYCD", "PLOT", "SUBP", "TREE") %in% cols)
  ) {
    data <-
      data |>
      dplyr::mutate(
        plot_ID = paste(STATECD, UNITCD, COUNTYCD, PLOT, sep = "_"),
        tree_ID = paste(STATECD, UNITCD, COUNTYCD, PLOT, SUBP, TREE, sep = "_"),
        .before = 1
      )
  } else if (all(c("STATECD", "UNITCD", "COUNTYCD", "PLOT") %in% cols)) {
    data <-
      data |>
      dplyr::mutate(
        plot_ID = paste(STATECD, UNITCD, COUNTYCD, PLOT, sep = "_"),
        .before = 1
      )
  } else {
    stop("Not all required columns are present")
  }
  data
}

#' Split composite ID columns
#'
#' Splits the composite ID columns `tree_ID` and/or `plot_ID` into their
#' original component columns
#'
#' @param data A tibble with the `tree_ID` and/or `plot_ID` columns
#' @returns The input tibble with additional columns `STATECD`, `UNITCD`,
#'   `COUNTYCD`, `PLOT` and possibly `SUBP` and `TREE`.
#' @seealso [fia_add_composite_ids()]
#' @export
fia_split_composite_ids <- function(data) {
  cols <- colnames(data)
  if (!any(c("plot_ID", "tree_ID") %in% cols)) {
    stop("No composite ID columns found")
  }

  # tree_ID contains all the information in plot_ID, so if tree_ID exists, it's
  # enough to just split that one
  if ("tree_ID" %in% cols) {
    data <- data |>
      tidyr::separate_wider_delim(
        tree_ID,
        delim = "_",
        names = c("STATECD", "UNITCD", "COUNTYCD", "PLOT", "SUBP", "TREE"),
        cols_remove = FALSE
      )
    return(data)
  }

  if ("plot_ID" %in% cols) {
    data <- data |>
      tidyr::separate_wider_delim(
        plot_ID,
        delim = "_",
        names = c("STATECD", "UNITCD", "COUNTYCD", "PLOT"),
        cols_remove = FALSE
      )
    return(data)
  }
}
