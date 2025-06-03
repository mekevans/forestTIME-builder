#' Expand data to include years between inventory years
#'
#' This expands the data frame in preparation for interpolation of now "missing"
#' values between inventory years. Time-invariant variables `tree_ID`,
#' `plot_ID`, `SPCD`, `ECOSUBCD`, `DESIGNCD`, and `PROP_BASIS` are simply filled
#' in with [tidyr::fill()]. Categorical variables `STATUDSCD`, `RECONCILECD`,
#' `STDORGCD`, `CONDID`, and `COND_STATUS_CD` are modified to replace `NA`s with
#' `999` so that they are properly interpolated by [interpolate_data()] (which
#' converts them back to `NA`s).
#'
#' @param data tibble produced by [read_fia()]---must have at least `tree_ID`
#'   and `INVYR` columns.
#' @export
#' @returns a tibble with a logical column `interpolated` marking whether a row
#'   was present in the original data (`FALSE`) or was added (`TRUE`).
expand_data <- function(data) {
  cli::cli_progress_step("Expanding years between surveys")

  data <- data |>
    # replace NAs for some categorical variables with 999 (temporarily) so they
    # switch from NA correctly
    # (https://github.com/mekevans/forestTIME-builder/issues/72)
    dplyr::mutate(dplyr::across(
      any_of(c(
        "STATUSCD",
        "RECONCILECD",
        "DECAYCD",
        "STANDING_DEAD_CD",
        "STDORGCD",
        "CONDID",
        "COND_STATUS_CD"
      )),
      \(x) dplyr::if_else(is.na(x), 999, x)
    ))

  all_yrs <-
    data |>
    dplyr::group_by(plot_ID, tree_ID) |>
    tidyr::expand(YEAR = tidyr::full_seq(INVYR, 1))

  # Join while creating a flag indicating whether the row is from the original
  # data or a result of "expanding"
  tree_annual <-
    dplyr::right_join(
      data |> dplyr::mutate(interpolated = FALSE),
      all_yrs |> dplyr::mutate(interpolated = TRUE),
      by = dplyr::join_by(plot_ID, tree_ID, INVYR == YEAR)
    ) |>
    dplyr::mutate(
      interpolated = dplyr::coalesce(interpolated.x, interpolated.y),
      .keep = "unused" #to remove interpolated.x and interpolated.y
    ) |>
    dplyr::rename(YEAR = INVYR) |>
    dplyr::arrange(tree_ID, YEAR) |>
    #fill down any time-invariant columns
    dplyr::group_by(tree_ID) |>
    tidyr::fill(any_of(c(
      "plot_ID",
      "SPCD",
      "ECOSUBCD",
      "DESIGNCD",
      "PROP_BASIS",
      "MORTYR"
    )), .direction = "downup") |>
    dplyr::ungroup() |>
    #rearrange
    dplyr::select(
      any_of(c(
        "plot_ID",
        "tree_ID",
        "YEAR",
        "interpolated",
        "DIA",
        "HT",
        "ACTUALHT",
        "CR",
        "CULL"
      )),
      everything()
    )
  tree_annual
}
