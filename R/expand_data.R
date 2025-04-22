#' Expand data to include years between inventory years
#'
#' This expands the data frame in preparation for interpolation of now "missing"
#' values between inventory years. Time-invariant variables `tree_ID`,
#' `plot_ID`, `SPCD`, `ECOSUBCD`, `DESIGNCD`, and `PROP_BASIS` are simply filled
#' in with [tidyr::fill()]. Categorical variables `STATUDSCD`, `RECONCILECD`,
#' `STDORGCD`, `CONDID`, and `COND_STATUS_CD` are modified to replace `NA`s with
#'  `999` so that they are properly interpolated by [interpolate_data()] (which
#' converts them back to `NA`s).
#'
#' @param data tibble produced by [read_fia()].
#' @export
#' @returns a tibble
expand_data <- function(data) {
  cli_progress_step("Expanding years between surveys")
  #We do the expand() in chunks because it is computationally expensive otherwise
  #TODO actually bench::mark() this.  I don't know why this doesn't work not chunked

  data <- data |>
    # replace NAs for some categorical variables with 999 (temporarily) so
    # they switch from NA correctly (https://github.com/mekevans/forestTIME-builder/issues/72)
    dplyr::mutate(dplyr::across(
      all_of(c(
        "STATUSCD",
        "RECONCILECD",
        # Except these two which get handled differently, I think. (see adjust_mortality())
        # "DECAYCD",
        # "STANDING_DEAD_CD",
        "STDORGCD",
        "CONDID",
        "COND_STATUS_CD"
      )),
      \(x) if_else(is.na(x), 999, x)
    ))

  plot_chunks <-
    data |>
    dplyr::ungroup() |>
    dplyr::select(plot_ID) |>
    dplyr::distinct() |>
    dplyr::mutate(plot_chunk = ntile(plot_ID, n = 10))

  all_yrs <-
    dplyr::left_join(data, plot_chunks, by = join_by(plot_ID)) |>
    dplyr::group_by(plot_chunk) |>
    dplyr::group_split() |>
    purrr::map(
      \(x) {
        x |>
          dplyr::group_by(tree_ID) |>
          tidyr::expand(YEAR = tidyr::full_seq(INVYR, 1))
      },
      .progress = TRUE
    ) |>
    purrr::list_rbind()

  tree_annual <-
    dplyr::right_join(
      data,
      all_yrs,
      by = dplyr::join_by(tree_ID, INVYR == YEAR)
    ) |>
    dplyr::rename(YEAR = INVYR) |>
    dplyr::arrange(tree_ID, YEAR) |>
    #fill down any time-invariant columns
    dplyr::group_by(tree_ID) |>
    tidyr::fill(plot_ID, SPCD, ECOSUBCD, DESIGNCD, PROP_BASIS) |>
    dplyr::ungroup() |>
    #rearrange
    dplyr::select(
      tree_ID,
      plot_ID,
      YEAR,
      DIA,
      HT,
      ACTUALHT,
      CR,
      CULL,
      everything()
    )
  tree_annual
}
