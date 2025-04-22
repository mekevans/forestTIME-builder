#' Adjust interpolated tables for mortality
#'
#' Trees in the input `data_interpolated` already have had their switch to
#' `STATUSCD` 2 (i.e. death) interpolated to the midpoint (rounded down) between
#' it's last survey alive and first survey dead.
#'
#' This does the following:
#' - Optionally figures out if a tree has a recorded `MORTYR` and uses that for
#'   the transition to `STATUSCD` 2 instead of the interpolated values. (assumes
#'   `MORTYR` was *before* the inventory where it was recoreed dead)
#' - Drops trees that transition to `STATUSCD` 0 and `RECONCILECD` 5, 6, or 9
#'   (moved out of plot) at the midpoint between surveys
#' - Adjusts `STANDING_DEAD_CD` so that it only applies to dead trees
#' - Adjusts `DECAYCD` so that it only applies to standing dead trees
#' - Adjusts `DIA`, `HT`, `ACTUALHT`, `CULL`, and `CR` so that they only apply
#'   to live or standing dead trees.
#' - Removes the `MORTYR` column, as it is no longer needed
#'
#' @param data_interpolated tibble created by [interpolate_data()]
#' @param use_mortyr logical; use `MORTYR` (if recorded) as the first year a
#'   tree was dead?
#' @export
#' @returns a tibble
adjust_mortality <- function(data_interpolated, use_mortyr = TRUE) {
  cli::cli_progress_step("Adjusting for mortality")

  if (isTRUE(use_mortyr)) {
    df <- data_interpolated |>
      dplyr::group_by(tree_ID) |>
      dplyr::mutate(
        #to use MORTYR
        first_dead = ifelse(
          test = any(!is.na(MORTYR)), #has recorded MORTYR
          yes = max(MORTYR, na.rm = TRUE),
          # see utils.R for more info on what %|||% does
          no = YEAR[min(which(STATUSCD == 2) %|||% NA)]
        )
      )
  } else {
    df <- data_interpolated |>
      dplyr::group_by(tree_ID) |>
      dplyr::mutate(
        # see utils.R for more info on what %|||% does
        first_dead = YEAR[min(which(STATUSCD == 2) %|||% NA)]
      )
  }

  df |>
    #drop trees that transition to STATUSCD 0 and RECONCILECD 5, 6, or 9
    #https://github.com/mekevans/forestTIME-builder/issues/59
    dplyr::filter(!(STATUSCD == 0 & RECONCILECD %in% c(5, 6, 9))) |>
    #then adjust STATUSCD & DECAYCD
    dplyr::group_by(tree_ID) |>
    dplyr::mutate(
      STATUSCD = dplyr::if_else(
        YEAR >= first_dead,
        2,
        STATUSCD,
        missing = STATUSCD
      )
    ) |>
    #MORTYR might be earlier than the midpoint, so backfill NAs for DECAYCD and STANDING_DEAD_CD
    tidyr::fill(DECAYCD, STANDING_DEAD_CD, .direction = "up") |>
    #But, STANDING_DEAD_CD only applies to dead trees
    dplyr::mutate(
      STANDING_DEAD_CD = dplyr::if_else(STATUSCD == 2, STANDING_DEAD_CD, NA)
    ) |>
    #and DECAYCD only applies to standing dead trees > 4.9 DIA
    dplyr::mutate(
      DECAYCD = dplyr::if_else(STANDING_DEAD_CD == 1 & DIA > 4.9, DECAYCD, NA)
    ) |>
    #fallen trees shouldn't have measurements for anything
    dplyr::mutate(
      dplyr::across(
        c(DIA, HT, ACTUALHT, CULL, CR),
        \(x) dplyr::if_else(STANDING_DEAD_CD == 0, NA, x, missing = x)
      )
    ) |>
    dplyr::select(-MORTYR, -first_dead) #don't need this anymore?
}
