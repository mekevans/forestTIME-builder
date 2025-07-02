#' Adjust interpolated tables for mortality
#'
#' This is an "internal" function—most users will want to run [fia_annualize()]
#' instead. Trees in the input `data_interpolated` already have had their switch
#' to `STATUSCD` 2 (i.e. death) interpolated to the midpoint (rounded down)
#' between it's last survey alive and first survey dead.
#'
#' This does the following:
#' - Optionally figures out if a tree has a recorded `MORTYR` and uses that for
#'   the transition to `STATUSCD` 2 instead of the interpolated values. If the
#'   tree is alive (`STATUSCD` 1) in `MORTYR`, then it is assumed it died in the
#'   following year.
#' - Adjusts `STANDING_DEAD_CD` so that it only applies to dead trees
#' - Adjusts `DECAYCD` so that it only applies to standing dead trees
#' - Adjusts `DIA`, `HT`, `ACTUALHT`, `CULL`, and `CR` so that they only apply
#'   to live or standing dead trees in sampled conitions.
#'
#' @param data_interpolated tibble created by [interpolate_data()]
#' @param use_mortyr logical; use `MORTYR` (if recorded) as the first year a
#'   tree was dead?
#' @export
#' @keywords internal
#' @returns a tibble
adjust_mortality <- function(data_interpolated, use_mortyr = TRUE) {
  cli::cli_progress_step("Adjusting for mortality")

  # STATUSCD is already interpolated to the midpoint between surveys, but if
  # MORTYR is used it is more complicated

  if (isTRUE(use_mortyr)) {
    # If there aren't any recorded MORTYR, warn and skip the complicated stuff
    any_mortyr <- any(!is.na(data_interpolated$MORTYR))
    if (isFALSE(any_mortyr)) {
      cli::cli_warn(
        c(
          "!" = "No recorded {.var MORTYR} in data. ",
          "i" = "Setting {.arg use_mortyr} to `FALSE`"
        )
      )
      use_mortyr <- FALSE
    }
  }

  if (isTRUE(use_mortyr)) {
    # If a tree is marked as alive (STATUSCD 1) in the recorded MORTYR (i.e.
    # MORTYR is an inventory year), then assume death happened the year after
    # that inventory year
    # (https://github.com/mekevans/forestTIME-builder/issues/61)

    df <- data_interpolated |>
      dplyr::group_by(tree_ID) |>
      dplyr::mutate(
        MORTYR_eff = if_else(
          YEAR == MORTYR & STATUSCD == 1,
          MORTYR + 1,
          MORTYR
        ),
        .after = MORTYR
      ) |>
      # this "fills in" the new effective MORTYR for all rows since the above
      # if_else() only increments it for the inventory year row
      mutate(MORTYR_eff = max(MORTYR_eff)) |>
      dplyr::mutate(
        # STATUSCD is interpolated to the midpoint between surveys
        # see utils.R for more info on what %|||% does
        first_dead = YEAR[min(which(STATUSCD == 2) %|||% NA)]
      ) |>
      # When a tree has a recorded MORTYR, adjust STATUSCD depending on whether
      # MORTYR is before or after the midpoint (first_dead). This works because
      # MORTYR is filled in for every row of a tree by fia_tidy() and
      # expand_data(). Can't assume tree has STATUSCD 2 after MORTYR since
      # sometimes STATUSCD goes from 1 to 2 to 0.
      dplyr::mutate(
        STATUSCD = dplyr::case_when(
          is.na(MORTYR_eff) ~ STATUSCD, #do nothing
          MORTYR_eff == first_dead ~ STATUSCD, #do nothing

          # if MORTYR is earlier than midpoint and the year is between the
          # MORTYR and the midpoint, adjust STATUSCD to 2
          MORTYR_eff < first_dead & YEAR >= MORTYR_eff & YEAR < first_dead ~ 2,

          # if MORTYR is after the midpoint and the year is between the midpoint
          # and MORTYR, adjust STATUSCD to 1
          MORTYR_eff > first_dead & YEAR < MORTYR_eff & YEAR >= first_dead ~ 1,
          .default = STATUSCD
        )
      ) |>
      # MORTYR might be earlier than the midpoint, so backfill NAs for DECAYCD
      # and STANDING_DEAD_CD. These will be corrected later anyways
      tidyr::fill(DECAYCD, STANDING_DEAD_CD, .direction = "up") |>
      dplyr::select(-first_dead, -MORTYR_eff)
  } else {
    df <- data_interpolated |> dplyr::group_by(tree_ID)
  }

  df |>
    # adjust STATUSCD & DECAYCD

    # STANDING_DEAD_CD only applies to dead trees
    dplyr::mutate(
      STANDING_DEAD_CD = dplyr::if_else(STATUSCD == 2, STANDING_DEAD_CD, NA)
    ) |>
    # and DECAYCD only applies to standing dead trees
    dplyr::mutate(
      DECAYCD = dplyr::if_else(STANDING_DEAD_CD == 1, DECAYCD, NA)
    ) |>
    # fallen trees shouldn't have measurements for anything
    dplyr::mutate(
      dplyr::across(
        c(DIA, HT, ACTUALHT, CULL, CR),
        \(x) dplyr::if_else(STANDING_DEAD_CD == 0, NA, x, missing = x)
      )
    ) |>
    # trees in non-sampled areas shoudn't have measurements for anything
    # https://github.com/mekevans/forestTIME-builder/issues/59
    dplyr::mutate(
      dplyr::across(
        c(DIA, HT, ACTUALHT, CULL, CR),
        \(x)
          dplyr::if_else(
            (STATUSCD == 0 & RECONCILECD %in% c(5, 6, 9)) |
              (COND_STATUS_CD != 1),
            NA,
            x,
            missing = x
          )
      )
    ) |>
    dplyr::ungroup()
}
