#' Interpolate expanded tree data
#'
#' Fills in `NA`s between survey years with either linear interpolation /
#' extrapolation or by switching categorical variables at the midpoint (rounded
#' down) between surveys. Linear interpolation/extrapolation is accomplished
#' with [inter_extra_polate()] and the categorical variables are handled with
#' [step_interp()]. Also converts temporary `999` values created by
#' [expand_data()] back to `NA`s
#'
#' @param data_expanded tibble produced by [expand_data()]
#' @param tpa_rules_path file path to `DESIGNCD_TPA.csv` which contains rules
#'   on how to assign values for `TPA_UNADJ` based on design code and
#'   (interpolated) tree diameter.
#' @export
#' @returns a tibble
interpolate_data <- function(
  data_expanded,
  tpa_rules_path = here::here("data/DESIGNCD_TPA.csv")
) {
  cli::cli_progress_step("Interpolating between surveys")
  #variables to linearly interpolate/extrapolate
  cols_interpolate <- c("ACTUALHT", "DIA", "HT", "CULL", "CR", "CONDPROP_UNADJ")
  #variables that switch at the midpoint (rounded down) between surveys
  cols_midpt_switch <- c(
    "STATUSCD",
    "RECONCILECD",
    "DECAYCD",
    "STANDING_DEAD_CD",
    "STDORGCD",
    "CONDID",
    "COND_STATUS_CD"
  )

  # #read in TPA_UNADJ joining rules
  # tpa_rules <- readr::read_csv(
  #   here::here("data/DESIGNCD_TPA.csv"),
  #   show_col_types = FALSE
  # )

  data_expanded |>
    dplyr::group_by(tree_ID) |>
    dplyr::mutate(
      #linearly interpolate/extrapolate
      dplyr::across(
        dplyr::all_of(cols_interpolate),
        \(var) inter_extra_polate(x = YEAR, y = var)
      ),
      #interpolate to switch at midpoint
      dplyr::across(dplyr::all_of(cols_midpt_switch), step_interp)
    ) |>
    # convert 999 back to NA for some vars
    dplyr::mutate(dplyr::across(
      dplyr::all_of(cols_midpt_switch),
      \(x) dplyr::if_else(x == 999, NA, x)
    )) |>
    dplyr::ungroup() |>
    #join TPA_UNADJ
    dplyr::left_join(
      tpa_rules,
      by = dplyr::join_by(
        DESIGNCD,
        dplyr::between(DIA, min_DIA, max_DIA, bounds = "[)")
      )
    ) |>
    dplyr::select(-min_DIA, -max_DIA)
}
