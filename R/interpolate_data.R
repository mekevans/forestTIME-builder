#' Interpolate expanded tree data
#'
#' Fills in `NA`s between survey years with either linear interpolation /
#' extrapolation or by switching categorical variables at the midpoint (rounded
#' down) between surveys. Linear interpolation/extrapolation is accomplished
#' with [inter_extra_polate()] and the categorical variables are handled with
#' [step_interp()]. Also converts temporary `999` values created by
#' [expand_data()] back to `NA`s.  This also assigns a value for `TPA_UNADJ`
#' based on `DESIGNCD` and interpolated values of `DIA` according to Appendix G
#' of the FIADB user guide.
#'
#' @references
#' Burrill, E.A., Christensen, G.A., Conkling, B.L., DiTommaso, A.M.,
#' Kralicek, K.M., Lepine, L.C., Perry, C.J., Pugh, S.A., Turner, J.A.,
#' Walker, D.M., 2024. The Forest Inventory and Analysis Database User Guide
#' (NFI). USDA Forest Service. <https://research.fs.usda.gov/understory/forest-inventory-and-analysis-database-user-guide-nfi>
#'
#' @param data_expanded tibble produced by [expand_data()]
#' @export
#' @returns a tibble
interpolate_data <- function(data_expanded) {
  cli::cli_progress_step("Interpolating between surveys")
  #variables to linearly interpolate/extrapolate
  cols_interpolate <- c("ACTUALHT", "DIA", "HT", "CULL", "CR", "CONDPROP_UNADJ")
  #variables that switch at the midpoint (rounded down) between surveys
  cols_midpt_switch <- c(
    "PLT_CN", #used to join to POP tables.  Possibly a better way...
    "STATUSCD",
    "RECONCILECD",
    "DECAYCD",
    "STANDING_DEAD_CD",
    "STDORGCD",
    "CONDID",
    "COND_STATUS_CD"
  )

  data_expanded |>
    dplyr::group_by(plot_ID, tree_ID) |>
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
