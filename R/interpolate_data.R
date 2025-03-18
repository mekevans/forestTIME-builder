#' Interpolate expanded tree data
#'
#' Fills in `NA`s between survey years with either linear
#' interpolation/extrapolation or by switching categorical variables at the
#' midpoint (rounded down) between surveys.  Linear interpolation/extrapolation
#' is accomplished with [inter_extra_polate()] and the categorical variables are
#' handled with [step_interp()].
#' 
#' @param data_expanded tibble produced by [expand_data()]
interpolate_data <- function(data_expanded) {
  #variables to linearly interpolate/extrapolate
  cols_interpolate <- c("ACTUALHT", "DIA", "HT", "CULL", "CR", "CONDPROP_UNADJ")
  #variables that switch at the midpoint (rounded down) between surveys
  cols_midpt_switch <- c("STATUSCD", "DECAYCD", "STANDING_DEAD_CD", "STDORGCD", "CONDID", "COND_STATUS_CD")
  
  data_expanded |> 
    dplyr::group_by(tree_ID) |> 
    dplyr::mutate(
      #linearly interpolate/extrapolate
      dplyr::across(dplyr::all_of(cols_interpolate),
             \(var) inter_extra_polate(x = YEAR, y = var)),
      #interpolate to switch at midpoint (rounded up)
      dplyr::across(dplyr::all_of(cols_midpt_switch), step_interp)
    ) |> 
    dplyr::ungroup()
}