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
#' @note
#' If `HT` or `ACTUALHT` are extrapolated to values < 4.5 (or < 1 for woodland
#' species) OR `DIA` is extrapolated to < 1, the tree is marked as fallen dead
#' (`STATUSCD` 2 and `STANDING_DEAD_CD` 0). All measurements for these trees
#' will be removed (set to `NA`) by [adjust_mortality()]. Trees with only one
#' measurement have that measurement carried forward as appropriate (e.g. until
#' fallen and dead or in non-sampled condition).
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

  data_interpolated <- data_expanded |>
    dplyr::group_by(plot_ID, tree_ID) |>
    dplyr::mutate(
      #linearly interpolate/extrapolate
      dplyr::across(
        dplyr::any_of(cols_interpolate),
        \(var) inter_extra_polate(x = YEAR, y = var)
      ),
      #interpolate to switch at midpoint
      dplyr::across(dplyr::any_of(cols_midpt_switch), step_interp)
    ) |>
    # convert 999 back to NA for some vars
    dplyr::mutate(dplyr::across(
      dplyr::any_of(cols_midpt_switch),
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

  # If trees are interpolated to below FIA thresholds for being measured, set
  # them to fallen dead. For most trees, this is DIA < 1 and ACTUALHT < 4.5.
  # For woodland species, the ACTUALHT threshold is 1. To figure out if a tree
  # is a woodland species, we need to pull in one of the ref tables
  # temporarily.

  ref_species <-
    REF_SPECIES |>
    dplyr::select(
      SPCD,
      JENKINS_SPGRPCD
    )

  data_interpolated |>
    dplyr::left_join(ref_species, by = dplyr::join_by(SPCD)) |>
    #TODO is there a way of only having to do the case_when once?  E.g. would it
    #be faster to create a column "dead_fallen" and then in a subsequent step
    #use dead_fallen to set STATUSCD and STANDING_DEAD_CD? this function feels a
    #lot slower since adding this bit
    dplyr::mutate(
      STATUSCD = dplyr::case_when(
        JENKINS_SPGRPCD < 10 & (DIA < 1 | HT < 4.5 | ACTUALHT < 4.5) ~ 2,
        JENKINS_SPGRPCD == 10 & (DIA < 1 | HT < 1 | ACTUALHT < 1) ~ 2,
        .default = STATUSCD
      ),
      STANDING_DEAD_CD = dplyr::case_when(
        JENKINS_SPGRPCD < 10 & (DIA < 1 | HT < 4.5 | ACTUALHT < 4.5) ~ 0,
        JENKINS_SPGRPCD == 10 & (DIA < 1 | HT < 1 | ACTUALHT < 1) ~ 0,
        .default = STANDING_DEAD_CD
      )
    ) |> 
    dplyr::select(-JENKINS_SPGRPCD)
}
