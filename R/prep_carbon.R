#' Prepare data for carbon estimation
#'
#' Adds columns from reference tables and re-names variables to work with carbon
#' estimtion code provided by David Walker. Some of this simply has to do with
#' the Walker code using `[]` for filtering which doesn't work well with NAs.
#'
#' @param data_mortyr annualized tree table already adjusted for mortality by
#'   [adjust_mortality()].
#' @noRd
#' @returns a tibble
prep_carbon <- function(data_mortyr) {
  cli::cli_progress_step("Prepping for estimating carbon")
  #read in ref tables
  ref_species <-
    REF_SPECIES |>
    dplyr::select(
      SPCD,
      JENKINS_SPGRPCD,
      SFTWD_HRDWD,
      CARBON_RATIO_LIVE,
      WDSG = WOOD_SPGR_GREENVOL_DRYWT
    ) #TODO change this in walker code, not here

  ref_tree_decay_prop <-
    REF_TREE_DECAY_PROP |>
    dplyr::select(
      SFTWD_HRDWD,
      DECAYCD,
      DENSITY_PROP,
      BARK_LOSS_PROP,
      BRANCH_LOSS_PROP
    )

  ref_tree_carbon_ratio_dead <-
    REF_TREE_CARBON_RATIO_DEAD |>
    dplyr::select(SFTWD_HRDWD, DECAYCD, CARBON_RATIO)

  data_mortyr |>
    dplyr::ungroup() |>
    #join to get species properties
    dplyr::left_join(ref_species, by = dplyr::join_by(SPCD)) |>
    # Use density reduction for DECAYCD 3 for live trees (described in appendix K of FIADB User Guides)
    dplyr::left_join(
      ref_tree_decay_prop |>
        dplyr::filter(DECAYCD == 3) |>
        dplyr::select(
          SFTWD_HRDWD,
          CULL_DECAY_RATIO = DENSITY_PROP
        ),
      by = dplyr::join_by(SFTWD_HRDWD)
    ) |>
    # then joins additional columns (including DENSITY_PROP) based on DECAYCD and SFTWD_HRDWD
    dplyr::left_join(
      ref_tree_decay_prop,
      by = dplyr::join_by(DECAYCD, SFTWD_HRDWD)
    ) |>
    dplyr::left_join(
      ref_tree_carbon_ratio_dead,
      by = dplyr::join_by(DECAYCD, SFTWD_HRDWD)
    ) |>
    #TODO Why is CULL_DECAY_RATIO set to 1 when trees are dead?
    dplyr::mutate(
      CULL_DECAY_RATIO = dplyr::if_else(STATUSCD == 1, CULL_DECAY_RATIO, 1),
      STANDING_DEAD_CD = dplyr::if_else(STATUSCD == 1, 0, STANDING_DEAD_CD),
      #TODO shouldn't DECAYCD actually get ajusted based on STANDING_DEAD_CD?
      DECAYCD = dplyr::if_else(STATUSCD == 1, 0, DECAYCD),
      #additional variables for walker code only
      DECAY_WD = dplyr::if_else(STATUSCD == 1, 1, DENSITY_PROP),
      DECAY_BK = dplyr::if_else(STATUSCD == 1, 1, BARK_LOSS_PROP),
      DECAY_BR = dplyr::if_else(STATUSCD == 1, 1, BRANCH_LOSS_PROP),
      #TODO: why is this called C_FRAC if it is a percentage?
      C_FRAC = dplyr::if_else(
        STATUSCD == 1,
        CARBON_RATIO_LIVE * 100,
        CARBON_RATIO * 100
      )
    ) |> 
    dplyr::mutate(
      PROVINCE = getDivision(ECOSUBCD, TRUE),
      DIVISION = getDivision(ECOSUBCD)
    ) |>
    # I think this is only necessary because this code uses [] for indexing
    # instead of `filter()`
    dplyr::mutate(
      dplyr::across(
        c(DECAYCD, STANDING_DEAD_CD),
        \(x) dplyr::if_else(STATUSCD == 1, 0, x)
      ),
      CULL = ifelse(is.na(CULL), 0, CULL)
    )
}
