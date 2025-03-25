#' Prepare data for carbon estimation
#'
#' Adds columns from reference tables and re-names variables to work with carbon
#' estimtion code provided by David Walker. Some of this simply has to do with
#' the Walker code using `[]` for filtering which doesn't work well with NAs.
#'
#' @param data_mortyr annualized tree table already adjusted for mortality by
#'   [adjust_mortality()].
#' @param ref_dir absolute path to directory containing REF_SPECIES.csv,
#'   REF_TREE_DECAY_PROP.csv, and REF_TREE_CARBON_RATIO_DEAD.csv.
#'
prep_carbon <- function(data_mortyr, ref_dir = here::here("data/rawdat/")) {
  #read in ref tables
  ref_species <-
    read_csv(fs::path(ref_dir, "REF_SPECIES.csv")) |>
    select(
      SPCD,
      JENKINS_SPGRPCD,
      SFTWD_HRDWD,
      CARBON_RATIO_LIVE,
      WDSG = WOOD_SPGR_GREENVOL_DRYWT
    ) #TODO change this in walker code, not here

  ref_tree_decay_prop <-
    read_csv(fs::path(ref_dir, "REF_TREE_DECAY_PROP.csv")) |>
    select(
      SFTWD_HRDWD,
      DECAYCD,
      DENSITY_PROP,
      BARK_LOSS_PROP,
      BRANCH_LOSS_PROP
    )

  ref_tree_carbon_ratio_dead <-
    read_csv(fs::path(ref_dir, "REF_TREE_CARBON_RATIO_DEAD.csv")) |>
    select(SFTWD_HRDWD, DECAYCD, CARBON_RATIO)

  #remove trees with non positive values for HT and warn if there were any
  weird_obs <- data_mortyr |>
    filter(HT <= 0 | is.na(HT))
  if (nrow(weird_obs) > 0) {
    warning(
      nrow(weird_obs),
      " observations removed due to negative or missing HT values"
    )
    data_mortyr <- data_mortyr |>
      filter(HT > 0 & !is.na(HT))
  }

  data_mortyr |>
    ungroup() |>
    #join to get species properties
    left_join(ref_species, by = join_by(SPCD)) |>
    #first joins by SFTWD_HRDWD only the DENSITY_PROP column for DECAYCD 3, but calls it CULL_DECAY_RATIO
    left_join(
      ref_tree_decay_prop |>
        filter(DECAYCD == 3) |>
        select(
          SFTWD_HRDWD,
          #not sure I understand the naming of this variable
          CULL_DECAY_RATIO = DENSITY_PROP
        ),
      by = join_by(SFTWD_HRDWD)
    ) |>
    #then joins additional columns (including DENSITY_PROP) based on DECAYCD and SFTWD_HRDWD
    left_join(ref_tree_decay_prop, by = join_by(DECAYCD, SFTWD_HRDWD)) |> 
    left_join(ref_tree_carbon_ratio_dead, by = join_by(DECAYCD, SFTWD_HRDWD)) |>
    #TODO Why is CULL_DECAY_RATIO set to 1 when trees are dead?
    mutate(
      # CULL_DECAY_RATIO = if_else(STATUSCD == 1, CULL_DECAY_RATIO, 1),
      # STANDING_DEAD_CD = if_else(STATUSCD == 1, 0, STANDING_DEAD_CD),
      #TODO shouldn't DECAYCD actually get ajusted based on STANDING_DEAD_CD?
      # DECAYCD = if_else(STATUSCD == 1, 0, DECAYCD),
      #additional variables for walker code only
      DECAY_WD = if_else(STATUSCD == 1, 1, DENSITY_PROP),
      DECAY_BK = if_else(STATUSCD == 1, 1, BARK_LOSS_PROP),
      DECAY_BR = if_else(STATUSCD == 1, 1, BRANCH_LOSS_PROP),
      #TODO: why is this called C_FRAC if it is a percentage?
      C_FRAC = if_else(
        STATUSCD == 1,
        CARBON_RATIO_LIVE * 100,
        CARBON_RATIO * 100
      )
    ) 
}
