source("R/utils.R")
adjust_mortality <- function(data_interpolated, use_mortyr = TRUE) {
  cli_progress_step("Adjusting for mortality")

  if (isTRUE(use_mortyr)) {
    df <- data_interpolated |>
      group_by(tree_ID) |>
      mutate(
        #to use MORTYR
        first_dead = ifelse(
          test = any(!is.na(MORTYR)), #has recorded MORTYR
          yes = max(MORTYR, na.rm = TRUE),
          no = YEAR[min(which(STATUSCD == 2) %|||% NA)]
        )
      )
  } else {
    df <- data_interpolated |>
      group_by(tree_ID) |>
      mutate(
        first_dead = YEAR[min(which(STATUSCD == 2) %|||% NA)]
      )
  }

  df |>
    #drop trees that transition to STATUSCD 0 and RECONCILECD 5, 6, or 9
    #https://github.com/mekevans/forestTIME-builder/issues/59
    filter(!(STATUSCD == 0 & RECONCILECD %in% c(5, 6, 9))) |>
    #then adjust STATUSCD & DECAYCD
    group_by(tree_ID) |>
    mutate(
      STATUSCD = if_else(YEAR >= first_dead, 2, STATUSCD, missing = STATUSCD)
    ) |>
    #MORTYR might be earlier than the midpoint, so backfill NAs for DECAYCD and STANDING_DEAD_CD
    tidyr::fill(DECAYCD, STANDING_DEAD_CD, .direction = "up") |>
    #But, STANDING_DEAD_CD only applies to dead trees
    mutate(STANDING_DEAD_CD = if_else(STATUSCD == 2, STANDING_DEAD_CD, NA)) |>
    #and DECAYCD only applies to standing dead trees > 4.9 DIA
    mutate(DECAYCD = if_else(STANDING_DEAD_CD == 1 & DIA > 4.9, DECAYCD, NA)) |>
    #fallen trees shouldn't have measurements for anything
    mutate(
      across(
        c(DIA, HT, ACTUALHT, CULL, CR),
        \(x) if_else(STANDING_DEAD_CD == 0, NA, x, missing = x)
      )
    ) |>
    select(-MORTYR, -first_dead) #don't need this anymore?
}
