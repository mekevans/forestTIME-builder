adjust_mortality <- function(data_interpolated, use_mortyr = TRUE) {
  if (isTRUE(use_mortyr)) {
    df <- data_interpolated |>
      group_by(tree_ID) |>
      mutate(
        #to use MORTYR
        first_dead = if_else(
          condition = any(!is.na(MORTYR)), #has recorded MORTYR
          true = max(MORTYR, na.rm = TRUE),
          # if no STATUSCD 2, prints warning, but it's ok because YEAR[Inf] is
          # NA, which is appropriate here
          # I'm counting STATUSCD 0 as "dying" (I *think* this means the tree was harvested)
          false = YEAR[min(which(STATUSCD == 2 | STATUSCD == 0))]
        )
      )
  } else {
    df <- data_interpolated |>
      group_by(tree_ID) |>
      mutate(
        first_dead = YEAR[min(which(STATUSCD == 2 | STATUSCD == 0))]
      )
  }

  df |>
    group_by(tree_ID) |> 
    #then adjust STATUSCD & DECAYCD
    mutate(STATUSCD = if_else(YEAR >= first_dead, 2, STATUSCD, missing = STATUSCD)) |> 
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
