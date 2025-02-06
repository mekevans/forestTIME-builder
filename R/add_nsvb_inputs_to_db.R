#TODO why is this in the database?  Will users interact with this table or is it just used to calculate carbon estimates?

#' Add nsvb variables table to database
#'
#' @param con database connection
#'
#' @return nothing
#' @export
#' @importFrom DBI dbListTables dbSendStatement
#' @importFrom dplyr collect select distinct arrange group_by mutate ungroup left_join summarize n filter cross_join join_by lead inner_join
add_nsvb_vars_to_db <- function(con) {
  
  existing_tables <- dbListTables(con)
  
  if ("nsvb_vars" %in% existing_tables) {
    message("nsvb_vars table already present in database!")
    return()
  }
  
  if (!(all(c("tree", "plot", "ref_species", "ref_tree_decay_prop", "ref_tree_carbon_ratio_dead") %in% existing_tables))) {
    message(
      "Missing a table needed for nsvb_vars!"
    )
    return()
  }
  
  nsvb_vars <- tbl(con, "tree") |>
    select(
      PLT_CN,
      TREE_CN,
      STATUSCD,
      DIA,
      HT,
      ACTUALHT,
      CONDID,
      SPCD,
      TREECLCD,
      CULL,
      VOLCFGRS,
      DRYBIO_AG,
      CARBON_AG,
      STANDING_DEAD_CD,
      DECAYCD,
      CR
    ) |>
    mutate(ACTUAL_HT = as.numeric(ACTUALHT),
           HT = as.numeric(HT),
           DIA = as.numeric(DIA),
           CULL = as.numeric(CULL)) |>
    mutate(ACTUALHT = ifelse(is.na(ACTUALHT),
                             HT,
                             as.numeric(ACTUALHT)),
           CULL = ifelse(is.na(CULL), 0, CULL)) |>
    left_join(tbl(con, "plot") |>
                select(PLT_CN, ECOSUBCD),
              by = join_by(PLT_CN)) |>
    left_join(tbl(con, "cond") |>
                select(PLT_CN, CONDID, STDORGCD, COND_STATUS_CD),
              by = join_by(PLT_CN, CONDID)) |>
    left_join(
      tbl(con, "ref_species") |>
        select(
          SPCD,
          JENKINS_SPGRPCD,
          SFTWD_HRDWD,
          WOOD_SPGR_GREENVOL_DRYWT,
          CARBON_RATIO_LIVE
        ) |>
        #TODO why rename?
        rename(WDSG = WOOD_SPGR_GREENVOL_DRYWT),
      by = join_by(SPCD)
    ) |>
    #TODO why is CULL_DECAY_RATIO the DENSITY_PROP for DECAYCD 3?
    left_join(
      tbl(con, "ref_tree_decay_prop") |>
        filter(DECAYCD == 3) |>
        mutate(CULL_DECAY_RATIO = DENSITY_PROP) |>
        select(
          SFTWD_HRDWD,
          CULL_DECAY_RATIO
        ),
      by = join_by(SFTWD_HRDWD) 
    ) |>
    #TODO what about all the NAs for DECAYCD?
    left_join(
      tbl(con, "ref_tree_decay_prop") |>
        select(SFTWD_HRDWD,
               DECAYCD,
               DENSITY_PROP,
               BARK_LOSS_PROP,
               BRANCH_LOSS_PROP),
      by = join_by(DECAYCD, SFTWD_HRDWD)
    ) |>
    left_join(
      tbl(con, "ref_tree_carbon_ratio_dead") |>
        select(SFTWD_HRDWD, DECAYCD, CARBON_RATIO),
      by = join_by(DECAYCD, SFTWD_HRDWD)
    ) |> 
    #TODO Why is CULL_DECAY_RATIO set to 1 when trees are dead?
    mutate(CULL_DECAY_RATIO = ifelse(STATUSCD == 1,
                                     CULL_DECAY_RATIO,
                                     1),
           STANDING_DEAD_CD = ifelse(STATUSCD == 1,
                                     0,
                                     as.numeric(STANDING_DEAD_CD)),
           # TODO: why is this important?  Why not just use NA?
           DECAYCD = ifelse(STATUSCD == 1,
                            0,
                            as.numeric(DECAYCD)),
           # TODO: why are these variables created?
           DECAY_WD = ifelse(STATUSCD == 1,
                             1,
                             DENSITY_PROP),
           DECAY_BK = ifelse(STATUSCD == 1,
                             1,
                             BARK_LOSS_PROP),
           DECAY_BR = ifelse(STATUSCD == 1,
                             1,
                             BRANCH_LOSS_PROP),
           #TODO: why is this called C_FRAC if it is a percentage?
           C_FRAC = ifelse(STATUSCD == 1,
                           CARBON_RATIO_LIVE * 100,
                           CARBON_RATIO * 100)) |>
    mutate(STATUSCD = as.numeric(STATUSCD),
           STANDING_DEAD_CD = as.numeric(STANDING_DEAD_CD),
           COND_STATUS_CD = as.numeric(COND_STATUS_CD)) |>
    mutate(DEAD_AND_STANDING = STATUSCD == 2 && STANDING_DEAD_CD == 1,
           LIVE = STATUSCD == 1) |>
    filter(COND_STATUS_CD == 1,
           DEAD_AND_STANDING | LIVE) |>
    #TODO: why is this renamed?
    rename(TRE_CN = TREE_CN) |>
    select(-CONDID,
           -COND_STATUS_CD,
           -CARBON_RATIO_LIVE,
           -CARBON_RATIO,
           -DENSITY_PROP,
           -BARK_LOSS_PROP,
           -BRANCH_LOSS_PROP,
           -DEAD_AND_STANDING,
           -LIVE) 
  copy_to(
    dest = con,
    df = nsvb_vars,
    name = "nsvb_vars",
    temporary = FALSE
  )
  
  return()
}
