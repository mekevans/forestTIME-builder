#' Add nsvb variables table to database
#'
#' @param con database connection
#'
#' @return nothing
#' @export
#' @importFrom DBI dbListTables dbSendStatement
#' @importFrom dplyr collect select distinct arrange group_by mutate ungroup left_join summarize n filter cross_join join_by lead inner_join
#' @importFrom arrow to_duckdb
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
    mutate(ACTUALHT = ifelse(is.na(ACTUALHT),
                             HT,
                             ACTUALHT),
           CULL = ifelse(is.na(CULL), 0, CULL)) |>
    left_join(tbl(con, "plot") |>
                select(PLT_CN, ECOSUBCD)) |>
    left_join(tbl(con, "cond") |>
                select(PLT_CN, CONDID, STDORGCD, COND_STATUS_CD)) |>
    left_join(
      tbl(con, "ref_species") |>
        select(
          SPCD,
          JENKINS_SPGRPCD,
          SFTWD_HRDWD,
          WOOD_SPGR_GREENVOL_DRYWT,
          CARBON_RATIO_LIVE
        ) |>
        rename(WDSG = WOOD_SPGR_GREENVOL_DRYWT)
    ) |>
    left_join(
      tbl(con, "ref_tree_decay_prop") |>
        filter(DECAYCD == 3) |>
        mutate(CULL_DECAY_RATIO = DENSITY_PROP) |>
        select(
          SFTWD_HRDWD,
          CULL_DECAY_RATIO
        ) 
    ) |>
    left_join(
      tbl(con, "ref_tree_decay_prop") |>
        select(SFTWD_HRDWD,
               DECAYCD,
               DENSITY_PROP,
               BARK_LOSS_PROP,
               BRANCH_LOSS_PROP)
    ) |>
    left_join(
      tbl(con, "ref_tree_carbon_ratio_dead") |>
        select(SFTWD_HRDWD, DECAYCD, CARBON_RATIO)
    ) |>
    mutate(CULL_DECAY_RATIO = ifelse(STATUSCD == 1,
                                     CULL_DECAY_RATIO,
                                     1),
           STANDING_DEAD_CD = ifelse(STATUSCD == 1,
                                     0,
                                     STANDING_DEAD_CD),
           DECAYCD = ifelse(STATUSCD == 1,
                            0,
                            DECAYCD),
           DECAY_WD = ifelse(STATUSCD == 1,
                             1,
                             DENSITY_PROP),
           DECAY_BK = ifelse(STATUSCD == 1,
                             1,
                             BARK_LOSS_PROP),
           DECAY_BR = ifelse(STATUSCD == 1,
                             1,
                             BRANCH_LOSS_PROP),
           C_FRAC = ifelse(STATUSCD == 1,
                           CARBON_RATIO_LIVE * 100,
                           CARBON_RATIO * 100)) |>
    mutate(DEAD_AND_STANDING = STATUSCD == 2 && STANDING_DEAD_CD == 1,
           LIVE = STATUSCD == 1) |>
    filter(COND_STATUS_CD == 1,
           DEAD_AND_STANDING | LIVE) |>
    rename(TRE_CN = TREE_CN) |>
    select(-CONDID,
           -COND_STATUS_CD,
           -CARBON_RATIO_LIVE,
           -CARBON_RATIO,
           -DENSITY_PROP,
           -BARK_LOSS_PROP,
           -BRANCH_LOSS_PROP,
           -DEAD_AND_STANDING,
           -LIVE) |>
    collect()
  
  arrow::to_duckdb(nsvb_vars,
                   table_name = "nsvb_vars",
                   con = con)
  dbExecute(con,
            "CREATE TABLE nsvb_vars AS SELECT * FROM nsvb_vars")
  
  return()
  
}