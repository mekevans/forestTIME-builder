#' Add QA flag table to database
#'
#' @param con database connection
#'
#' @return nothing
#' @export
#' @importFrom DBI dbListTables dbSendStatement
#' @importFrom dplyr collect select distinct arrange group_by mutate ungroup
#' @importFrom arrow to_duckdb
add_qa_flags_to_db <- function(con) {
  
  existing_tables <- dbListTables(con)
  
  if("qa_flags" %in% existing_tables) {
    message("qa_flags table already present in database!")
    return()
  }
  
  if(!("tree" %in% existing_tables)) {
    message("tree table not present in database; needed for qa_flags!")
    return()
  }
  
  trees <- tbl(con, "tree")
  
  # Backfill SPCDs to last recorded SPCD  ####
  
  tree_latest_species <- trees |>
    select(TREE_CN, TREE_COMPOSITE_ID, INVYR, SPCD) |>
    distinct() |>
    group_by(TREE_COMPOSITE_ID) |>
    mutate(last_invyr = max(INVYR)) |>
    mutate(SPCD_CORR = ifelse(INVYR == last_invyr, SPCD, NA)) |>
    mutate(SPCD_CORR = max(SPCD_CORR)) |>
    ungroup() |>
    mutate(SPCD_FLAG = SPCD != SPCD_CORR) |>
    select(-last_invyr) 
  
  # Backfill status codes ####
  # any trees that are marked Dead and then later Alive receive a "5" which is a code I made up for "incorrectly marked dead". 
  trees_last_dead <- trees |>
    select(TREE_CN, TREE_COMPOSITE_ID, INVYR, STATUSCD) |>
    mutate(isdead = (STATUSCD %in% c(2, 3)))|> 
    group_by(TREE_COMPOSITE_ID) |>
    mutate(dead_invyr = ifelse(isdead, (INVYR), NA),
           live_invyr = ifelse(STATUSCD == 1, INVYR, NA)) |>
    mutate(first_dead_invyr = min(dead_invyr, na.rm = T),
           last_live_invyr = max(live_invyr, na.rm = T)) |>
    ungroup() |>
    mutate(zombie = last_live_invyr > first_dead_invyr) |>
    mutate(STATUSCD_CORR = ifelse(is.na(zombie), 
                                  STATUSCD,
                                  ifelse(zombie, 
                                         ifelse(INVYR <= last_live_invyr,
                                                ifelse(isdead, 
                                                       5, 
                                                       STATUSCD),
                                                STATUSCD),
                                         STATUSCD))) |>
    mutate(STATUSCD_FLAG = (STATUSCD != STATUSCD_CORR)) |>
    select(TREE_CN, TREE_COMPOSITE_ID, INVYR, STATUSCD, STATUSCD_CORR, STATUSCD_FLAG) 
  
  # Flag repeat visits in a cycle ####
  
  tree_cycles <- trees |>
    select(TREE_COMPOSITE_ID, CYCLE, INVYR) |>
    group_by(TREE_COMPOSITE_ID, CYCLE) |>
    mutate(first_INVYR = min(INVYR),
           last_INVYR = max(INVYR)) |>
    mutate(CYCLE_VISIT = ifelse(INVYR == first_INVYR, 1, 2)) |>
    mutate(CYCLE_MULTIPLE_VISITS = max(CYCLE_VISIT) > 1) |>
    mutate(LAST_CYCLE_VISIT = ifelse(CYCLE_MULTIPLE_VISITS, 
                                     ifelse(CYCLE_VISIT == 1, FALSE, TRUE),
                                     TRUE)) |>
    select(-first_INVYR, -last_INVYR, -CYCLE_VISIT) 
  
  # Combine all and add to db ####
  
  left_join(trees_last_dead, tree_latest_species) |>
    left_join(tree_cycles) |>
    collect() |>
    arrow::to_duckdb(table_name = "qa_flags", con = con)
  
  dbExecute(con, "CREATE TABLE qa_flags AS SELECT * FROM qa_flags")
  
  return()
  
}

