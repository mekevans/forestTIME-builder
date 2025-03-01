#' Add tree_info_composite_id table to database
#'
#' @param con database connection
#'
#' @return nothing
#' @export
#' @importFrom DBI dbListTables dbSendStatement
#' @importFrom dplyr collect select distinct arrange group_by mutate ungroup left_join summarize n
add_info_table_to_db <- function(con) {
  
  existing_tables <- dbListTables(con)
  
  if("tree_info_composite_id" %in% existing_tables) {
    message("tree_info table already present in database!")
    return()
  }
  
  if(!(all(c("tree", "tree_cns", "cond", "qa_flags") %in% existing_tables))) {
    message("At least one of tree, tree_cns, cond, qa_flags tables not present in database; needed for tree_info!")
    return()
  }
  
  cns_multiple_locations <- tbl(con, "tree") |>
    left_join(tbl(con, "tree_cns"), by = join_by(TREE_CN)) |>
    select(TREE_COMPOSITE_ID, TREE_FIRST_CN) |>
    distinct() |>
    group_by(TREE_FIRST_CN) |>
    mutate(N_COMPOSITE_IDS = n()) |>
    ungroup() |>
    mutate(MULTIPLE_LOCATIONS_FLAG = N_COMPOSITE_IDS > 1) |>
    select(TREE_COMPOSITE_ID, MULTIPLE_LOCATIONS_FLAG) |>
    distinct()
  
  multiple_cns <- tbl(con, "tree") |>
    left_join(tbl(con, "tree_cns"), by = join_by(TREE_CN)) |> 
    select(TREE_COMPOSITE_ID, TREE_FIRST_CN) |>
    distinct() |>
    group_by(TREE_COMPOSITE_ID) |>
    summarize(N_FIRST_CNS = n()) |>
    ungroup() |>
    mutate(MULTIPLE_CNS_FLAG = N_FIRST_CNS > 1) |>
    select(TREE_COMPOSITE_ID, MULTIPLE_CNS_FLAG)
  
  multiple_owners <- tbl(con, "tree") |>
    left_join(tbl(con, "cond"), by = join_by(PLT_CN, INVYR, STATECD, UNITCD, COUNTYCD, PLOT, CONDID, CREATED_DATE, MODIFIED_DATE, CYCLE, SUBCYCLE, PLOT_COMPOSITE_ID)) |>
    select(TREE_COMPOSITE_ID, OWNCD, ADFORCD) |>
    distinct() |>
    group_by(TREE_COMPOSITE_ID, OWNCD) |>
    summarize(n_ADFORCD = n()) |>
    ungroup() |>
    group_by(TREE_COMPOSITE_ID) |>
    summarize(n_OWNCD = n(),
              n_ADFORCD = sum(n_ADFORCD)) |>
    ungroup() |>
    mutate(MULTI_OWNCD_FLAG = n_OWNCD > 1,
           MULTI_ADFORCD_FLAG = n_ADFORCD > 1) |>
    select(TREE_COMPOSITE_ID,
           MULTI_OWNCD_FLAG,
           MULTI_ADFORCD_FLAG)
  
  death_damage_disturbance <- tbl(con, "tree") |>
    left_join(tbl(con, "cond"), by = join_by(PLT_CN, INVYR, STATECD, UNITCD, COUNTYCD, PLOT, CONDID, CREATED_DATE, MODIFIED_DATE, CYCLE, SUBCYCLE, PLOT_COMPOSITE_ID)) |>
    select(TREE_COMPOSITE_ID, INVYR, CONDID, STATUSCD, DSTRBCD1,
           DSTRBCD2, DSTRBCD3, DAMSEV1, DAMSEV2) |>
    mutate(across(starts_with("DSTRBCD"), as.numeric)) |> # if all NAs, these columns can be read in as character and downstream operations will fail
    mutate(DISTURBED = (DSTRBCD1 + DSTRBCD2 + DSTRBCD3) > 0,
           DAMAGED = !is.na(DAMSEV1) | !is.na(DAMSEV2)) |>
    mutate(DISTURBED = ifelse(is.na(DISTURBED),
                              FALSE,
                              DISTURBED)) |>
    group_by(TREE_COMPOSITE_ID) |>
    summarize(
      DEATH = any(STATUSCD == 2),
      DISTURBANCE = any(DISTURBED),
      DAMAGE = any(DAMAGED)) 
  
  tree_info_composite_id <- tbl(con, "tree") |>
    left_join(tbl(con, "qa_flags"), by = join_by(TREE_CN, INVYR, STATUSCD, SPCD, CYCLE, TREE_COMPOSITE_ID)) |>
    group_by(TREE_COMPOSITE_ID,
             PLOT_COMPOSITE_ID,
             PLOT,
             SUBP,
             STATECD,
             COUNTYCD,
             SPCD_CORR) |>
    summarize(NRECORDS = dplyr::n(),
              FIRSTYR = min(INVYR),
              LASTYR = max(INVYR),
              ANY_SPCD_FLAG = any(SPCD_FLAG),
              ANY_STATUSCD_FLAG = any(STATUSCD_FLAG),
              ANY_CYCLE_VISITS_FLAG = any(CYCLE_MULTIPLE_VISITS)) |>
    ungroup() |> 
    left_join(cns_multiple_locations, by = join_by(TREE_COMPOSITE_ID)) |>
    left_join(multiple_cns, by = join_by(TREE_COMPOSITE_ID)) |>
    left_join(multiple_owners, by = join_by(TREE_COMPOSITE_ID)) |>
    left_join(death_damage_disturbance, by = join_by(TREE_COMPOSITE_ID)) |>
    copy_to(con, df = _, name = "tree_info_composite_id", temporary = FALSE)
  
  return()
  
}
