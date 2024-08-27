#' Add tree_annualized table to database
#'
#' @param con database connection
#'
#' @return nothing
#' @export
#' @importFrom DBI dbListTables dbSendStatement
#' @importFrom dplyr collect select distinct arrange group_by mutate ungroup left_join summarize n filter cross_join join_by lead inner_join
#' @importFrom arrow to_duckdb
add_annual_estimates_to_db <- function(con) {
  existing_tables <- dbListTables(con)
  
  if ("tree_annualized" %in% existing_tables) {
    message("tree_annualized table already present in database!")
    return()
  }
  
  if (!(all(c("tree", "tree_info_composite_id") %in% existing_tables))) {
    message(
      "At least one of tree, tree_info_composite_id tables not present in database; needed for tree_annualized!"
    )
    return()
  }
  
  if (!("all_invyrs" %in% existing_tables)) {
    all_invyrs <- data.frame(INVYR = c(2000:2024))
    
    arrow::to_duckdb(all_invyrs, con, "all_invyrs")
    dbSendStatement(con, "CREATE TABLE all_invyrs AS SELECT * FROM all_invyrs")
  }
  
  trees <- tbl(con, "tree")  |>
    mutate(ACTUALHT = as.numeric(ACTUALHT),
           HT = as.numeric(HT),
           DIA = as.numeric(DIA),
           MORTYR = as.numeric(MORTYR),
           CARBON_AG = as.numeric(CARBON_AG),
           CARBON_BG = as.numeric(CARBON_BG),
           INVYR = as.numeric(INVYR)) |>
    left_join(tbl(con, "tree_info_composite_id")) |>
    filter(NRECORDS > 1) |>
    filter(!is.na(DIA),!is.na(HT),!is.na(ACTUALHT),
           !is.na(CARBON_AG), !is.na(CARBON_BG)) |>
    select(
      TREE_COMPOSITE_ID,
      INVYR,
      DIA,
      HT,
      ACTUALHT,
      CARBON_AG,
      CARBON_BG,
      TREE_CN,
      PLT_CN,
      CONDID,
      MORTYR,
      STATUSCD,
      DEATH,
      DAMAGE,
      DISTURBANCE
    )  |>
    group_by(TREE_COMPOSITE_ID) |>
    mutate(
      NRECORDS_NONA = n(),
      next_INVYR = lead(INVYR, order_by = INVYR),
      next_DIA = lead(DIA, order_by = INVYR),
      next_HT = lead(HT, order_by = INVYR),
      next_ACTUALHT = lead(ACTUALHT, order_by = INVYR),
      next_CARBONAG = lead(CARBONAG, order_by = INVYR),
      next_CARBONBG = lead(CARBONBG, order_by = INVYR),
      last_MORTYR = max(MORTYR),
      first_INVYR = min(INVYR)
    ) |>
    filter(NRECORDS_NONA > 1) |>
    mutate(next_INVYR = next_INVYR - 1) |>
    mutate(next_INVYR = ifelse(is.na(next_INVYR), INVYR, next_INVYR)) |>
    mutate(INVYR_diff = next_INVYR + 1 - INVYR) |>
    mutate(MORTYR_diff = ifelse(
      is.na(last_MORTYR),
      INVYR_diff,
      ifelse(last_MORTYR < next_INVYR,
             last_MORTYR - INVYR,
             INVYR_diff)
    )) |>
    mutate(
      next_DIA = ifelse(is.na(next_DIA), DIA, next_DIA),
      next_HT = ifelse(is.na(next_HT), HT, next_HT),
      next_ACTUALHT = ifelse(is.na(next_ACTUALHT), ACTUALHT, next_ACTUALHT),
      next_CARBONAG = ifelse(is.na(next_CARBONAG), CARBONAG, next_CARBONAG),
      next_CARBONBG = ifelse(is.na(next_CARBONBG), CARBONBG, next_CARBONBG)
    ) |> 
    mutate(
      DIA_slope = (next_DIA - DIA) / INVYR_diff,
      HT_slope = (next_HT - HT) / INVYR_diff,
      ACTUALHT_slope = (next_ACTUALHT - ACTUALHT) / INVYR_diff,
      CARBONBG_slope = (next_CARBONBG - CARBONBG) / INVYR_diff,
      CARBONAG_slope = (next_CARBONAG - CARBONAG) / INVYR_diff,
      DIA_slope_mort = (next_DIA - DIA) / MORTYR_diff,
      HT_slope_mort = (next_HT - HT) / MORTYR_diff,
      ACTUALHT_slope_mort = (next_ACTUALHT - ACTUALHT) / MORTYR_diff,
      CARBONAG_slope_mort = (next_CARBONAG - CARBONAG) / MORTYR_diff,
      CARBONBG_slope_mort = (next_CARBONBG - CARBONBG) / MORTYR_diff
      
    ) 
  
  all_years <- tbl(con, "tree") |>
    select(TREE_COMPOSITE_ID) |>
    distinct() |>
    cross_join(tbl(con, "all_invyrs")) |>
    arrange(TREE_COMPOSITE_ID, INVYR) |>
    rename(YEAR = INVYR)
  
  by <-
    join_by(TREE_COMPOSITE_ID,
            between(YEAR, INVYR, next_INVYR, bounds = "[]"))
  
  trees_annual_measures <- all_years |>
    inner_join(trees, by) |>
    mutate(
      time_run = YEAR - INVYR,
      DIA_start = DIA,
      HT_start = HT,
      ACTUALHT_start = ACTUALHT,
      CARBONBG_start = CARBONBG,
      CARBONAG_start = CARBONAG
    ) |>
    mutate(
      DIA_est = DIA_start + (DIA_slope * time_run),
      HT_est = HT_start + (HT_slope * time_run),
      ACTUALHT_est = ACTUALHT_start + (ACTUALHT_slope * time_run),
      CARBONBG_est = CARBONBG_start + (CARBONBG_slope * time_run),
      CARBONAG_est = CARBONAG_start + (CARBONAG_slope * time_run),
            DIA_est_mort = ifelse(
        !is.na(last_MORTYR) && YEAR > last_MORTYR,
        next_DIA,
        DIA_start + (DIA_slope_mort * time_run)
      ),
      HT_est_mort = ifelse(
        !is.na(last_MORTYR) && YEAR > last_MORTYR,
        next_HT,
        HT_start + (HT_slope_mort * time_run)
      ),
      ACTUALHT_est_mort = ifelse(
        !is.na(last_MORTYR) && YEAR > last_MORTYR,
        next_ACTUALHT,
        ACTUALHT_start + (ACTUALHT_slope_mort * time_run)
      ),
      CARBONAG_est_mort = ifelse(
        !is.na(last_MORTYR) && YEAR > last_MORTYR,
        next_CARBONAG,
        CARBONAG_start + (CARBONAG_slope_mort * time_run)
      ),
      CARBONBG_est_mort = ifelse(
        !is.na(last_MORTYR) && YEAR > last_MORTYR,
        next_CARBONBG,
        CARBONBG_start + (CARBONBG_slope_mort * time_run)
      )
    ) |>
    arrange(TREE_COMPOSITE_ID, YEAR) |>
    select(
      TREE_COMPOSITE_ID,
      TREE_CN,
      PLT_CN,
      CONDID,
      YEAR,
      DIA_est,
      HT_est,
      ACTUALHT_est,
      CARBONAG_est,
      CARBONBG_est,
      DIA_est_mort,
      HT_est_mort,
      ACTUALHT_est_mort,
      CARBONAG_est_mort,
      CARBONBG_est_mort,
      last_MORTYR,
      STATUSCD,
      DEATH,
      DAMAGE,
      DISTURBANCE
    ) |>
    collect()
  
  arrow::to_duckdb(trees_annual_measures,
                   table_name = "tree_annualized",
                   con = con)
  dbExecute(con,
            "CREATE TABLE tree_annualized AS SELECT * FROM tree_annualized")
  
  return()
  
}