
#' Add tree_cn table to database
#'
#' @param con database connection; must contain tree table
#'
#' @return nothing
#' @importFrom dplyr collect tbl
#' @importFrom DBI dbListTables dbSendQuery
#' @importFrom arrow to_duckdb
add_cns_to_db <- function(con) {
  
  existing_tables <- dbListTables(con)
  
  if("tree_cns" %in% existing_tables) {
    return(warning("tree_cn table already present in database!"))
  }
  
  if(!("tree" %in% existing_tables)) {
    return(warning("tree table not present in database; needed for cn table!"))
  }
  
  trees <- tbl(con, "tree") 
  
  chain_by_joins(trees) |>
    collect() |>
    arrow::to_duckdb(table_name = "tree_cns", con = con)
  
  dbExecute(con, "CREATE TABLE tree_cns AS SELECT * FROM tree_cns")
  
  return()
}


#' Create cns by chain by joins method
#'
#' @param tree_table tree table
#'
#' @return table of CN, TREE_FIRST_CN
#' @export
#'
#' @importFrom dplyr select distinct arrange collect compute mutate filter left_join 
chain_by_joins <- function(tree_table) {
  cycles <-
    tree_table |>
    select(INVYR) |>
    distinct() |>
    arrange(INVYR) |>
    collect()
  
  cycle_trees <- tree_table |>
    select(TREE_CN, PREV_TRE_CN, INVYR) |> compute()
  
  known_trees <- cycle_trees |>
    select(TREE_CN, INVYR) |>
    mutate(TREE_FIRST_CN = ifelse(INVYR == !!cycles$INVYR[1], TREE_CN, NA)) |>
    select(-INVYR) |>
    compute()
  
  for (i in 2:nrow(cycles)) {
    thiscycle_trees <- cycle_trees |>
      filter(INVYR == !!cycles$INVYR[i]) |>
      select(-INVYR) |>
      left_join(select(known_trees, TREE_CN, TREE_FIRST_CN),
                by = c("PREV_TRE_CN" = "TREE_CN")) |>
      mutate(TREE_FIRST_CN = 
               ifelse(is.na(TREE_FIRST_CN), 
                      TREE_CN, TREE_FIRST_CN)) |>
      compute()
    
    known_trees <- known_trees |>
      left_join(thiscycle_trees |> 
                  select(-PREV_TRE_CN), by = c("TREE_CN")) |>
      mutate(TREE_FIRST_CN = ifelse(
        is.na(TREE_FIRST_CN.x),
        ifelse(is.na(TREE_FIRST_CN.y), NA, TREE_FIRST_CN.y),
        TREE_FIRST_CN.x
      )) |>
      select(TREE_CN, TREE_FIRST_CN) |>
      compute()
    
  }
  
  known_trees <- known_trees |>
    left_join(tree_table) |>
    select(TREE_CN, TREE_FIRST_CN)
  
  known_trees
}


