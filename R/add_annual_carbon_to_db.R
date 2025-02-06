#' Add tree_annualized table to database
#'
#' @param con database connection
#'
#' @return nothing
#' @export
#' @importFrom DBI dbListTables dbSendStatement
#' @importFrom dplyr collect select distinct  group_by mutate ungroup left_join summarize n filter cross_join join_by lead inner_join
add_annual_carbon_vars_to_db <- function(con) {
  existing_tables <- dbListTables(con)
  
  if ("tree_annualized_mortyr_carbon" %in% existing_tables |
      "tree_annualized_midpoint_carbon" %in% existing_tables) {
    message("tree_annualized table already present in database!")
    return()
  }
  
  if (!(all(c("tree", "tree_info_composite_id", 
              "tree_annualized") %in% existing_tables))) {
    message(
      "At least one of tree, tree_info_composite_id tables not present in database; needed for tree_annualized!"
    )
    return()
  }
  
  
  trees <- tbl(con, "tree")  
  all_annual_measures <- trees_annual_measures |>
    left_join(trees_annual_measures_mortyr) 
  
  copy_to(
    dest = con,
    df = all_annual_measures,
    name = "tree_annualized_mortyr_carbon_vars",
    temporary = FALSE
  )

  return()
}