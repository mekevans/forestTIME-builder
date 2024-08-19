#' Pull records from the sapling transition tables
#'
#' @param con database connection
#' @param conditions use `create_conditions` to create a list of logical conditions involving columns from the PLOT table
#' @param variables character vector or variables to return
#'
#' @return data frame of sapling transitions + any additional variables for plots satisfying conditions
#' @export
query_saplings <- function(con,
                           conditions = create_conditions(...),
                           variables = c("STATECD")) {
  # Connect to tables
  
  saplings <- tbl(con, "sapling_transitions")
  
  plots <-
    tbl(con, "plot") |>  select(-any_of(
      c(
        "CREATED_BY",
        "CREATED_DATE",
        "CREATED_IN_INSTANCE",
        "MODIFIED_BY",
        "MODIFIED_DATE",
        "MODIFIED_IN_INSTANCE"
      )
    ))
  
  
  # Prepare variables to pull
  
  
  needed_variables <- c(
    'PLOT_COMPOSITE_ID',
    'PLOT',
    'COUNTYCD',
    'STATECD',
    'PLT_CN',
    'INVYR',
    'PREV_INVYR',
    'CYCLE',
    'live_sapling',
    'new_sapling',
    'sapling_sapling',
    'sapling_tree',
    'sapling_dead',
    'sapling_removed',
    'sapling_not_sampled',
    'sapling_missing_data',
    'sapling_vanishes_next_year',
    'PREV_live_sapling',
    'sapling_vanished',
    'presumed_dead',
    'timespan',
    'sapling_sapling_prop',
    'sapling_tree_prop',
    'sapling_removed_prop',
    'presumed_dead_prop',
    'sapling_not_sampled_prop',
    'sapling_missing_data_prop'
  )
  
  all_variables <- c(needed_variables, variables)
  
  
  # Pull saplings
  
  sapling_transitions <- saplings |>
    left_join(plots) |>
    filter(!!!conditions) |>
    select(all_of(all_variables)) |>
    collect()
  
  sapling_transitions
}