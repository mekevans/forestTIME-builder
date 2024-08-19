library(duckdb)
library(dplyr)

#' Query tree surveys
#' 
#' Pull timeseries of records for individual trees. 
#' You can filter based on any logical conditions involving columns from the TREE, PLOT, COND, qa_flags, or tree_info_composite_id tables. 
#'
#' @param con database connection
#' @param conditions use `create_conditions` to create a set of conditions
#' @param variables character vector of variables to pull
#'
#' @return data frame of variables for all tree records satisfying conditions
#' @export
#'
query_tree_surveys <- function(con,
                            conditions = create_conditions(...),
                            variables = c("DIA")) {
  # Connect to tables
  
  trees <- tbl(con, "tree")
  
  tree_info <- tbl(con, "tree_info_composite_id")
  
  qa_flags <- tbl(con, "qa_flags")
  
  plots <-
    tbl(con, "plot") |> select(-any_of(
      c(
        "CREATED_BY",
        "CREATED_DATE",
        "CREATED_IN_INSTANCE",
        "MODIFIED_BY",
        "MODIFIED_DATE",
        "MODIFIED_IN_INSTANCE"
      )
    ))
  
  cond <-
    tbl(con, "cond")  |> select(-any_of(
      c(
        "CREATED_BY",
        "CREATED_DATE",
        "CREATED_IN_INSTANCE",
        "MODIFIED_BY",
        "MODIFIED_DATE",
        "MODIFIED_IN_INSTANCE"
      )
    ))
  
  # Prepare filter args
  
  filter_args <- conditions |>
    lapply(rlang::as_label) |>
    unlist() |>
    strsplit(split = " ") |>
    lapply(purrr::pluck, 1) |>
    unlist()
  
  tree_info_names <-
    c(
      'TREE_COMPOSITE_ID',
      'PLOT_COMPOSITE_ID',
      'PLOT',
      'SUBP',
      'STATECD',
      'COUNTYCD',
      'SPCD_CORR',
      'NRECORDS',
      'FIRSTYR',
      'LASTYR',
      'ANY_SPCD_FLAG',
      'ANY_STATUSCD_FLAG',
      'ANY_CYCLE_VISITS_FLAG',
      'MULTIPLE_LOCATIONS_FLAG',
      'MULTIPLE_CNS_FLAG',
      'MULTI_OWNCD_FLAG',
      'MULTI_ADFORCD_FLAG'
    )
  
  if (any(filter_args %in% tree_info_names)) {
    tree_info_filters <- conditions[which(filter_args %in% tree_info_names)]
  } else {
    tree_info_filters <- NULL
  }
  
  if (all(filter_args %in% tree_info_names)) {
    tree_filters <- NULL
  } else {
    tree_filters <-
      conditions[which(!(filter_args %in% tree_info_names))]
  }
  
  # Prepare variables to pull
  
  needed_variables <- c(
    'TREE_COMPOSITE_ID',
    'PLOT_COMPOSITE_ID',
    'SPCD',
    'PLOT',
    'SUBP',
    'COUNTYCD',
    'STATECD',
    'PLT_CN',
    'INVYR',
    'CYCLE',
    'MEASYEAR',
    'TREE_CN',
    'COND_CN',
    'CONDID'
  )
  
  all_variables <- c(needed_variables, variables)
  
  # Select trees
  
  selected_trees <- tree_info |>
    filter(!!!tree_info_filters)
  
  # Pull timeseries
  
  tree_timeseries <- selected_trees |>
    left_join(trees) |>
    left_join(qa_flags) |>
    left_join(plots) |>
    left_join(cond) |>
    filter(!!!tree_filters) |>
    select(all_of(all_variables)) |>
    collect()
  
  tree_timeseries
}

#' Create conditions
#'
#' Creates a list of conditions to pass to query_tables functions
#' 
#' @param ... 
#'
#' @return list of conditions
#' @export
#'
create_conditions <- function(...) {
  rlang:::enquos(...)
  
}

#' Connect to duckdb
#'
#' @param db_path path to duckdb
#'
#' @return db connection
#' @export
#'
connect_to_tables <- function(db_path) {
  con <- dbConnect(duckdb(
    dbdir = db_path
  ))
  
  con
  
}
