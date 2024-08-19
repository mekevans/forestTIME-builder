#' Query annualized estimates of measurements
#'
#' Pulls timeseries of *annual* measurements interpolated for trees. 
#'
#' @param con database connection
#' @param conditions use `create_conditions` to create a list of conditions involving columns from the tree, plot, cond, qa_flags, or tree_info_composite_id tables
#' @param variables character vector of variables to return
#'
#' @return data frame of annual measurements and additional varaibles estimated for trees satisfying conditions 
#' @export
#'
query_annualized <- function(con,
                               conditions = create_conditions(...),
                               variables = c("DIA")) {
  # Connect to tables
  
  trees_annualized <- tbl(con, "tree_annualized")
  
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
    'YEAR',
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
    left_join(trees_annualized) |>
    filter(!is.na(YEAR)) |> 
    left_join(qa_flags) |>
    left_join(plots) |>
    left_join(cond) |>
    filter(!!!tree_filters) |>
    select(all_of(all_variables)) |>
    arrange(TREE_COMPOSITE_ID, YEAR) |>
    collect()
  
  tree_timeseries
}