#' Import tree, plot, and condition tables from csvs
#'
#' @param con database connection
#' @param csv_dir directory with csv files
#' @param state which state(s) to pull data from. Defaults to "all" but can be a character vector of state abbreviations.
#'
#' @return nothing
#' @export
#'
#' @importFrom DBI dbSendQuery dbListTables
import_tables_from_csvs <- function(con, csv_dir, state = "all") {
  
  existing_tables <- dbListTables(con)
  
  if(any(c("tree", "plot", "cond") %in% existing_tables)) {
    message("Tree, plot, and/or cond tables already present in database!")
    return()
  }
  
  tree_files <- list.files(csv_dir, pattern = "_TREE.csv", full.names = FALSE)
  
  if(state != "all") {
  #  tree_states <- substr(tree_files, nchar(tree_files) - 10, nchar(tree_files) - 9)
  #  tree_files <- tree_files[ which(tree_states %in% state)]
    tree_files <- tree_files[ which(substr(tree_files, 1, 2) %in% state)]
  } 
  
  tree_files <- paste0(csv_dir, "/", tree_files)
  
  tree_files <- paste0(
    "['",
    paste(tree_files, collapse = "', '"),
    "']"
  )
  
  plot_files <- gsub("TREE", "PLOT", tree_files)
  cond_files <- gsub("TREE", "COND", tree_files)
  
  tree_query <-  paste0(
    "CREATE TABLE tree AS SELECT * FROM read_csv(",
    tree_files,
    ", header = true, ignore_errors=true) WHERE (INVYR >= 2000.0)")
  
  tree_name_query <- "ALTER TABLE tree RENAME COLUMN CN TO TREE_CN"
  tree_concat_query <- "ALTER TABLE tree ADD COLUMN PLOT_COMPOSITE_ID TEXT"
  tree_update_query <- "UPDATE tree SET PLOT_COMPOSITE_ID = CONCAT_WS('_', STATECD, UNITCD, COUNTYCD, PLOT)"
  tree_concat_query2 <- "ALTER TABLE tree ADD COLUMN TREE_COMPOSITE_ID TEXT"
  tree_update_query2 <- "UPDATE tree SET TREE_COMPOSITE_ID = CONCAT_WS('_', STATECD, UNITCD, COUNTYCD, PLOT, SUBP, TREE)"
  
  plot_query <-  paste0(
    "CREATE TABLE plot AS SELECT * FROM read_csv(",
    plot_files,
    ", types = {'ECO_UNIT_PNW': 'VARCHAR'}, ignore_errors=true, header = true) WHERE (INVYR >= 2000.0)")
  
  plot_name_query <- "ALTER TABLE plot RENAME COLUMN CN TO PLT_CN"
  plot_concat_query <- "ALTER TABLE plot ADD COLUMN PLOT_COMPOSITE_ID TEXT"
  plot_update_query <- "UPDATE plot SET PLOT_COMPOSITE_ID = CONCAT_WS('_', STATECD, UNITCD, COUNTYCD, PLOT)"
  
  
  cond_query <- paste0(
    "CREATE TABLE cond AS SELECT * FROM read_csv(",
    cond_files,
    ", header = true, ignore_errors = true, types = {'HABTYPCD1': 'VARCHAR', 'HABTYPCD2': 'VARCHAR'}) WHERE (INVYR >= 2000.0)")
  
  cond_name_query <- "ALTER TABLE cond RENAME COLUMN CN TO COND_CN"
  cond_concat_query <- "ALTER TABLE cond ADD COLUMN PLOT_COMPOSITE_ID TEXT"
  cond_update_query <- "UPDATE cond SET PLOT_COMPOSITE_ID = CONCAT_WS('_', STATECD, UNITCD, COUNTYCD, PLOT)"
  
  sp_query <- "CREATE table ref_species AS SELECT * FROM read_csv('data/rawdat/REF_SPECIES.csv')"
  decay_query <- "CREATE table ref_tree_decay_prop AS SELECT * FROM read_csv('data/rawdat/REF_TREE_DECAY_PROP.csv')"
  
  dbExecute(con, tree_query)
  dbExecute(con, tree_name_query)
  dbExecute(con, tree_concat_query)
  dbExecute(con, tree_update_query)
  dbExecute(con, tree_concat_query2)
  dbExecute(con, tree_update_query2)
  dbExecute(con, plot_query)
  dbExecute(con, plot_name_query)
  dbExecute(con, plot_concat_query)
  dbExecute(con, plot_update_query)
  dbExecute(con, cond_query)  
  dbExecute(con, cond_name_query)
  dbExecute(con, cond_concat_query)
  dbExecute(con, cond_update_query)
  dbExecute(con, sp_query)  
  dbExecute(con, decay_query)  
  
  
  return()
  
}
