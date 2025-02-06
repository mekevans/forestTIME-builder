library(duckdb)
library(dplyr)
library(fs)
#' Import tree, plot, and condition tables from csvs
#'
#' @param con database connection
#' @param csv_dir directory with csv files
#' @param state which state(s) to pull data from. Defaults to "all" but can be a
#'   character vector of state abbreviations.
#' @param column_types path to an .rds file created by
#'   scripts/create_column_types.R. This contains a list object with named
#'   character vectors of the data types of each column in the CSV files read
#'   in.
#' @return nothing
#' @export
#'
import_tables_from_csvs <- function(con, csv_dir, state = "all", column_types = "data/rawdat/table_types.rds") {
  types <- readRDS(column_types)
  tree_files <- fs::dir_ls(csv_dir, regexp = "_TREE.csv")
  if(state != "all") {
    tree_files <- tree_files[which(substr(fs::path_file(tree_files), 1, 2) %in% state)]
  } 
  plot_files <- gsub("TREE", "PLOT", tree_files)
  plotgeom_files <- gsub("TREE", "PLOTGEOM", tree_files)
  cond_files <- gsub("TREE", "COND", tree_files)
  
  ## Tree Table #####
  duckdb_read_csv(
    con,
    files = tree_files,
    name = "tree_raw",
    col.types = types$tree_types,
    temporary = TRUE
  )
  tree_raw <- tbl(con, "tree_raw")
  tree <- 
    tree_raw |> 
    filter(INVYR >= 2000.0) |> 
    rename(TREE_CN = CN) |> 
    mutate(
      PLOT_COMPOSITE_ID = paste(STATECD, UNITCD, COUNTYCD, PLOT, sep = "_"),
      TREE_COMPOSITE_ID = paste(STATECD, UNITCD, COUNTYCD, PLOT, SUBP, TREE, sep = "_"),
      .before = 1
    )
  copy_to(con, tree, "tree", temporary = FALSE)
  
  ## Plot Table ####
  duckdb_read_csv(
    con,
    files = plot_files,
    name = "plot_raw",
    col.types = types$plot_types,
    temporary = TRUE
  )
  duckdb_read_csv(
    con,
    files = plotgeom_files,
    name = "plotgeom_raw",
    col.types = types$plotgeom_types,
    temporary = TRUE
  )
  plot_raw <- tbl(con, "plot_raw")
  plotgeom_raw <- tbl(con, "plotgeom_raw")
  
  plot <- 
    # add ECOSUBCD column that was moved to PLOTGEOM in newer FIADB versions
    left_join(plot_raw,
              plotgeom_raw |> select(CN, INVYR, ECOSUBCD),
              by = join_by(CN, INVYR)) |> 
    filter(INVYR >= 2000.0) |> 
    rename(PLT_CN = CN) |> 
    mutate(PLOT_COMPOSITE_ID = paste(STATECD, UNITCD, COUNTYCD, PLOT, sep = "_"), .before = 1)
  copy_to(con, plot, name = "plot", temporary = FALSE)
  
  
  ## Cond Table ####
  duckdb_read_csv(
    con,
    files = cond_files,
    name = "cond_raw",
    col.types = types$cond_types,
    temporary = TRUE
  )
  cond_raw <- tbl(con, "cond_raw")
  
  cond <- 
    cond_raw |> 
    filter(INVYR >= 2000.0) |> 
    rename(COND_CN = CN) |> 
    mutate(PLOT_COMPOSITE_ID = paste(STATECD, UNITCD, COUNTYCD, PLOT, sep = "_"), .before = 1)
  copy_to(con, cond, "cond", temporary = FALSE)
  
  
  ## Ref Tables ####
  duckdb_read_csv(
    con,
    files = 'data/rawdat/REF_SPECIES.csv',
    name = "ref_species",
    nrow.check = 2000
  )
  
  duckdb_read_csv(
    con,
    files = 'data/rawdat/REF_TREE_DECAY_PROP.csv',
    name = "ref_tree_decay_prop",
    nrow.check = 2000
  )
  
  duckdb_read_csv(
    con,
    files = 'data/rawdat/REF_TREE_CARBON_RATIO_DEAD.csv',
    name = "ref_tree_carbon_ratio_dead",
    nrow.check = 2000
  )
  return()
}