# This script takes the individual state .parquet files for each table
# stacks them into a single table
# and adds each table to a single database. 

library(duckdb)
library(DBI)

# Create a data/db directory if none exists:

if(!dir.exists(here::here("data", "db"))) {
  dir.create(here::here("data", "db"), recursive = T)
}


# Specify the path to use for the consolidated database:
database_path <-
  here::here("data", "db", "foresttime-from-state-parquet.duckdb")

# Connect to database
con <- dbConnect(duckdb(dbdir = database_path))

# Get the list of paths to the available parquet files.
# There are individual parquet files for each state for most of the tables.
# They all have the same path with the table name substituted out. 
# E.g. "plot" for the plot table.
# So you can just get the path to the plot table, and then swap out the table names.
parquet_files <- paste0(
  "data/parquet/",
  list.files(here::here("data", "parquet"), pattern = "plot")) |>
  paste(collapse = "', '")

# There is only one all_invyrs file.
all_invyrs_files <- paste0("data/parquet/", list.files(here::here("data", "parquet"), pattern = "all_invyrs")[1])

# Construct the queries to copy all rows from each set of state tables to the consolidated table

## The tree tables are partitioned within each state to keep the files small;
## all the other tables have just one file per state. 
tree_query <- paste0("CREATE TABLE tree AS SELECT * FROM read_parquet(['",
                     "data/parquet/tree_table_*.parquet/*/*",
                     "'], hive_partitioning = true)")
plot_query <- paste0("CREATE TABLE plot AS SELECT * FROM read_parquet(['",
                     parquet_files,
                     "'])")
cond_query <- gsub("plot", "cond", plot_query)
qa_flags_query <- gsub("plot", "qa_flags", plot_query)
tree_info_composite_id_query <- gsub("plot", "tree_info_composite_id", plot_query)
sapling_transitions_query <- gsub("plot", "sapling_transitions", plot_query)
tree_annualized_query <- gsub("plot", "tree_annualized", plot_query)
tree_cns_query <- gsub("plot", "tree_cns", plot_query)
all_invyrs_query <- paste0("CREATE TABLE all_invyrs AS SELECT * FROM read_parquet(['",
                           all_invyrs_files,
                           "'])") |>
  gsub(pattern = "plot", replacement = "all_invyrs")


# Run the queries
dbExecute(con,
          tree_query)
dbExecute(con,
          plot_query)
dbExecute(con,
          cond_query)
dbExecute(con,
          qa_flags_query)
dbExecute(con,
          tree_info_composite_id_query)
dbExecute(con,
          sapling_transitions_query)
dbExecute(con,
          tree_annualized_query)
dbExecute(con,
          tree_cns_query)
dbExecute(con,
          all_invyrs_query)

# Clean up
dbDisconnect(con, shutdown = TRUE)
