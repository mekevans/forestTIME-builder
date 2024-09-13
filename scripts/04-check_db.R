# This script performs some checks on the consolidated forestTIME database.
# In an automated workflow, it's good to run it after uploading the database
# because if it fails and the workflow crashes, you will lose the database
# unless it's already been stored somewhere. 

library(duckdb)
library(DBI)
library(dplyr)
source(here::here("R", "create_all_tables.R"))

# Specify the path to .duckdb file for database
database_path <-
  here::here("data", "db", "foresttime-from-state-parquet.duckdb")

if (!file.exists(database_path)) {
  warning("Database file not found.")
}

# Connect to database
con <- dbConnect(duckdb(dbdir = database_path))

# Check that the db contains the right tables

expected_table_names <-
  c(
    'all_invyrs',
    'cond',
    'nsvb_vars',
    'plot',
    'qa_flags',
    'ref_species',
    'ref_tree_carbon_ratio_dead',
    'ref_tree_decay_prop',
    'sapling_transitions',
    'tree',
    'tree_annualized',
    'tree_carbon',
    'tree_cns',
    'tree_info_composite_id'
  )

if (!(all(dbListTables(con) == expected_table_names))) {
  warning("Table names do not match expected table names.")
}

# Check that sapling transitions sum to 1 
# in all years where there are >0 saplings in the previous year
# If there are 0 saplings in the previous year there is no
# logical denominator for calculating transition proportions
# and all proportion rows are NA. 

saplings <- tbl(con, "sapling_transitions") |>
  collect()

expected_rowsums <- ifelse(saplings$PREV_live_and_skipped > 0, 1, NA)

if(all.equal(rowSums(saplings[,7:13]), expected_rowsums) != TRUE) {
  warning("Sapling proportions do not sum to 1")
}

# Check that the sapling INVYRs are in the correct order
# duckdb routinely gives a warning about "ORDER_BY being ignored" for
# operations that involve lag/lead functions. 
# I have checked and the ORDER_BY is NOT ignored, and the lag/lead are working -
# but if they ever don't work, this check should start failing. 
if(!all(saplings$PREV_INVYR < saplings$INVYR)) {
  warning("Sapling years out of order")
}


# Just give a warning to ensure that the warning output is printing. 
warning("Here is just a warning")

# Clean up
dbDisconnect(con, shutdown = TRUE)
