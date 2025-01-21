#this helps to read in CSV files correctly by creating named character vectors that can be passed to the col.types argument of duckdb_read_csv(). You'll need to get these column types from one of the SQLite databases from datamart.

library(DBI)
library(dplyr)
#download any SQLite database from FIADB datamart
con <- 
  dbConnect(RSQLite::SQLite(), dbname = "~/Downloads/SQLite_FIADB_RI.db")

con_duckdb <- dbConnect(duckdb::duckdb())

#for duckdb, what are the column types?
tree_types <- dbDataType(con_duckdb, collect(head(tbl(con, "TREE")))) 
plot_types <- dbDataType(con_duckdb, collect(head(tbl(con, "PLOT"))))
cond_types <- dbDataType(con_duckdb, collect(head(tbl(con, "COND"))))
plotgeom_types <- dbDataType(con_duckdb, collect(head(tbl(con, "PLOTGEOM"))))

table_types <- list (
  tree_types = tree_types,
  plot_types = plot_types,
  cond_types = cond_types,
  plotgeom_types = plotgeom_types
)

saveRDS(table_types, "data/rawdat/table_types.rds")
