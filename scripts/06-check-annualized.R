# This script checks that the annualized tree measurements table created through duckdb
# matches one created manually with dplyr.

library(duckdb)
library(DBI)
library(dplyr)

# Specify the path to .duckdb file for database
database_path <-
  here::here("data", "db", "foresttime-from-state-parquet.duckdb")

if (!file.exists(database_path)) {
  warning("Database file not found.")
}

# Connect to database
con <- dbConnect(duckdb(dbdir = database_path))

# Get annualized for first state

first_state <- tbl(con, "plot") |>
  select(STATECD) |>
  distinct() |>
  head(n = 1) |>
  collect()

first_state <- as.character(first_state$STATECD[1])

one_state_annual <- tbl(con, "tree_annualized") |>
  mutate(STATE = substr(TREE_COMPOSITE_ID, 1, 2)) |>
  mutate(STATE = stringr::str_replace(pattern = "_", replacement =  "", STATE)) |>
  filter(STATE == first_state) |>
  arrange(TREE_COMPOSITE_ID, YEAR) |>
  collect()

trees <- tbl(con, "tree") |>
  filter(STATECD == first_state) |>
  mutate(ACTUALHT = as.numeric(ACTUALHT)) |>
  left_join(tbl(con, "tree_info_composite_id")) |>
  filter(NRECORDS > 1) |>
  filter(!is.na(DIA), !is.na(HT), !is.na(ACTUALHT)) |>
  collect() |>
  select(TREE_COMPOSITE_ID, INVYR, DIA, HT, ACTUALHT, TREE_CN, PLT_CN, CONDID) |> 
  group_by(TREE_COMPOSITE_ID) |>
  mutate(NRECORDS_NONA = n(),
         next_INVYR = lead(INVYR, order_by = INVYR),
         next_DIA = lead(DIA, order_by = INVYR),
         next_HT = lead(HT, order_by = INVYR),
         next_ACTUALHT = lead(ACTUALHT, order_by = INVYR)) |>
  filter(NRECORDS_NONA > 1) |>
  mutate(next_INVYR = next_INVYR - 1) |>
  mutate(
    next_INVYR = ifelse(is.na(next_INVYR), INVYR, next_INVYR),
    next_DIA = ifelse(is.na(next_DIA), DIA, next_DIA),
    next_HT = ifelse(is.na(next_HT), HT, next_HT),
    next_ACTUALHT = ifelse(is.na(next_ACTUALHT), ACTUALHT, next_ACTUALHT)) |>
  mutate(DIA_slope = (next_DIA - DIA) / ((next_INVYR + 1) - INVYR),
         HT_slope = (next_HT - HT) / ((next_INVYR + 1) - INVYR),
         ACTUALHT_slope = (next_ACTUALHT - ACTUALHT) / ((next_INVYR + 1) - INVYR)) 

all_years <- tbl(con, "tree") |>
  filter(STATECD == first_state) |>
  select(TREE_COMPOSITE_ID) |>
  collect() |>
  distinct() |>
  cross_join(collect(tbl(con, "all_invyrs"))) |>
  arrange(TREE_COMPOSITE_ID, INVYR) |>
  rename(YEAR = INVYR) 

by <- join_by(TREE_COMPOSITE_ID, between(YEAR, INVYR, next_INVYR, bounds = "[]"))

trees_annual_measures <- all_years |> 
  inner_join(trees, by) |>
  mutate(time_run = YEAR - INVYR,
         DIA_start = DIA,
         HT_start = HT,
         ACTUALHT_start = ACTUALHT) |>
  mutate(DIA_est = DIA_start + (DIA_slope * time_run),
         HT_est = HT_start + (HT_slope * time_run),
         ACTUALHT_est = ACTUALHT_start + (ACTUALHT_slope * time_run)) |>
  arrange(TREE_COMPOSITE_ID, YEAR) |>
  select(TREE_COMPOSITE_ID, TREE_CN, PLT_CN, CONDID, YEAR, DIA_est, HT_est, ACTUALHT_est)

dbDisconnect(con, shutdown = TRUE)

if(any(!(isTRUE(all.equal(trees_annual_measures, select(one_state_annual, colnames(trees_annual_measures))))))) {
  warning("Annualized tables diverge")
}

