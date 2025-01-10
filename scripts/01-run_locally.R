# This is a script for running all of the scripts that download + process state data
# into state-level forestTIME databases locally.
library(dplyr)

delete_files <- FALSE

# Download state level files 

source(here::here("R", "download_csv_wrapper.R"))

all_states <- read.csv(here::here("data", "fips.csv")) |>
  filter(STATEFP < 60) |>
  select(STATE) |>
  filter(STATE != "DC")

# Download data for each sate and create state-by-state databases and parquet files
purrr::walk(all_states, \(state) {
  withr::with_envvar(new = c("STATE" = state), source("scripts/state-parquet.R"))
}, .progress = TRUE)

# Combine state files into a single database
source(here::here("scripts", "02-create_db_from_parquet.R"))

# Upload database to Zenodo
source(here::here("scripts", "03-upload_parquet_db_zenodo.R"))

# Run checks on the database
source(here::here("scripts", "04-check_db.R"))
