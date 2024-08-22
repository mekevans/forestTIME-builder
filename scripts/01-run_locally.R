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

download_csv_from_datamart(all_states$STATE, here::here("data", "rawdat", "state"), overwrite = F)

# Create state-by-state databases and parquet files

state_scripts <- list.files(here::here("scripts",
                                       "01-state-by-state"),
                            pattern = "-state-parquet.R",
                            full.names = T) 


purrr::map(state_scripts[3:50], source, .progress = T)

# Combine state files into a single database

source(here::here("scripts", "02-create_db_from_parquet.R"))

# Upload database to Zenodo
source(here::here("scripts", "03-upload_parquet_db_zenodo.R"))

# Run checks on the database
source(here::here("scripts", "04-check_db.R"))
