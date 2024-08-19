# This script sets up the folder structure for foresTIME.
# It is intended to be run once per computer.

# Set up folder structure ####

if(!dir.exists(here::here("data", "db"))) {
  dir.create(here::here("data", "db"), recursive = T)
}


if(!dir.exists(here::here("data", "parquet"))) {
  dir.create(here::here("data", "parquet"), recursive = T)
}

