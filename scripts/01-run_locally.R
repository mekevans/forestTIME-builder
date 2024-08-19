# This is a script for running all of the scripts that download + process state data
# into state-level forestTIME databases locally.
# It takes a LONG time to run because you have to download every state's data.

delete_files <- FALSE

state_scripts <- list.files(here::here("scripts",
                                       "01-state-by-state"),
                            pattern = "-state-parquet.R",
                            full.names = T) 
# 
# for(i in 17:50) {
#   source(state_scripts[i])
# }

purrr::map(state_scripts[17:50], source, .progress = T)

source(here::here("scripts", "02-create_db_from_parquet.R"))
source(here::here("scripts", "03-upload_parquet_db_zenodo.R"))
source(here::here("scripts", "04-check_db.R"))
