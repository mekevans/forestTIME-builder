# This script uploads the consolidated forestTIME database to a (private) Zenodo record.

library(zen4R)

# Specify the path to .duckdb file for database
database_path <-
  here::here("data", "db", "foresttime-from-state-parquet.duckdb")

# Authenticate to Zenodo using token from environment variable
zenodo <- ZenodoManager$new(token = Sys.getenv("ZENODO_TOKEN"), logger = "INFO")

# Connect to an existing record (this one is private, owned by Renata)
# and update it with a new version with the new database file.
myrec <- zenodo$getDepositionByConceptId("13312345")
myrec <- zenodo$depositRecordVersion(
  myrec,
  delete_latest_files = TRUE,
  files = database_path,
  publish = TRUE
)
