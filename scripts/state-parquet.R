state = Sys.getenv("STATE", unset = "RI") #use RI for testing because it is small
#TODO set delete downloads arg of create_all_tables based on whether local or on GH Actions?

library(rFIA)
library(readr)
library(here)
library(purrr)
library(dplyr)
library(fs)
library(nanoparquet)
library(glue)
library(cli)

#load all functions needed
fs::dir_ls("R/") |> walk(source)

# Data Download
cli_progress_step("Downloading FIA data for {state}")
get_fia_tables(states = state, keep_zip = FALSE)

# Data prep
cli_progress_step("Wrangling data")
data <-
  read_fia(states = state) |>
  prep_data()

# Check that each tree only has 1 entry per year
n <- data |>
  group_by(tree_ID, INVYR) |>
  summarise(n = n(), .groups = "drop") |>
  filter(n > 1) |>
  nrow()
stopifnot(n == 0)

# Expand to include all years between surveys and interpolate/extrapolate
cli_progress_step("Interpolating between surveys")
data_interpolated <- data |> expand_data() |> interpolate_data()

# Adjust for mortality and estimate carbon.
cli_progress_step("Adjusting for mortality and estimating carbon")
# If any trees use the `MORTYR` variable, use both methods for adjusting for mortality
do_both <- any(!is.na(data$MORTYR))

if (do_both) {
  data_mortyr <-
    data_interpolated |>
    adjust_mortality(use_mortyr = TRUE) |>
    prep_carbon() |>
    estimate_carbon()
}

data_midpt <-
  data_interpolated |>
  adjust_mortality(use_mortyr = FALSE) |>
  prep_carbon() |>
  estimate_carbon()

# TODO: pop scaling???

# Write out to parquet
cli_progress_step("Writing results")

fs::dir_create("data/parquet")
if (do_both) {
  nanoparquet::write_parquet(
    data_mortyr,
    file = here(glue("data/parquet/{state}_mortyr.parquet"))
  )
}

nanoparquet::write_parquet(
  data_midpt,
  here(glue("data/parquet/{state}_midpt.parquet"))
)
