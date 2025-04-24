state = Sys.getenv("STATE", unset = "RI") #use RI for testing because it is small
#TODO set delete downloads arg of create_all_tables based on whether local or on GH Actions?

# library(rFIA)
# library(readr)
# library(here)
# library(purrr)
# library(dplyr)
# library(fs)
# library(nanoparquet)
# library(glue)
# library(cli)

#load all functions needed
# fs::dir_ls("R/") |> walk(source)
load_all()

# Data Download
get_fia_tables(states = state, keep_zip = FALSE)

# Data prep
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
data_interpolated <- data |> expand_data() |> interpolate_data()

# Adjust for mortality and estimate carbon.
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
cli::cli_progress_step("Writing results")

# fs::dir_create("fia/parquet")
# if (do_both) {
#   nanoparquet::write_parquet(
#     data_mortyr,
#     file = here::here(glue::glue("fia/parquet/{state}_mortyr.parquet"))
#   )
# }

# nanoparquet::write_parquet(
#   data_midpt,
#   here::here(glue::glue("fia/parquet/{state}_midpt.parquet"))
# )

#write to CSV
fs::dir_create("fia/out")
if (do_both) {
  readr::write_csv(
    data_mortyr |>
      split_composite_ids(),
    file = here::here(glue::glue("fia/out/{state}_mortyr.CSV"))
  )
}

readr::write_csv(
  data_midpt |>
    split_composite_ids(),
  here::here(glue::glue("fia/out/{state}_midpt.CSV"))
)
