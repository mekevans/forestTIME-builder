state = Sys.getenv("STATE", unset = "RI") #use RI for testing because it is small

library(forestTIME.builder)
library(dplyr)

# Data Download
fia_download(states = state, keep_zip = FALSE)

# Data prep
data <-
  fia_load(states = state) |>
  fia_tidy()

# Expand to include all years between surveys and interpolate/extrapolate
data_interpolated <- data |> expand_data() |> interpolate_data()

# Adjust for mortality and estimate carbon.
# If any trees use the `MORTYR` variable, use both methods for adjusting for mortality
do_both <- any(!is.na(data$MORTYR))

if (do_both) {
  data_mortyr <-
    data_interpolated |>
    adjust_mortality(use_mortyr = TRUE) |>
    fia_estimate() |> 
    fia_split_composite_ids()
}

data_midpt <-
  data_interpolated |>
  adjust_mortality(use_mortyr = FALSE) |>
  fia_estimate() |> 
  fia_split_composite_ids()

# Write out to parquet
cli::cli_progress_step("Writing results")

fs::dir_create("fia/parquet")
if (do_both) {
  nanoparquet::write_parquet(
    data_mortyr,
    file = glue::glue("fia/parquet/{state}_mortyr.parquet")
  )
}

nanoparquet::write_parquet(
  data_midpt,
  glue::glue("fia/parquet/{state}_midpt.parquet")
)

#write to CSV
# fs::dir_create("fia/csv")
# if (do_both) {
#   readr::write_csv(
#     data_mortyr,
#     file = glue::glue("fia/csv/{state}_mortyr.CSV")
#   )
# }

# readr::write_csv(
#   data_midpt,
#   glue::glue("fia/csv/{state}_midpt.CSV")
# )
