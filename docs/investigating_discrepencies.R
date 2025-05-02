db <- read_fia(
  "DE",
  dir = system.file("exdata", package = "forestTIME.builder")
)

#get CARBON_AG and DRYBIO_AG values from raw data
orig <- db$TREE |>
  add_composite_ids() |>
  select(tree_ID, INVYR, TPA_UNADJ, CARBON_AG, DRYBIO_AG)

#Prep data and do carbon estimation (skip interpolation)
data_prepped <- prep_data(db) |>
  rename(YEAR = INVYR) |>
  prep_carbon() |>
  #add back TPA_UNADJ from raw data because we are skipping interpolation steps
  left_join(orig |> select(tree_ID, YEAR = INVYR, TPA_UNADJ) |> distinct())

data <- data_prepped |>
  estimate_carbon() |>
  select(
    tree_ID,
    INVYR = YEAR,
    CARBON_AG_est = CARBON_AG,
    DRYBIO_AG_est = DRYBIO_AG
  )

# Combine the two sets of carbon and biomass columns
test <- left_join(
  orig |> select(-TPA_UNADJ),
  data
)

#which observations are NA from estimate_carbon() but present in the original data?

problems <- test |>
  filter(is.na(CARBON_AG_est) & !is.na(CARBON_AG)) |>
  select(tree_ID, INVYR)

#In this case, all the NAs are just observations that were dropped from the prepped data for some reason
semi_join(data_prepped, problems, by = join_by(tree_ID, YEAR == INVYR))

#notable reasons we drop trees:
# - SPCD changed
# - only one non-NA observation (can't extrapolate to midpt, so just removed)

#this has a pretty big effect on total carbon, just about halving it

test |>
  summarize(across(
    c(CARBON_AG, DRYBIO_AG, CARBON_AG_est, DRYBIO_AG_est),
    \(x) sum(x, na.rm = TRUE)
  )) |>
  mutate(
    CARBON_AG_diff = CARBON_AG - CARBON_AG_est,
    DRYBIO_AG_diff = DRYBIO_AG - DRYBIO_AG_est
  )
