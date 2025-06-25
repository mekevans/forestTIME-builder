library(dplyr)
test_that("estimates match those in raw data", {
  db <- fia_load(
    "DE",
    dir = system.file("exdata", package = "forestTIME.builder")
  )
  orig <- db$TREE |>
    dplyr::filter(INVYR >= 2000L) |>
    add_composite_ids() |>
    select(
      tree_ID,
      INVYR,
      # DIA,
      # HT,
      # ACTUALHT,
      # CR,
      # CULL,
      # STATUSCD,
      # STANDING_DEAD_CD,
      # DECAYCD,
      # RECONCILECD,
      TPA_UNADJ,
      CARBON_AG,
      DRYBIO_AG
    )
  data_prepped <- prep_data(db) |>
    dplyr::filter(INVYR >= 2000L) |>
    rename(YEAR = INVYR) |>
    prep_carbon() |>
    # add back TPA_UNADJ from raw data because we are skipping interpolation steps
    left_join(orig |> select(tree_ID, YEAR = INVYR, TPA_UNADJ) |> distinct())

  data_carbon <- data_prepped |>
    estimate_carbon() |>
    select(
      tree_ID,
      YEAR,
      CARBON_AG_est = CARBON_AG,
      DRYBIO_AG_est = DRYBIO_AG
    )
  # add the original estimates of carbon and biomass to the prepped data, then
  # add the outputs of estimate_carbon()
  test <- left_join(
    data_prepped |> filter(!is.na(tree_ID)), #ignore empty plots
    orig |>
      select(
        tree_ID,
        YEAR = INVYR,
        CARBON_AG_orig = CARBON_AG,
        DRYBIO_AG_orig = DRYBIO_AG
      )
  ) |>
    left_join(data_carbon |> filter(!is.na(tree_ID))) #ignore empty plots
    
  expect_equal(test$CARBON_AG_est, test$CARBON_AG_orig, tolerance = 1e-3)
  expect_equal(test$DRYBIO_AG_est, test$DRYBIO_AG_orig, tolerance = 1e-3)
})
