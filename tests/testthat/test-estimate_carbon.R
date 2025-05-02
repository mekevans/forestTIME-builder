library(dplyr)
test_that("estimates match those in raw data", {
  db <- read_fia(
    "DE",
    dir = system.file("exdata", package = "forestTIME.builder")
  )
  orig <- db$TREE |>
    add_composite_ids() |>
    select(tree_ID, INVYR, TPA_UNADJ, CARBON_AG, DRYBIO_AG)
  data <- prep_data(db) |>
    rename(YEAR = INVYR) |>
    prep_carbon() |>
    #add back TPA_UNADJ from raw data because we are skipping interpolation steps
    left_join(orig |> select(tree_ID, YEAR = INVYR, TPA_UNADJ) |> distinct()) |>
    estimate_carbon() |>
    select(
      tree_ID,
      INVYR = YEAR,
      CARBON_AG_est = CARBON_AG,
      DRYBIO_AG_est = DRYBIO_AG
    )

  test <- left_join(
    orig |> select(-TPA_UNADJ),
    data
  )

  #without NAs
  test_no_nas <- test |> filter(!is.na(CARBON_AG_est) & !is.na(DRYBIO_AG_est))
  expect_equal(
    test_no_nas$CARBON_AG,
    test_no_nas$CARBON_AG_est,
    tolerance = 1e-4
  )
  expect_equal(
    test_no_nas$DRYBIO_AG,
    test_no_nas$DRYBIO_AG_est,
    tolerance = 1e-4
  )

  #with NAs
  expect_equal(test$CARBON_AG, test$CARBON_AG_est, tolerance = 1e-4)
  expect_equal(test$DRYBIO_AG, test$DRYBIO_AG_est, tolerance = 1e-4)
})
