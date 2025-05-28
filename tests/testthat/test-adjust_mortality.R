library(dplyr)
test_that("fallen dead trees get NAs correctly", {
  db <- read_fia(
    "DE",
    dir = system.file("exdata", package = "forestTIME.builder")
  )
  data <- prep_data(db) |>
    dplyr::filter(tree_ID == "10_1_1_104_1_28") |>
    dplyr::select(
      plot_ID,
      tree_ID,
      INVYR,
      DIA,
      HT,
      ACTUALHT,
      CULL,
      CR,
      MORTYR,
      STATUSCD,
      STANDING_DEAD_CD,
      DECAYCD,
      DESIGNCD,
      RECONCILECD
    )
  data_interpolated <- data |>
    expand_data() |>
    interpolate_data() |>
    adjust_mortality(use_mortyr = FALSE)

  expect_true(all(is.na(
    data_interpolated |>
      filter(YEAR %in% 2017:2020) |>
      select(DIA, HT, ACTUALHT, CULL, CR)
  )))
})

test_that("trees moving to non-sampled conditions have NAs", {
  db <- read_fia(
    "DE",
    dir = system.file("exdata", package = "forestTIME.builder")
  )
  data <- prep_data(db) |>
    dplyr::filter(tree_ID == "10_1_1_22_4_3") |>
    dplyr::select(
      plot_ID,
      tree_ID,
      INVYR,
      DIA,
      HT,
      ACTUALHT,
      CULL,
      CR,
      MORTYR,
      STATUSCD,
      STANDING_DEAD_CD,
      DECAYCD,
      DESIGNCD,
      RECONCILECD
    )
  data_interpolated <- data |>
    expand_data() |>
    interpolate_data() |>
    adjust_mortality(use_mortyr = FALSE)
  expect_equal(max(data_interpolated$YEAR), 2018)
  expect_true(all(is.na(
    data_interpolated |>
      filter(YEAR >= 2015) |>
      select(DIA, HT, ACTUALHT, CR, CULL)
  )))
})
