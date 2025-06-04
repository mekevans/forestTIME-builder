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
      SPCD,
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
      COND_STATUS_CD,
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
      SPCD,
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
      COND_STATUS_CD,
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

test_that("method doesn't matter for DE", {
  db <- read_fia(
    "DE",
    dir = system.file("exdata", package = "forestTIME.builder")
  )
  data <- prep_data(db) |>
    dplyr::filter(tree_ID == "10_1_1_104_1_28") |>
    dplyr::select(
      plot_ID,
      tree_ID,
      SPCD,
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
      COND_STATUS_CD,
      RECONCILECD
    )
  data_interpolated <- data |>
    expand_data() |>
    interpolate_data() 
  
  data_midpt <- data_interpolated |>
    adjust_mortality(use_mortyr = FALSE)

  data_mortyr <- data_interpolated |>
    adjust_mortality(use_mortyr = TRUE)

  expect_equal(data_midpt, data_mortyr)
})

test_that("No values below thresholds for measurement", {
  db <- read_fia(
    "DE",
    dir = system.file("exdata", package = "forestTIME.builder")
  )
  data <- prep_data(db) |>
    dplyr::filter(tree_ID %in% c("10_1_1_104_3_4", "10_1_1_148_4_2")) |>
    dplyr::select(
      plot_ID,
      tree_ID,
      SPCD,
      INVYR,
      MORTYR,
      DIA,
      HT,
      ACTUALHT,
      CULL,
      CR,
      STATUSCD,
      STANDING_DEAD_CD,
      DECAYCD,
      RECONCILECD,
      COND_STATUS_CD,
      DESIGNCD
    )
  data_adj <- data |>
    expand_data() |>
    interpolate_data() |> 
    adjust_mortality()

  expect_equal(
    nrow(data_adj |> filter(ACTUALHT < 4.5)),
    0
  )
})