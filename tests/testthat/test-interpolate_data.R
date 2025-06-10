library(dplyr)
test_that("variables with NAs get interpolated correctly", {
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
      STATUSCD,
      STANDING_DEAD_CD,
      DECAYCD,
      DESIGNCD
    )
  data_interpolated <- data |>
    expand_data() |>
    interpolate_data()

  expect_equal(
    data_interpolated |> filter(YEAR %in% 2017:2020) |> pull(STANDING_DEAD_CD),
    rep(0, 4)
  )
})

test_that("interpolation flags negative numbers as fallen dead", {
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
      DIA,
      HT,
      ACTUALHT,
      CULL,
      CR,
      STATUSCD,
      STANDING_DEAD_CD,
      DECAYCD,
      DESIGNCD
    )
  data_interpolated <- data |>
    expand_data() |>
    interpolate_data()

  expect_equal(
    data_interpolated |>
      filter(ACTUALHT < 4.5) |>
      pull(STANDING_DEAD_CD) |>
      unique(),
    0
  )
  expect_equal(
    data_interpolated |> filter(ACTUALHT < 4.5) |> pull(STATUSCD) |> unique(),
    2
  )
})

test_that("interpolation of CULL is correct", {
  data <- read_fia(
    "DE",
    dir = system.file("exdata", package = "forestTIME.builder")
  ) |>
    prep_data() |>
    dplyr::filter(tree_ID == "10_1_1_128_1_24")

  data_interpolated <- data |>
    expand_data() |>
    interpolate_data()

  expect_true(
    data_interpolated |>
      filter(DIA < 5) |>
      pull(CULL) |>
      is.na() |>
      all()
  )

  cull_vals <- data_interpolated |>
    filter(DIA >= 5) |>
    pull(CULL) |>
    unique()
  expect_false(
    identical(cull_vals, c(0, 1))
  )
  
})
