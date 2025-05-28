test_that("variables with NAs get interpolated correctly", {
  db <- read_fia(
    "DE",
    dir = system.file("exdata", package = "forestTIME.builder")
  )
  data <- prep_data(db) |> 
    dplyr::filter(tree_ID == "10_1_1_104_1_28") |>
    dplyr::select(plot_ID, tree_ID, INVYR, DIA, HT, STATUSCD, STANDING_DEAD_CD, DECAYCD, DESIGNCD)
  data_interpolated <- data |> 
    expand_data() |> 
    interpolate_data() 

  expect_equal(data_interpolated |> filter(YEAR %in% 2017:2020) |> pull(STANDING_DEAD_CD), rep(0, 4))
})
