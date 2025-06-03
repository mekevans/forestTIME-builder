test_that("prep_carbon() keeps empty plots", {
  data <- read_fia(
    "DE",
    dir = system.file("exdata", package = "forestTIME.builder")
  ) |> prep_data()

  expect_gte(nrow(prep_carbon(data) |> dplyr::filter(is.na(tree_ID))), 1)
})
