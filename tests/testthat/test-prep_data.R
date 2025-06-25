library(dplyr)
test_that("fia_tidy() works", {
  fia_dir <- system.file("exdata", package = "forestTIME.builder")
  # fia_download(states = "RI", download_dir = fia_dir, keep_zip = TRUE)
  db <- fia_load(states = "DE", dir = fia_dir)
  data <- fia_tidy(db)

  expect_s3_class(data, "data.frame")
  expect_true("tree_ID" %in% colnames(data))
  expect_true("plot_ID" %in% colnames(data))

  #check that ACTUALHT gets filled in with HT when NA
  ht_check <- data |> dplyr::filter(!is.na(HT) & is.na(ACTUALHT))
  expect_equal(nrow(ht_check), 0)

  #check that SPCD is consistent
  spcd_check <- data |> group_by(tree_ID) |> filter(length(unique(SPCD)) > 1)
  expect_equal(nrow(spcd_check), 0)
})


test_that("composite ID helpers work", {
  df <- dplyr::tibble(
    STATECD = 1,
    UNITCD = 2,
    COUNTYCD = 3,
    PLOT = 4,
    SUBP = 5,
    TREE = 6
  )
  df2 <- add_composite_ids(df)

  expect_true("tree_ID" %in% colnames(df2))
  expect_true("plot_ID" %in% colnames(df2))

  expect_equal(df2$plot_ID, "1_2_3_4")
  expect_equal(df2$tree_ID, "1_2_3_4_5_6")

  df3 <- df2 |> select(tree_ID, plot_ID)

  df4 <- split_composite_ids(df3)

  expect_equal(
    c(
      "STATECD",
      "UNITCD",
      "COUNTYCD",
      "PLOT",
      "SUBP",
      "TREE",
      "tree_ID",
      "plot_ID"
    ),
    colnames(df4)
  )

  df5 <- df3 |> select(-tree_ID) |> split_composite_ids()

  expect_equal(
    c(
      "STATECD",
      "UNITCD",
      "COUNTYCD",
      "PLOT",
      "plot_ID"
    ),
    colnames(df5)
  )
})
