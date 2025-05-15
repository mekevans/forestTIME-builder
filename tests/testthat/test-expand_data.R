test_that("expand_data() works", {
  # fmt: skip
  df <- dplyr::tribble( 
    ~plot_ID, ~tree_ID, ~INVYR, ~HT, ~DIA, ~SPCD,
    "p1", "A", 2000, 12, 24, 123,
    "p1", "A", 2005, 18, 26, 123,
    "p1", "B", 2000, 5, 10, 222,
    "p1", "B", 2005, 7, 11, 222
  )

  df_expanded <- expand_data(df)
  expect_snapshot(df_expanded) #TODO comment out if this gets annoying
  expect_gt(nrow(df_expanded), nrow(df))

  expect_true(all(!is.na(df_expanded$plot_ID)))
  expect_true(all(!is.na(df_expanded$SPCD)))
  expect_identical(
    unique(df_expanded$YEAR),
    seq(min(df$INVYR), max(df$INVYR), by = 1)
  )
})
