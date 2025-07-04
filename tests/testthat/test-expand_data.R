library(dplyr)
test_that("expand_data() works", {
  # fmt: skip
  df <- dplyr::tribble( 
    ~plot_ID, ~tree_ID, ~INVYR, ~HT, ~DIA, ~CULL, ~SPCD,
    "p1", "A", 2000, 12, 24, 0, 123,
    "p1", "A", 2005, 18, 26, 0, 123,
    "p1", "B", 2000, 5, 10, 0, 222,
    "p1", "B", 2005, 7, 11, 0, 222
  )

  df_expanded <- expand_data(df)
  expect_gt(nrow(df_expanded), nrow(df))

  expect_true(all(!is.na(df_expanded$plot_ID)))
  expect_true(all(!is.na(df_expanded$SPCD)))
  expect_identical(
    unique(df_expanded$YEAR),
    seq(min(df$INVYR), max(df$INVYR), by = 1)
  )
})

test_that("interpolation flag works", {
  # fmt: skip
  df <- dplyr::tribble( 
      ~plot_ID, ~tree_ID, ~INVYR, ~HT, ~DIA,~CULL, ~SPCD,
      "p1", "A", 2000, 12, 24, 0, 123,
      "p1", "A", 2005, NA, NA, NA, NA,
      "p1", "A", 2010, 5, 10, 0, 222,
      "p1", "A", 2015, 7, 11, 0, 222
    )
  df_expanded <- expand_data(df)

  expect_equal(nrow(df_expanded |> filter(interpolated == FALSE)), nrow(df))
  expect_equal(
    nrow(df_expanded |> filter(interpolated == TRUE)),
    length(seq(min(df$INVYR), max(df$INVYR))) - nrow(df)
  )
})
