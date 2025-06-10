test_that("inter_extra_polate() works", {
  y <- c(2, NA, 5, 6, NA, NA, NA)
  expect_equal(
    inter_extra_polate(x = seq_along(y), y = y),
    c(2, 3.5, 5, 6, 7, 8, 9)
  )

  expect_equal(
    inter_extra_polate(x = seq_along(y), y = y, extrapolate = FALSE),
    c(2, 3.5, 5, 6, NA, NA, NA)
  )
})

test_that("single numbers get carried forward", {
  y <- c(5, NA, NA, NA)
  expect_equal(
    inter_extra_polate(x = seq_along(y), y = y, extrapolate = TRUE),
    c(5, 5, 5, 5)
  )
})