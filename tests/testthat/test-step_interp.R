test_that("step_interp() works", {
  x <- c(NA, NA, "A", NA, NA, NA, "B", NA, NA, NA, NA, "C", NA, NA)
  expect_equal(
    step_interp(x),
    c(NA, NA, "A", "A", "B", "B", "B", "B", "B", "C", "C", "C", "C", "C")
  )
})
