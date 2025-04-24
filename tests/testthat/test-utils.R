test_that("%|||% works as expected", {
  expect_equal(character() %|||% "hi", "hi")
  expect_equal(NULL %|||% "hi", "hi")
  expect_equal(5 %|||% 10, 5)
})
