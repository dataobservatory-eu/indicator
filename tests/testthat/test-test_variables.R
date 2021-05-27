
test_that("time column", {
  expect_true(is_time(as.integer(1:2)))
  expect_true(is_time(c(2011,2012)))
  expect_false(is_time(c(2021,2022.5)))
  expect_true(is_time(as.Date("2021-12-12")))
  expect_false(is_time(rep("x", 3)))
  expect_false(is_time(list("a")))
})
