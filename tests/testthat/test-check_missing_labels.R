

df_with_missings <- data.frame(
  time = rep(c(as.Date ("2019-01-01"), as.Date ("2020-01-01")),2),
  geo = c("NL", "BE"),
  value = c(10, NA_real_, 12,13),
  estimate = c("actual", "missing", "actual", "actual")
)

df_with_missings$method <- df_with_missings$estimate

test_that("Assertions are correctly made", {
  expect_true(check_missing_labels(check_indicator = df_with_missings))
})

df_with_incorrect_missings <- df_with_missings
df_with_incorrect_missings$method <- rep("actual", 4)

test_that("multiplication works", {
  expect_error(check_missing_labels(check_indicator = df_with_incorrect_missings))
})
