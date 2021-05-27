benelux_linear <- data.frame (
  time = as.Date (paste0(2010:2020, "-01-01")),
  NL = seq ( 30, 50, by = 2),
  BE = seq ( 35, 45, by = 1),
  LU = seq ( 2, 22, by = 2)
)


benelux_linear$NL[c(6, 10:11)] <- NA_real_
benelux_linear$BE[c(1:3, 10)] <- NA_real_
benelux_linear$LU[c(4:6)] <- NA_real_

test_indicator <- data.frame (
  geo = c("DE", "DE", "CH", "CH"),
  value = 1:4,
  time = as.Date(paste0(2020:2021, "-01-01")),
  estimate = rep("actual", 4)
)

is_unique_observations(test_indicator)

benelux_linear_long <- tidyr::pivot_longer( benelux_linear,
                                            cols = all_of(c("NL", "BE", "LU")),
                                            names_to = "geo", values_to = "value") %>%
  mutate ( estimate = ifelse (is.na(value), "missing", "actual")) %>%
  mutate ( frequency = "A",
           method = estimate )

test_that("unique indicator value tests", {
  expect_true(is_unique_observations (benelux_linear_long))
  expect_false(is_unique_observations (
    indicator = rbind (benelux_linear_long, benelux_linear_long)
  ))
})

test_that("stop or warning", {
  expect_error(test_unique_observations (
    indicator = rbind (benelux_linear_long, benelux_linear_long),
    stop_on_error = TRUE
  ))
  expect_warning(test_unique_observations (
    indicator = rbind (benelux_linear_long, benelux_linear_long)
  ))
  expect_true(test_unique_observations (
    indicator = benelux_linear_long
  ))
})
