## code to prepare `small_population` dataset goes here

require(dplyr)
require(eurostat)

population <- eurostat::get_eurostat("demo_pjan")

small_population <- population %>%
  filter ( geo %in% c("AD", "LI", "SM"),
           age == "TOTAL",
           time > as.Date ("2009-01-01"),
           time < as.Date ("2021-01-01")) %>%
  select ( -all_of("age"))

assertthat::assert_that(nrow(small_population)==60,
                        msg = "The unit tests are made with this filtering. Please do not alter the number of observations")

usethis::use_data(small_population, overwrite = FALSE)
