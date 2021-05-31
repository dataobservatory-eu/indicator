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


population_long <- small_population %>%
  pivot_wider( names_from = "geo", values_from = "values") %>%
  pivot_longer ( cols = all_of(c("LI", "AD", "SM")),
                 names_to = "geo", values_to = "values")

# Explicit missing values are present in AD but not in SM
population_long <- population_long %>%
  dplyr::anti_join(
    population_long %>%
      filter (geo == "SM",
              is.na(values)),
    by = c("unit", "sex", "time", "geo", "values"))

small_population_data <- get_eurostat_indicator(
  preselected_indicators = population_long,
  id = "demo_pjan")

small_population_indicator <- small_population_data$indicator
small_population_metadata <- small_population_data$metadata
small_population_labelling  <- small_population_data$labelling
small_population_description  <- small_population_data$description

usethis::use_data(small_population_indicator, overwrite = TRUE)
usethis::use_data(small_population_metadata,  overwrite = TRUE)
usethis::use_data(small_population_labelling, overwrite = TRUE)
usethis::use_data(small_population_description, overwrite = TRUE)
