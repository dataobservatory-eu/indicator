data ("small_population")

small_population

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

test_population <- get_eurostat_indicator(
  preselected_indicators = population_long,
  id = "demo_pjan")

test_population$metadata


test_that("labelling is correct", {
  expect_true(all(
    c("[number]", "Females", "Males", "Total") %in% test_population$labelling$var_label
  ))
  expect_true(all(
    c("NR", "F", "M", "T") %in% test_population$labelling$var_code
  ))
  expect_true(all(
    c("unit", "sex", "sex", "sex") %in% test_population$labelling$var_name
  ))
})

indic_df <- test_population$indicator

unique ( indic_df$indicator_code)


test_that("correct actual values are returned", {
  # number of actual population observations for females in original and returned data frame
  expect_equal(
    indic_df %>%
      filter( .data$indicator_code == 'eurostat_demo_pjan_f_nr',
              !is.na(.data$value)) %>%
      nrow(),
    population_long %>% filter ( .data$sex == "F",
                                 !is.na(.data$values) ) %>%
      nrow())
  expect_true(
    # explicit missing numbers increase the number of missings
    {indic_df %>%
        filter( .data$indicator_code == 'eurostat_demo_pjan_f_nr',
                is.na(.data$value)) %>%
        nrow() } > {
          population_long %>% filter ( .data$sex == "F",
                                       is.na(.data$values) ) %>%
            nrow()

        }
  )
})

actual_female_population <-indic_df %>%
    filter( .data$indicator_code == 'eurostat_demo_pjan_f_nr',
            !is.na(.data$value))


missing_female_population <-indic_df %>%
  filter( .data$indicator_code == 'eurostat_demo_pjan_f_nr',
          is.na(.data$value))

test_that("Correct metadata is returned", {
  expect_equal(test_population$metadata %>%
                 ungroup() %>%
                 filter ( .data$indicator_code == 'eurostat_demo_pjan_f_nr') %>%
                 select ( all_of("missing")) %>% unlist() %>% as.numeric(),
               nrow(missing_female_population))
  expect_equal(test_population$metadata %>%
                 ungroup() %>%
                 filter ( .data$indicator_code == 'eurostat_demo_pjan_f_nr') %>%
                 select ( all_of("actual")) %>% unlist() %>% as.numeric(),
               nrow(actual_female_population))
  expect_equal(test_population$metadata %>%
                 ungroup() %>%
                 filter ( .data$indicator_code == 'eurostat_demo_pjan_f_nr') %>%
                 select ( all_of("actual")) %>% unlist() %>% as.numeric(),
               population_long %>% filter ( .data$sex == "F",
                                            !is.na(.data$values) ) %>%
                 nrow())
})



