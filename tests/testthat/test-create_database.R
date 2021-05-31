
data ( "small_population")
population_long <- small_population %>%
  pivot_wider( names_from = "geo", values_from = "values") %>%
  pivot_longer ( cols = all_of(c("LI", "AD", "SM")),
                 names_to = "geo", values_to = "values")

small_population_data <- get_eurostat_indicator(
  preselected_indicators = population_long,
  id = "demo_pjan")


names ( small_population_data$indicator)
names ( small_population_metadata)
names ( small_population_data$labelling)

save_path <- file.path(tempdir(), "testdb.db")

create_database (indicator_table = small_population_data$indicator,
                 metadata_table = small_population_data$metadata,
                 labelling_table = small_population_data$labelling,
                 description_table = small_population_data$description,
                 db_path = save_path)

disc_con <- RSQLite::dbConnect(RSQLite::SQLite(), save_path )
DBI::dbListTables(disc_con)

test_that("database is created", {
  expect_true(file.exists ( file.path(tempdir(), "testdb.db") ))
  expect_equal(DBI::dbListTables(disc_con), c("description", "indicator", "labelling", "metadata"))
  expect_equal(names(
    DBI::dbReadTable(disc_con, "description")
  ), c("shortcode", "description", "keyword_1", "keyword_2", "keyword_3", "keyword_4",
       "further_keywords", "indicator_code", "description_at_source", "original_source"))
  expect_equal(names(
    DBI::dbReadTable(disc_con, "indicator")
  ), c("shortcode", "geo", "time", "unit", "value", "estimate", "method", "frequency" ))
  expect_equal(names(
    DBI::dbReadTable(disc_con, "labelling")
  ), c("shortcode", "var_name", "var_code", "var_label"))
})

DBI::dbDisconnect(disc_con)
