#' Initialize the metadata table
#' @param con A connection
#' @importFrom DBI dbSendQuery dbConnect
#' @importFrom RSQLite SQLite
#' @keywords internal

create_metadata_table <- function( con ) {

  dbSendQuery(conn = con,
  statement =
  "CREATE TABLE metadata  (
    indicator_code TEXT,
   title_at_source TEXT,
   description_indicator TEXT,
   frequency TEXT,
   code TEXT,
   type TEXT,
   last_update_data INT,
   last_update_data_source INT,
   last_structure_change INT,
   data_start INT,
   data_end INT,
   db_source_code TEXT,
   value REAL,
   actual INT,
   missing INT,
   locf INT,
   nocb INT,
   interpolate INT,
   forecast INT,
   backcast INT,
   impute INT,
   PRIMARY KEY (db_source_code)
  )")

}

#' Initialize the labelling table
#' @param con A connection
#' @keywords internal

create_labelling_table <- function( con ) {

  dbSendQuery(conn = con,
              statement =
                "CREATE TABLE labelling  (
                 db_source_code TEXT,
                 indicator_code TEXT,
                 description_indicator TEXT,
                 variable TEXT,
                 code TEXT,
                 description_variable TEXT,
                 PRIMARY KEY (db_source_code) )
              ")

}

#' Create the indicator table in the database
#' @keywords internal
create_indicator_table <- function( con ) {

   dbSendQuery(conn = con,
               statement =
                  "CREATE TABLE indicator  (
                  geo TEXT,
                  time INT,
                  value REAL,
                  UNIT TEXT,
                  indicator_source_code TEXT,
                  db_source_code TEXT,
                  year INT
                  month INT
                  day INT,
                  frequency TEXT,
                  validate TEXT,
                 PRIMARY KEY (db_source_code) )
              ")

}

#' Initialize a database
#' @param path A path
#' @importFrom RSQLite SQLite
#' @importFrom DBI dbListTables
#' @importFrom glue glue
#' @importFrom assertthat assert_that
#' @export

initialize_database <- function ( path = ":memory:") {

  con <- dbConnect(RSQLite::SQLite(), path )

  create_metadata_table(con)
  create_labelling_table(con)
  create_indicator_table(con)

  exists("con")

  table_name_difference <- setdiff( c("indicator", "labelling", "metadata"),
                                    DBI::dbListTables(con) )
  assertthat::assert_that(
     length(table_name_difference)==0,
     msg = glue::glue(
        "The table '{table_name_difference}' was not created. This is an error.")
  )

 con

}
