#' @importFrom DBI dbSendQuery dbConnect
#' @importFrom RSQLite SQLite
#' @keywords internal

create_metadata_table <- function( con ) {

  dbSendQuery(conn = con,
  statement =
  "CREATE TABLE metadata  (
    indicator_code TEXT,
   title_at_source TEXT,
   description_raw TEXT,
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

create_metadata_table <- function( con ) {

  dbSendQuery(conn = con,
              statement =
                "CREATE TABLE metadata  (
                 indicator_code TEXT, )
              ")

    }

initialize_database <- function () {
  con <- dbConnect(RSQLite::SQLite(), ":memory:")
  create_metadata_table(con)
  con
}


