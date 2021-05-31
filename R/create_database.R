#' Save Eurostat Indicators into a Database
#'
#' Download and process Eurostat indicators, create their labelling table and descriptive
#' metadata about the indicators. Save the two metadata tables into metadata tables, and
#' the tidy indicators themselves into an indicator table.
#' @param indicator_tables Indicators created by \code{\link{get_eurostat_indicator}} or \code{\link{impute_indicators}}.
#' @param metadata_tables Meatadata tables created by \code{\link{get_eurostat_indicator}} or \code{\link{update_metadata}}.
#' @param labelling_table Labelling table created by \code{\link{get_eurostat_indicator}}
#' @param description_table Description and keywords created for the indicators
#' by \code{\link{add_keywords}}.
#' @param db_path A path to save the database. Defaults to \code{tempdir()}
#' @importFrom DBI dbWriteTable dbAppendTable dbDisconnect dbConnect
#' @importFrom RSQLite SQLite sqliteCopyDatabase
#' @return An Sqlite database with three tables: indicator, metadata and labelling.
#' @family database functions
#' @examples
#' \dontrun{
#' ## See vignette
#' )
#' }
#' @export

create_database <- function ( indicator_tables,
                              metadata_tables,
                              labelling_table,
                              description_table,
                              db_path = tempdir() ) {

  con <- initialize_database()


  DBI::dbWriteTable(con, "indicator",
                    indicator_tables,
                    overwrite = TRUE,
                    row.names  = FALSE)

  DBI::dbWriteTable(con, "metadata",
                    metadata_tables,
                    overwrite = TRUE,
                    row.names  = FALSE)

  DBI::dbWriteTable(con, "labelling",
                    labelling_table,
                    overwrite = TRUE,
                    row.names  = FALSE)

  DBI::dbWriteTable(con, "description",
                    description_table,
                    overwrite = TRUE,
                    row.names  = FALSE)



  disc_con <- dbConnect(RSQLite::SQLite(), db_path )
  DBI::dbListTables(con)

  RSQLite::sqliteCopyDatabase(from = con, to = disc_con)
  DBI::dbListTables(disc_con)

  DBI::dbDisconnect(con)
  DBI::dbDisconnect(disc_con)
}
