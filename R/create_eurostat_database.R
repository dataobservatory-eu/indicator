#' Save Eurostat Indicators into a Database
#'
#' Download and process Eurostat indicators, create their labelling table and descriptive
#' metadata about the indicators. Save the two metadata tables into metadata tables, and
#' the tidy indicators themselves into an indicator table.
#' @param ids Identifiers of Eurostat statistical products.
#' @param db_path A path to save the database. Defaults to \code{tempdir()}
#' @importFrom DBI dbWriteTable dbAppendTable dbDisconnect
#' @importFrom RSQLite SQLite sqliteCopyDatabase
#' @return An Sqlite database with three tables: indicator, metadata and labelling.
#' @examples
#' \dontrun{
#' tmp_dir <- tempdir()
#' create_eurostat_database (
#'   ids = c("tin00092"),
#'   db_path = file.path(tmp_dir, "example1.db")
#' )
#' }
#' @export

create_eurostat_database <- function ( ids, db_path = tempdir() ) {

  con <- initialize_database()

  first <- get_eurostat_indicator(id = ids[1])

  DBI::dbWriteTable(con, "indicator",
                    first$indicator,
                    overwrite = TRUE,
                    row.names  = FALSE)

  DBI::dbWriteTable(con, "metadata",
                    first$metadata,
                    overwrite = TRUE,
                    row.names  = FALSE)

  DBI::dbWriteTable(con, "labelling",
                    first$labelling,
                    overwrite = TRUE,
                    row.names  = FALSE)

  i <- 2

  while ( length(ids) >= i ) {

    tmp <- get_eurostat_indicator(ids[i])
    DBI::dbAppendTable(con, "indicator",
                       tmp$indicator)
    DBI::dbAppendTable(con, "metadata",
                       tmp$metadata)
    DBI::dbAppendTable(con, "labelling",
                       tmp$labelling)

    i <- i+1
  }

  disc_con <- dbConnect(RSQLite::SQLite(), db_path )

  DBI::dbListTables(con)

  RSQLite::sqliteCopyDatabase(from = con, to = disc_con)

  DBI::dbDisconnect(con)
  DBI::dbDisconnect(disc_con)
}
