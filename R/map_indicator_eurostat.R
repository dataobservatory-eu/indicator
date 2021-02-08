#' Map Eurostat indicators into a databse
#'
#' @param ids Identifiers of Eurostat statistical products.
#' @param path A path to save the database.
#' @importFrom DBI dbWriteTable dbAppendTable dbDisconnect
#' @importFrom RSQLite SQLite sqliteCopyDatabase
#' @retrun An Sqlite database with two tables: metadata and labelling.
#' @examples
#' \dontrun{
#' map_indicator_eurostat <- function (
#'  ids = c("ISOC_R_BLT12_I", "isoc_cicce_use", "teicp090"),
#'  path = 'not_included/example.db' )
#' }

map_indicator_eurostat <- function ( ids, path ) {

  con <- initialize_database()

  first <- get_eurostat_indicator(ids[1])
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
    DBI::dbAppendTable(con, "metadata",
                       tmp$metadata)
    DBI::dbAppendTable(con, "labelling",
                       tmp$labelling)

    i <- i+1
  }

  path = "not_included/test2.db"

  disc_con <- dbConnect(RSQLite::SQLite(), path )

  DBI::dbListTables(con)

  RSQLite::sqliteCopyDatabase(from = con, to = disc_con)

  DBI::dbDisconnect(con)
  DBI::dbDisconnect(disc_con)
}
