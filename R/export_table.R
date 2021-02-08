#' Export a table
#'
#' @param table A table name.
#' @param db A path to the database to import the table from.
#' @param savefile A path to save the exported table.
#' @importFrom DBI dbConnect dbReadTable
#' @importFrom fs path_ext
#' @importFrom writexl write_xlsx

export_table <- function ( table, db, savefile  ) {

  extension <- fs::path_ext(savefile)

  disc_con <- dbConnect(RSQLite::SQLite(), db )

  this_table <- DBI::dbReadTable(disc_con, table)

  if ( extension %in% c("xls", "xlsx") ) {
    writexl::write_xlsx(this_table, savefile)
  }

  DBI::dbDisconnect(disc_con)
}

