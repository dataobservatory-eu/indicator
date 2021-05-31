#' Save Eurostat Indicators into a Database
#'
#' Download and process Eurostat indicators, create their labelling table and descriptive
#' metadata about the indicators. Save the two metadata tables into metadata tables, and
#' the tidy indicators themselves into an indicator table.
#' @param indicator_table Indicators created by \code{\link{get_eurostat_indicator}} or \code{\link{impute_indicators}}.
#' @param metadata_table Meatadata tables created by \code{\link{get_eurostat_indicator}} or \code{\link{update_metadata}}.
#' @param labelling_table Labelling table created by \code{\link{get_eurostat_indicator}}
#' @param description_table Description and keywords created for the indicators
#' by \code{\link{add_keywords}}.
#' @param db_path A path to save the database. Defaults to \code{tempdir()}
#' @importFrom DBI dbWriteTable dbAppendTable dbDisconnect dbConnect dbReadTable
#' @importFrom RSQLite SQLite sqliteCopyDatabase
#' @return An SQLite database with four tables: indicator, metadata, labelling and description.
#' @family database functions
#' @examples
#' \donttest{
#' data ( "small_population")
#' population_long <- small_population %>%
#'   pivot_wider( names_from = "geo", values_from = "values") %>%
#'   pivot_longer ( cols = all_of(c("LI", "AD", "SM")),
#'                  names_to = "geo", values_to = "values")
#'
#' small_population_data <- get_eurostat_indicator(
#'   preselected_indicators = population_long,
#'   id = "demo_pjan")
#'
#'
#' create_database (indicator_table = small_population_data$indicator,
#'                  metadata_table = small_population_data$metadata,
#'                  labelling_table = small_population_data$labelling,
#'                  description_table = small_population_data$description,
#'                  db_path = file.path(tempdir(), "smallpopulation.db"))
#'
#' }
#' @export

create_database <- function ( indicator_table,
                              metadata_table,
                              labelling_table,
                              description_table,
                              db_path = tempdir() ) {

  con <- initialize_database()


  DBI::dbWriteTable(con, "indicator",
                    indicator_table,
                    overwrite = TRUE,
                    row.names  = FALSE)

  DBI::dbWriteTable(con, "metadata",
                    metadata_table,
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
