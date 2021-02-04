#' Get Tidy Indicator(s) From Eurostat
#'
#' Get a Eurostat data product and save its metadata and the data in
#' tidy tables.
#'
#' @param id The identifier code of a Eurostat data product.
#' @importFrom lubridate day month year
#' @importFrom dplyr mutate filter case_when relocate bind_cols
#' @importFrom eurostat get_eurostat label_eurostat
#' @importFrom purrr set_names
#' @importFrom tidyselect any_of
#' @importFrom glue glue
#' @importFrom assertthat assert_that
#' @return A list that contains three tables: the indicator, a value label
#' description table and a metadata table.
#' @export

get_eurostat_indicator <- function ( id, eurostat_toc = NULL ) {

  . <- NULL

  ## The id must be available in the Eurostat Table of Contents ----------

  if ( is.null(eurostat_toc) )  {
    load_eurostat_metadata()
  }

  assertthat::assert_that (
    id %in% eurostat_toc$code,
    msg = glue::glue ("'{id}' is not a valid Eurostat product code")
    )

  indic_raw <- eurostat::get_eurostat(id)

  ## The metadata columns do not have a strict ordering, except for the case when
  ## Eurostat has complex tables with several indicators in one data file ----

  indicator_labels <- var_labels %>%
    filter ( grepl("indicator", full_name ))

  indicator <- indic_raw %>%
    mutate ( year  = lubridate::year(.data$time),
             month = lubridate::month(.data$time),
             day   = lubridate::day(.data$time)
             ) %>%
    mutate ( frequency = case_when (
      length( unique(.data$month) ) ==  1 ~ "A",
      length( unique(.data$month) ) ==  4 ~ "Q",
      length( unique(.data$day)   ) >= 28 ~ "D",
      TRUE ~"M"
    )) %>%
    mutate ( unit = ifelse ( "unit" %in% names(.),
                             yes = .data$unit,
                             no  = NA_character_)
             ) %>%
    relocate ( # we want to have indicator identification elements first
               any_of ( tolower(indicator_labels$code_name) )) %>%
    relocate ( any_of (c("geo", "time", "values", "unit")) )


  ## The value labels do not have a strict ordering, except for the case when
  ## Eurostat has complex tables with several indicators in one data file ----

  val_labels <- indicator %>%
    select ( -any_of (c("values", "geo", "time", "unit",
                        "year", "month", "day",
                        "frequency")) ) %>%
    distinct_all() %>%
    relocate ( # in case there are multiple indicators, should start with indicator description
               any_of ( tolower(indicator_labels$code_name)),
               .before = -any_of(tolower(indicator_labels$code_name)) )

  value_labels <- val_labels %>%
    eurostat::label_eurostat() %>%
    purrr::set_names( paste0(names(.), "_description")) %>%
    bind_cols(val_labels)

  ## The indicators must have a unique frequency, i.e.
  ## annual, quarterly, monthly, weekly or daily -------------------------------
  indicator_frequency <- unique(indicator$frequency)

  assertthat::assert_that(length(indicator_frequency)==1,
                          msg = "The indicator frequency should be A, Q, M or D.")

  ## The metadata is based on the Eurostat metadata information, but
  ## includes frequency and the date of the data download ---------------------
  metadata <- eurostat_toc %>%
    filter ( code == id ) %>%
    rename ( last_update_data = `last update of data`,
             last_structure_change = `last table structure change`,
             data_start = `data start`,
             data_end = `data end`) %>%
    mutate ( last_update_data_source = as.Date(last_update_data, format = "%d.%m.%Y"),
             last_structure_change = as.Date(last_update_data, format = "%d.%m.%Y"),
             last_update_data = as.Date(Sys.Date()),
             data_start = as.integer(data_start),
             data_end = as.integer(data_end),
             frequency = indicator_frequency )

  list ( indicator = indicator,
         value_labels = value_labels,
         metadata = metadata)
}
