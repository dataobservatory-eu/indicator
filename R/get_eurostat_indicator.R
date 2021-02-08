#' Get Tidy Indicator(s) From Eurostat
#'
#' Get a Eurostat data product and save its metadata and the data in
#' tidy tables.
#'
#' @param id The identifier code of a Eurostat data product.
#' @param eurostat_toc The Eurostat table of contents
#' @importFrom lubridate day month year
#' @importFrom dplyr mutate filter case_when relocate bind_cols group_by_all
#' @importFrom dplyr distinct_all select if_else rename left_join add_count
#' @importFrom eurostat get_eurostat label_eurostat
#' @importFrom purrr set_names
#' @importFrom tidyselect any_of all_of everything contains
#' @importFrom glue glue
#' @importFrom assertthat assert_that
#' @importFrom tidyr unite pivot_wider pivot_longer
#' @importFrom snakecase to_sentence_case
#' @return A list that contains three tables: the indicator, a value label
#' description table and a metadata table.
#' @export

get_eurostat_indicator <- function ( id, eurostat_toc = NULL ) {

  . <- missing <- NULL

  id <- tolower(id)

  ## The id must be available in the Eurostat Table of Contents ----------

  load_eurostat_metadata( envir = environment() )

  assertthat::assert_that (
    id %in% eurostat_toc$code,
    msg = glue::glue ("'{id}' is not a valid Eurostat product code")
    )


  indic_raw <- eurostat::get_eurostat(id)

  ## The metadata columns do not have a strict ordering, except for the case when
  ## Eurostat has complex tables with several indicators in one data file ----

  indicator_labels <- indic_dict

  indicator <- indic_raw %>%
    mutate ( year  = as.integer(lubridate::year(.data$time)),
             month = as.integer(lubridate::month(.data$time)),
             day   = as.integer(lubridate::day(.data$time))
             ) %>%
    mutate ( frequency = case_when (
      length( unique(.data$month) ) ==  1 ~ "A",
      length( unique(.data$month) ) ==  4 ~ "Q",
      length( unique(.data$day)   ) >= 28 ~ "D",
      TRUE ~"M"
    )) %>%
    mutate ( unit = ifelse (
      # if_else cannot be used here as length(condition) != length(true)
      test = "unit" %in% names(.),
      yes  = .data$unit,
      no   = NA_character_ )
             ) %>%
    mutate ( validate = if_else (
      condition = is.na(.data$values),
      true = "missing",
      false = "actual")
      ) %>%
    relocate ( # we want to have indicator identification elements first
               any_of ( tolower(indicator_labels$code_name) )) %>%
    relocate ( any_of (c("geo", "time", "values", "unit")) )

  ## The value labels do not have a strict ordering, except for the case when
  ## Eurostat has complex tables with several indicators in one data file ----

  val_labels <- indicator %>%
    select ( -any_of (c("values", "geo", "time", "unit",
                        "year", "month", "day",
                        "frequency", "validate"))
             ) %>%
    distinct_all()

  if ( any (names(val_labels) %in% tolower(indicator_labels$code_name)) &
            ncol(val_labels) > 1 ) {
    # reordering only makes sense if there are multiple columns and some of them
    # should be given priority

    val_labels <- val_labels %>%
      relocate ( # in case there are multiple indicators, should start with indicator description
        any_of ( tolower(indicator_labels$code_name) ),
        .before = -any_of(tolower(indicator_labels$code_name)) )

  }

  description_raw <- NULL # for NSE in the next block

  value_labels <- val_labels %>%
    eurostat::label_eurostat()

  labelling_table <- value_labels %>%
    purrr::set_names( paste0(names(.), "_description")) %>%
    bind_cols(val_labels) %>%
    unite ( col = "indicator_code",
                 -contains("description"),
                  remove = FALSE
            ) %>%
    mutate ( indicator_code = glue::glue ( "eurostat_{id}_{indicator_code}")) %>%
    mutate ( db_source_code = paste0("eurostat_", id),
             indicator_code = tolower(as.character(.data$indicator_code)) ) %>%
    relocate ( any_of(c("db_source_code", "indicator_code")),
               .before = everything()
               ) %>%
    unite ( col = description_indicator,
            contains("_description"),
            sep = " ",
            remove = FALSE ) %>%
    mutate ( description_indicator = snakecase::to_sentence_case(.data$description_indicator) )

  ## Creating a complete coding / labelling table -----------------------------

  indicator_description <- labelling_table %>%
    select ( -contains("_description"))  %>%
    pivot_longer ( cols = -all_of(c("db_source_code", "indicator_code",
                                    "description_indicator")),
                   names_to  = "variable",
                   values_to = "code")

  variable_description <- value_labels %>%
    set_names ( paste0(names(.), "_description")) %>%
    bind_cols ( val_labels ) %>%
    distinct_all() %>%
    pivot_longer ( cols = contains( "_description"),
                   names_to  = "variable",
                   values_to = "description_variable") %>%
    mutate ( variable = gsub("_description", "", .data$variable) ) %>%
    pivot_longer ( cols = -all_of(c("description_variable", "variable")),
                   names_to = "indic",
                   values_to = "code") %>%
    filter ( .data$variable == .data$indic ) %>%
    select ( -all_of(c("indic"))) %>%
    distinct_all()

  labelling <- indicator_description %>%
    left_join ( variable_description, by = c("variable", "code") )

  ## Further metadata and assertions  -------------------------------------------
  indicator_frequency <- unique(indicator$frequency)

  assertthat::assert_that(
    length(indicator_frequency)==1,
    msg = "The indicator frequency should be A, Q, M or D."
    )

  validation_summary <- summary ( as.factor(indicator$validate) )

  if (! "missing" %in% names(validation_summary) ) {
    validation_summary <- c(validation_summary, c( missing = 0))
  }

  ## Finalize the indicator -----------------------------------------------------

  indicator_final <- indicator %>%
    unite ( col = "indicator_code",
                  -all_of(c("geo", "time", "values", "unit",
                            "year", "month", "day",
                            "frequency", 'validate')),
                   remove = TRUE) %>%
    mutate ( indicator_code = glue::glue ( "eurostat_{id}_{indicator_code}" ) ) %>%
    mutate ( indicator_code = tolower(as.character(.data$indicator_code)) )

  ## The metadata is based on the Eurostat metadata information, but
  ## includes frequency and the date of the data download ---------------------
  metadata <- eurostat_toc %>%
    filter ( .data$code == id ) %>%
    distinct_all (
      #there are duplications in the TOC
      ) %>%
    rename ( last_update_data = .data$`last update of data`,
             last_structure_change = .data$`last table structure change`,
             data_start = .data$`data start`,
             data_end = .data$`data end`,
             title_at_source = .data$title,
             value = .data$values ) %>%
    mutate ( db_source_code = paste0("eurostat_", .data$code),
             last_update_data_source = as.Date(.data$last_update_data, format = "%d.%m.%Y"),
             last_structure_change = as.Date(.data$last_update_data, format = "%d.%m.%Y"),
             last_update_data = as.Date(Sys.Date()),
             data_start = as.integer(.data$data_start),
             data_end = as.integer(.data$data_end),
             frequency = indicator_frequency,
             actual =  as.numeric(validation_summary["actual"]),
             missing = as.numeric(validation_summary["missing"]),
             locf = 0, nocb = 0, interpolate = 0,
             forecast = 0, backcast = 0, impute =0 )

  metadata_final <- indicator_final %>%
    select ( all_of(c("indicator_code", "validate"))) %>%
    group_by_all() %>%
    add_count() %>%
    distinct_all() %>%
    pivot_wider ( names_from = "validate",
                  values_from = "n",
                  values_fill = 0 ) %>%
    mutate ( missing = ifelse ( "missing" %in% names(.),
                                .data$missing,
                                0)
             ) %>%
    bind_cols ( metadata %>%
                  select ( -all_of(c("missing", "actual")))
               ) %>%
    left_join ( labelling %>%
                  select ( all_of (c("indicator_code", "description_indicator"))),
                  by = 'indicator_code' ) %>%
    select ( all_of(c("indicator_code", "title_at_source", "description_indicator",
                      "db_source_code",
                      "frequency","data_start", "data_end",
                      "last_update_data", "last_update_data_source",
                      "last_structure_change",
                      "actual", "missing", "locf", "nocb", "interpolate",
                      "forecast", "backcast", "impute"
                      ))
             )

  list ( indicator = indicator_final,
         labelling = labelling,
         metadata = metadata_final )
}
