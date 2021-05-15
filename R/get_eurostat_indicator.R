#' Get Tidy Indicator(s) From Eurostat
#'
#' Get a Eurostat data product and save its metadata and the data in
#' tidy tables.
#'
#' @param id The identifier code of a Eurostat data product.
#' \code{\link[eurostat]{get_eurostat}} will be called with \code{id} if
#' \code{preselected_indicators=NULL}.
#' @param preselected_indicators A pre-filtered datatable from \code{\link[eurostat]{get_eurostat}}.
#' Defaults to \code{NULL}, when the download will be called with \code{id}.
#' @param eurostat_toc The Eurostat table of contents
#' @importFrom lubridate day month year
#' @importFrom dplyr mutate filter case_when relocate bind_cols group_by_all
#' @importFrom dplyr distinct_all select if_else rename left_join add_count
#' @importFrom dplyr anti_join
#' @importFrom eurostat get_eurostat label_eurostat
#' @importFrom purrr set_names
#' @importFrom tidyselect any_of all_of everything contains
#' @importFrom glue glue
#' @importFrom assertthat assert_that
#' @importFrom tidyr unite pivot_wider pivot_longer
#' @importFrom snakecase to_sentence_case
#' @family acquisition functions
#' @return A list that contains three tables: the indicator, a value label
#' description table and a metadata table.
#' @export

get_eurostat_indicator <- function ( preselected_indicators = NULL,
                                     id,
                                     eurostat_toc = NULL ) {

  indic_dict <- NULL # must be initialized, will be loaded
  id <- tolower(id)

  ## The id must be available in the Eurostat Table of Contents ----------
  load_eurostat_metadata( envir = environment() )

  add_test <- eurostat_toc[1,]
  add_test$title = "Testing variable"; add_test$code = "test"; add_test$type <- "test table"
  eurostat_toc <- rbind (eurostat_toc, add_test)

  assertthat::assert_that (
    id %in% eurostat_toc$code,
    msg = glue::glue ("'{id}' is not a valid Eurostat product code")
    )

  ## The metadata columns do not have a strict ordering, except for the case when
  ## Eurostat has complex tables with several indicators in one data file ----

  indicator_labels <- indic_dict

  if (is.null(preselected_indicators)) {
    indic_downloaded <- eurostat::get_eurostat(id)

    if (is.null(indic_downloaded)) {
      indic_downloaded <- eurostat::get_eurostat(id)
    }

    if (is.null(indic_downloaded)) {
      stop ("Download stopped.")
    }
  } else {
    required_columns <- c("geo", "time", "values") # if unit is missing, it will be added later in tidy_indicator()
    not_present <- required_columns[!required_columns %in% names (preselected_indicators)]
    not_present <-  paste(not_present, collapse = ", ")
    is_preselected <- ifelse ( is.null(preselected_indicators), "", "the preselected part of")

    assertthat::assert_that (
      nchar(not_present) == 0,
      msg = glue::glue ("Columns {not_present} are not present in {is_preselected} {id}")
    )
   indic_downloaded <- preselected_indicators
  }

  indic_downloaded <- indic_downloaded %>%
    mutate ( indicator_code = glue::glue ("eurostat_{id}"),
             db_source_code = .data$indicator_code,
             description_indicator = eurostat_toc$title[eurostat_toc$code == id] )


  ## The value labels do not have a strict ordering, except for the case when
  ## Eurostat has complex tables with several indicators in one data file ----

  val_labels <- indic_downloaded %>%
    select ( -any_of (c("value", "geo", "time", "unit",
                        "year", "month", "day",
                        "frequency", "estimate"))
    ) %>%
    distinct_all()

  ## Reorder the columns -------------------------------------------------------
  if (any (names(val_labels) %in% tolower(indicator_labels$code_name)) &
       ncol(val_labels) > 1 ) {
    # reordering only makes sense if there are multiple columns and some of them
    # should be given priority

    val_labels <- val_labels %>%
      relocate ( # in case there are multiple indicators, should start with indicator description
        any_of ( tolower(indicator_labels$code_name) ),
        .before = -any_of(tolower(indicator_labels$code_name)) )

  }

  indicator <- tidy_indicator ( indic_raw = indic_downloaded,
                                indicator_labels = indicator_labels )

  # At this point various sub-indicator columns may be present and the values are
  # not distinct in the id.
  # Also indicator may be present with several units.

  ## Create the variable labeling for each variable code
  ## for 'folder' type eurostat products ------------------------------------------

  value_codes <- val_labels %>%
    select (-any_of(c("time", "values"))) %>%
    distinct_all()

  if ( ncol(value_codes)>0 ) {

    value_labelling <- value_codes %>%
      select ( -any_of(c("indicator_code", "db_source_code", "description_indicator"))) %>%
      eurostat::label_eurostat()

    value_labelling <- value_labelling %>%
      purrr::set_names(paste0(names(value_labelling), "_description")) %>%
      bind_cols ( value_codes ) %>%
      relocate ( -any_of(c("indicator_code", "db_source_code", "description_indicator")),
                         .after = "description_indicator")

    value_labels <- value_labelling  %>%
      tidyr::unite ( col = "extend_indicator_code",
                     -contains("description"),
                     -any_of(c("db_source_code", "indicator_code")),
                     remove = FALSE
      ) %>%
      tidyr::unite ( col = "extend_description",
                     contains("_description"),
                     sep = " - ",
                     remove = TRUE
      )
  } else {
    value_labels <- value_codes # this should be an empty tibble
  }

  common_ext_vars <- names(value_labels)[names(value_labels) %in% names(indicator)]
  table_specific_vars <- common_ext_vars[!common_ext_vars %in% c("indicator_code", "description_indicator", "db_source_code")]

  if ( length(common_ext_vars)>3 ) {
    indicator_ext <- indicator %>%
      left_join ( value_labels, by = common_ext_vars ) %>%
      relocate ( -any_of(c("indicator_code", "db_source_code", "description_indicator")),
                 .after = "description_indicator") %>%
      unite ( col = "description_indicator",
              all_of (c("description_indicator", "extend_description")),
              sep = " - ",
              remove = TRUE) %>%
      unite ( col = "indicator_code",
              all_of (c("indicator_code", "extend_indicator_code")),
              sep = "_",
              remove = TRUE) %>%
      select ( -all_of(table_specific_vars ))
  } else {
    indicator_ext <- indicator
  }

  ## Create the unit labeling -------------------
  units <- indicator_ext %>%
    select ( any_of ("unit"))  %>%
    distinct_all()

  if ( all(is.na(units$unit)) ) {
    units$unit_label <- "[no unit]"
  } else {
    unit_labels <- eurostat::label_eurostat(units) %>%
      mutate ( unit_label = paste0("[", .data$unit, "]")) %>%
      select ( all_of("unit_label")) %>%
      mutate ( unit_label = tolower(as.character(.data$unit_label))) %>%
      bind_cols ( units )
  }

  indicator_ext_unit <- indicator_ext %>%
    mutate ( description_indicator =
               snakecase::to_sentence_case(.data$description_indicator) ) %>%
    left_join ( unit_labels, by = c("unit") ) %>%
    unite ( col = "description_indicator",
            all_of (c("description_indicator", "unit_label")),
            sep = " ",
            remove = TRUE) %>%
    unite ( col = "indicator_code",
            all_of (c("indicator_code", "unit")),
            sep = "_",
            remove = FALSE) %>%
    mutate ( indicator_code = snakecase::to_snake_case(.data$indicator_code))

  # We make implicitly missing observations explicit, and leave out the separate
  # year, month, day columns---------------------------------------------------

  indicator_final <- fill_missing_from_long_form (
    # From long form raw sources you may get implicitly missing values, not showing up
    # in the dataframe. We make them explicit.
    indic_to_fill = indicator_ext_unit )


  ## Further metadata and assertions  -------------------------------------------
  indicator_frequency <- unique( indicator_final$frequency)

  assertthat::assert_that(
    length(indicator_frequency)==1,
    msg = "The indicator frequency should be A, Q, M, D or unknown."
    )

  ## The metadata is based on the Eurostat metadata information, but
  ## includes frequency and the date of the data download ---------------------

  metadata <- eurostat_toc %>%
    filter ( .data$code == id ) %>%
    select ( -any_of(c("values"))) %>%
    distinct_all (
      #there are duplications in the TOC
      ) %>%
    rename ( last_update_data = .data$`last update of data`,
             last_structure_change = .data$`last table structure change`,
             data_start = .data$`data start`,
             data_end = .data$`data end`,
             title_at_source = .data$title ) %>%
    mutate ( db_source_code = paste0("eurostat_", .data$code),
             last_update_data_source = as.Date(.data$last_update_data, format = "%d.%m.%Y"),
             last_structure_change = as.Date(.data$last_update_data, format = "%d.%m.%Y"),
             last_update_data = as.Date(Sys.Date()),
             data_start = as.character(.data$data_start),
             data_end = as.character(.data$data_end),
             frequency = indicator_frequency,
             locf = 0, nocb = 0, approximate=0,
             forecast = 0, backcast = 0, impute=0,
             recode = 0) %>%
    select ( -all_of("code"))

  check_missing_labels(indicator_final)

  metadata_final <- indicator_final %>%
    select (
      all_of(c("indicator_code", "description_indicator", "db_source_code",
               "estimate", "frequency")) ) %>%
    group_by_all() %>%
    add_count() %>%
    distinct_all()  %>%
    pivot_wider ( names_from = "estimate",
                  values_from = "n",
                  values_fill = 0 )

  metadata_final <- metadata_final %>%
    mutate ( missing = ifelse ( "missing" %in% names(metadata_final),
                                .data$missing,
                                0)
    ) %>%
    left_join ( metadata,  by = c("db_source_code", "frequency")) %>%
    distinct_all() # I wonder what duplicates (unit of measure?)

  labelling <- unit_labels %>%
    mutate ( var_name = "unit") %>%
    rename ( var_code = .data$unit,
             var_label = .data$unit_label )

  if ( ncol(value_labelling)>0 ) {
    labelling <- labelling %>%
       full_join (
         value_labelling %>%
           select ( -all_of( c("description_indicator", "indicator_code",
                               "db_source_code"))) %>%
           pivot_longer ( contains("_description"),
                          names_to = "var_name2",
                          values_to = "var_label") %>%
           pivot_longer ( -all_of(c("var_name2", "var_label")),
                          names_to = "var_name",
                          values_to = "var_code") %>%
           mutate ( var_name2 = gsub("_description", "", .data$var_name2)) %>%
           filter ( .data$var_name2 == .data$var_name) %>%
           select ( -all_of("var_name2") ),
         by = c("var_label", "var_code", "var_name")
         )
  }

  list ( indicator = indicator_final,
         labelling = labelling,
         metadata = metadata_final )
}

