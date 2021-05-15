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
#' @family acquisition functions
#' @return A list that contains three tables: the indicator, a value label
#' description table and a metadata table.
#' @export

get_eurostat_indicator <- function ( id, eurostat_toc = NULL ) {

  . <- description_indicator <- indic_dict <- NULL
  db_source_code <- NULL # this should be eliminated with .data$db_source_code

  id <- tolower(id)

  ## The id must be available in the Eurostat Table of Contents ----------

  load_eurostat_metadata( envir = environment() )

  assertthat::assert_that (
    id %in% eurostat_toc$code,
    msg = glue::glue ("'{id}' is not a valid Eurostat product code")
    )

  ## The metadata columns do not have a strict ordering, except for the case when
  ## Eurostat has complex tables with several indicators in one data file ----

  indicator_labels <- indic_dict

  indic_downloaded <- eurostat::get_eurostat(id)

  if (is.null(indic_downloaded)) {
    indic_downloaded <- eurostat::get_eurostat(id)
  }

  if (is.null(indic_downloaded)) {
    stop ("Download stopped.")
  }


  ## The value labels do not have a strict ordering, except for the case when
  ## Eurostat has complex tables with several indicators in one data file ----

  val_labels <- indic_downloaded %>%
    select ( -any_of (c("value", "geo", "time", "unit",
                        "year", "month", "day",
                        "frequency", "estimate"))
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

  indicator <- tidy_indicator ( indic_raw = indic_downloaded,
                                indicator_labels = indicator_labels )

  ## Create the variable labeling -----------------
  value_codes <- val_labels %>%
    select (-any_of(c("time", "values"))) %>%
    distinct_all()

  value_labelling <- value_codes %>%
    eurostat::label_eurostat() %>%
    purrr::set_names( paste0(names(.), "_description"))

  value_labels <- value_codes %>%
    bind_cols ( value_labelling ) %>%
    tidyr::unite ( col = "indicator_code",
            -contains("description"),
            remove = FALSE
    )

  value_labelling <- value_labels %>%
    mutate ( indicator_code = glue::glue ( "eurostat_{id}_{indicator_code}")) %>%
    mutate ( db_source_code = paste0("eurostat_", id),
             indicator_code = tolower(as.character(.data$indicator_code)) ) %>%
    relocate ( any_of(c("db_source_code", "indicator_code")),
               .before = everything()
    ) %>%
    tidyr::unite ( col = description_indicator,
            contains("_description"),
            sep = " ",
            remove = FALSE ) %>%
    mutate ( description_indicator = snakecase::to_sentence_case(
      .data$description_indicator)
      )


  ## Create the unit labeling -------------------
  units <- indicator %>%
    select ( any_of ("unit"))  %>%
    distinct_all()

  unit_labels <- eurostat::label_eurostat(units) %>%
    mutate ( unit_label = paste0("[", .data$unit, "]")) %>%
    select ( all_of("unit_label")) %>%
    bind_cols ( units )

  ## Add labelling to coded table -------------------------------------------
  var_names <- names(value_labelling)[names(value_labelling) %in% names(indicator)]

  indicator_table_labelled <- indicator %>%
    left_join ( unit_labels, by = "unit" ) %>%
    left_join ( value_labelling, by = var_names  ) %>%
    unite ( description_indicator, c("description_indicator", "unit_label"), sep = " ") %>%
    select ( -any_of(c(var_names, paste0(var_names, ("_description"))))) %>%
    relocate ( all_of(c("description_indicator", "geo", "time", "value",
                        "unit","estimate", "method", "indicator_code")))


  ## Creating a complete coding / labeling table -----------------------------

  indicator_description <- value_labelling %>%
    select ( -contains("_description"))  %>%
    pivot_longer ( cols = -all_of(c("db_source_code", "indicator_code",
                                    "description_indicator")),
                   names_to  = "variable",
                   values_to = "code")

  variable_description <- value_labels %>%
    distinct_all() %>%
    select ( -any_of("indicator_code") ) %>%
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
    msg = "The indicator frequency should be A, Q, M, D or unknown."
    )

  estimation_summary <- summary ( as.factor(indicator$estimate) )

  if (! "missing" %in% names(estimation_summary) ) {
    # must have "actual" and "missing"
    estimation_summary <- c(estimation_summary, c( missing = 0))
  }

  ## Finalize the indicator -----------------------------------------------------

  indicator_final <- indicator %>%

    unite ( col = "indicator_code",
                  -all_of(c("geo", "time", "value", "unit",
                            "year", "month", "day",
                            "frequency", 'estimate', "method")),
                   remove = TRUE) %>%
    mutate ( db_source_code = glue::glue ( "eurostat_{id}" ) ) %>%
    mutate ( db_source_code = tolower( as.character(.data$db_source_code)) ) %>%
    mutate ( indicator_code = tolower(paste0(db_source_code, "_", .data$indicator_code)))

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
             title_at_source = .data$title ) %>%
    mutate ( db_source_code = paste0("eurostat_", .data$code),
             last_update_data_source = as.Date(.data$last_update_data, format = "%d.%m.%Y"),
             last_structure_change = as.Date(.data$last_update_data, format = "%d.%m.%Y"),
             last_update_data = as.Date(Sys.Date()),
             data_start = as.character(.data$data_start),
             data_end = as.character(.data$data_end),
             frequency = indicator_frequency,
             actual =  as.numeric(estimation_summary["actual"]),
             missing = as.numeric(estimation_summary["missing"]),
             locf = 0, nocb = 0, interpolate = 0,
             forecast = 0, backcast = 0, impute =0,
             recode = 0)

  metadata_final <- indicator_final %>%
    select ( all_of(c("indicator_code", "estimate")) ) %>%
    group_by_all() %>%
    add_count() %>%
    distinct_all() %>%
    pivot_wider ( names_from = "estimate",
                  values_from = "n",
                  values_fill = 0 ) %>%
    mutate ( missing = ifelse ( "missing" %in% names(.),
                                .data$missing,
                                0)
    ) %>%
    bind_cols ( metadata %>%
                  select ( -all_of(c("missing", "actual")) )
    ) %>%
    left_join ( labelling %>%
                  select ( all_of (c("indicator_code", "description_indicator")) ),
                by = 'indicator_code' ) %>%
    select ( all_of(c("indicator_code", "title_at_source", "description_indicator",
                      "db_source_code",
                      "frequency","data_start", "data_end",
                      "last_update_data", "last_update_data_source",
                      "last_structure_change",
                      "actual", "missing", "locf", "nocb", "interpolate",
                      "forecast", "backcast", "impute", "recode")
                    )
    ) %>%
    distinct_all() # I wonder what duplicates (unit of measure?)

  list ( indicator = indicator_final,
         labelling = labelling,
         metadata = metadata_final )
}

