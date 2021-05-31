#' Get Tidy Indicator(s) From Eurostat
#'
#' Get a Eurostat data product and save its metadata and the data in
#' tidy tables.
#'
#' This function creates a tidy indicator table that is ready to be inserted into a database.
#'
#' @param indic An imputed indicator
#' @param metadata A metadata table returned by \code{\link{get_eurostat_indicator}}.
#' @importFrom lubridate day month year
#' @importFrom dplyr mutate_if group_by ungroup
#' @importFrom dplyr distinct_all select right_join add_count
#' @importFrom tidyselect all_of
#' @importFrom tidyr unite pivot_wider
#' @family acquisition functions
#' @return A data frame that with updated actual, missing, approximated, forecasted,
#' nocb, locf estimates.
#' @export

update_metadata <- function ( indic, metadata ) {

  na_to_zero <- function(x) ifelse(is.na(x), 0, x)

  count_estimate_categories <- indic %>%
    select ( all_of(c("shortcode", "estimate")) ) %>%
    group_by ( .data$estimate ) %>%
    add_count( ) %>%
    ungroup() %>%
    distinct_all() %>%
    pivot_wider ( names_from = "estimate", values_from = "n")  %>%
    mutate_if ( is.numeric, na_to_zero)

  right_join (count_estimate_categories,
              metadata,
              by = c("shortcode", "actual", "missing", "locf")) %>%
    select ( all_of(names(metadata)) )

}
