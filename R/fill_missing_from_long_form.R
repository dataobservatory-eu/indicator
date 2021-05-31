#' Fill missing indicator values
#'
#' Missing values are filled when the original long form data frame
#' hides the fact that certain observations are not present.
#'
#' @param indic_to_fill A long form Eurostat indicator received by \code{\link{get_eurostat_indicator}}
#' that may contain implicitly missing values.
#' @importFrom dplyr mutate select group_by ungroup
#' @importFrom purrr map
#' @importFrom tidyr fill nest unnest pivot_longer pivot_wider
#' @importFrom tidyselect any_of
#' @importFrom rlang .data
#' @return A tibble explicitly showing missing observations.
#' @keywords internal
#'

fill_missing_from_long_form <- function( indic_to_fill ) {

  fill_indicator <- function(x){
    tmp_fill <-  x %>%
      select ( all_of(c("time", "geo", "value")) ) %>%
      pivot_wider ( names_from = "geo",
                    values_from = "value" ) %>%
      pivot_longer ( cols = -all_of (c("time")),
                     names_to = "geo", values_to = "value") %>%
      mutate ( method   = ifelse (is.na(.data$value), "missing", "actual"),
               estimate = ifelse (is.na(.data$value), "missing", "actual")
      )

    test_unique_observations(tmp_fill)

    tmp_fill
  }

  nest_to_fill <- indic_to_fill %>%
    select ( -any_of(c("year", "month", "day"))) %>%
    group_by ( .data$indicator_code,
               .data$description_at_source,
               .data$code_at_source,
               .data$frequency,
               .data$unit) %>%
    tidyr::nest () %>%
    mutate (data = purrr::map(.data$data, fill_indicator ))

  ##u <- nest_to_fill %>% tidyr::unnest(c("indicator_code", "description_at_source",
  ##                                      "code_at_source", "frequency", "unit"))

  nest_to_fill %>%
    tidyr::unnest( "data") %>%
    ungroup()

}


