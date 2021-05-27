#' Fill missing indicator values
#'
#' Missing values are filled when the original long form data frame
#' hides the fact that certain observations are not present.
#'
#' @param indic_to_fill A long form Eurostat indicator received by \code{\link{get_eurostat_indicator}}
#' that may contain implicitly missing values.
#' @importFrom dplyr mutate select group_by
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
      pivot_wider ( names_from = "geo", values_from = "value" ) %>%
      pivot_longer ( cols = -all_of (c("time")),
                     names_to = "geo", values_to = "value") %>%
      left_join ( x,
                  by = c("time", "geo", "value")
                  ) %>%
      tidyr::fill ( all_of(c("unit", "description_at_source", "code_at_source", "frequency")),
                    .direction = "downup" ) %>%
      mutate ( method   = ifelse (is.na(.data$value), "missing", "actual"),
               estimate = ifelse (is.na(.data$value), "missing", "actual")
      )

    test_unique_observations(tmp_fill, stop_on_error =  FALSE)

    tmp_fill
  }

  nest_to_fill <- indic_to_fill %>%
    select ( -any_of(c("year", "month", "day"))) %>%
    group_by ( .data$indicator_code ) %>%
    tidyr::nest () %>%
    mutate (data = purrr::map(.data$data, fill_indicator ))

  nest_to_fill %>%
    tidyr::unnest(cols = .data$data )

}


