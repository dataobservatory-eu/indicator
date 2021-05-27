#' Test Unique Observations
#'
#' Tidy indicators have observations that are unique. They must have on of the three
#' types of values: an actual, an estimated or a missing value.
#'
#' Approximation and other filling techniques require unique observations.
#'
#' @param indicator An indicator table to test.
#' @importFrom dplyr select group_by add_count filter distinct_all
#' @return \code{TRUE} if the test is met, otherwise \code{FALSE}.
#' @examples{
#' test_indicator <- data.frame (
#'    geo = c("DE", "DE", "CH", "CH"),
#'    value = 1:4,
#'    time = as.Date(paste0(2020:2021, "-01-01")),
#'    estimate = rep("actual", 4)
#'  )
#' is_unique_observations(test_indicator)
#' }
#' @export

is_unique_observations <- function( indicator ) {

  assertthat::assert_that(
    all ( c("geo", "time", "value", "estimate") %in% names(indicator)),
    msg = "The indicator must have geo, time, value, estimate variable columns."
  )

  uniqueness <- indicator %>%
    dplyr::select ( all_of(c("geo", "time", "value", "estimate")) ) %>%
    dplyr::group_by ( .data$geo, .data$time, .data$value ) %>%
    dplyr::add_count() %>%
    filter ( .data$n != 1 )

  ifelse (nrow(uniqueness)==0, TRUE, FALSE)

}

#' Test Unique Observations for Approximation functions.
#'
#' Tidy indicators have observations that are unique. They must have on of the three
#' types of values: an actual, an estimated or a missing value.
#'
#' Approximation and other filling techniques require unique observations.
#'
#' This is an internal function and can give either a warning or
#'
#' @param indicator An indicator table to test.
#' @param stop_on_error Defaults to \code{TRUE} when the code stops with an error
#' message.  If \code{FALSE}, it displays non-unique values.
#' @importFrom dplyr select group_by add_count filter distinct_all
#' @importFrom assertthat assert_that
#' @importFrom utils head
#' @return \code{TRUE} if the test is met, otherwise warning if \code{stop_on_error = FALSE}
#' and returns \code{FALSE} or stops with an error if \code{stop_on_error = TRUE}.
#' @keywords internal

test_unique_observations <- function( indicator, stop_on_error = FALSE ) {

  ifelse ( is_unique_observations(indicator),
           return(TRUE), ifelse ( stop_on_error,
                          stop("Non-unique values found in ", head(indicator)),
                          warning("Non-unique values found in ", head(indicator))) )

  FALSE
}


