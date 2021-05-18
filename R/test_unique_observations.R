#' Test Unique Observations
#'
#' Tidy indicators have observations that are unique. They must have on of the three
#' types of values: an actual, an estimated or a missing value.
#'
#' Approximation and other filling techniques require unique observations.
#'
#' @param indicator An indicator table to test.
#' @param stop_on_error Defaults to \code{TRUE} when the code stops with an error
#' message.  If \code{FALSE}, it displays non-unique values.
#' @importFrom dplyr select group_by add_count filter distinct_all
#' @importFrom assertthat assert_that
#' @return \code{TRUE} if the test is met, otherwise and error message.
#' @export

test_unique_observations <- function( indicator, stop_on_error = FALSE ) {

  assertthat::assert_that(
    all ( c("geo", "time", "value", "estimate") %in% names(indicator)),
    msg = "The indicator must have geo, time, value, estimate variable columns."
  )

  uniqueness <- indicator %>%
    dplyr::select ( all_of(c("geo", "time", "value", "estimate")) ) %>%
    dplyr::group_by ( .data$geo, .data$time, .data$value ) %>%
    add_count() %>%
    filter ( .data$n != 1 )

  if ( stop_on_error ) {
    assertthat::assert_that(nrow(uniqueness)==0,
                            msg= "test_unique_observations() found non-unique values.")
  } else if ( nrow( uniqueness ) == 0 ) {
    return(TRUE)
  } else {
    message ("Non-unique values:")
    print ( distinct(uniqueness))
  }

  return(FALSE)

}
