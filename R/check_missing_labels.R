#' Check missing labels in a tidy indicator table.
#'
#' Check if all missing observations are correctly labelled in the \code{method}
#' and \code{estimate} variable columns.
#'
#' @param check_indicator A tidy indicator from \code{\link{tidy_indicator}}.
#' @param only_actual A boolean, if \code{TRUE}, then the total number of observations
#' must be equal to the total number of \code{actual} and \code{missing observations}.
#' @importFrom assertthat assert_that
#' @importFrom dplyr ungroup filter select
#' @importFrom tidyselect all_of
#' @return A boolean if all assertions are met.
#' @keywords internal

check_missing_labels <- function(check_indicator,
                                 only_actual = TRUE) {

  assertthat::assert_that(
    all(c("value", "estimate", "method") %in% names (check_indicator)),
    msg = "check_missing_labels() requires the presensence of variable columns 'value', 'estimate', 'method'"
  )

  missing_values_df <- check_indicator %>%
    dplyr::ungroup() %>%
    select ( all_of(c("value", "estimate", "method"))) %>%
    filter (is.na(.data$value))

  assertthat::assert_that(
    all(missing_values_df$estimate == "missing"),
    msg = "Some missing values are not labelled as esimate='missing'."
  )

  assertthat::assert_that(
    all(missing_values_df$method == "missing"),
    msg = "Some missing values are not labelled as method='missing'."
  )

  if ( only_actual ) {

    actual_values_df <- check_indicator %>%
      dplyr::ungroup() %>%
      select (all_of(c("value", "estimate", "method"))) %>%
      filter (!is.na(.data$value))

    assertthat::assert_that(
      all(actual_values_df$method == "actual"),
      msg = "Some missing values are not labelled as method='actual', even though only_actual=TRUE."
    )

    assertthat::assert_that(
      all(actual_values_df$estimate == "actual"),
      msg = "Some missing values are not labelled as estimate='actual', even though only_actual=TRUE."
    )

  }

}
