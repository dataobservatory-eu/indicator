#' Test time Column Formatting.
#'
#' The time dimension of an indicator must be a Date, or
#' it must be a year as an integer, or a numeric that
#' can be coerced into an integer.
#'
#' @param time An indicator table to test.
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
#' @keywords internal

is_time <- function (time) {

  ifelse ( test = class (time) %in% c("Date", "integer"),
           yes = TRUE,
           no = ifelse ( test = is.numeric(time),
                    yes = all (time %%1 == 0),
                    no = FALSE)
           )

}
