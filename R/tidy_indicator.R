#' Tidy Raw Indicator
#'
#' The indicator downloaded from Eurostat is brought to a tidy format.
#'
#' @param indic_raw A raw Eurostat indicator
#' @param indicator_labels The codes and description of \code{indic} columns.
#' @importFrom lubridate day month year
#' @importFrom dplyr case_when mutate if_else relocate rename
#' @importFrom tidyselect any_of
#' @return A tibble with fixed form.
#' @keywords internal
#'

tidy_indicator <- function ( indic_raw, indicator_labels = NULL ) {

  if ( "values" %in% names (indic_raw) ) {
    indic_raw <- indic_raw %>%
      rename ( value = .data$values)
  }

  if ( ! "estimate" %in% names(indic_raw) ) {
    indic_raw$estimate <- "actual"
  }

  if ( ! "method" %in% names(indic_raw) ) {
    indic_raw$method <- NA_character_
  }

  if ( ! "unit" %in% names(indic_raw) ) {
    indic_raw$unit <- NA_character_
  }

  tmp <- indic_raw %>%
    mutate ( year  = as.integer(lubridate::year(.data$time)),
             month = as.integer(lubridate::month(.data$time)),
             day   = as.integer(lubridate::day(.data$time))
    ) %>%
    mutate ( frequency = case_when (
      # establish the frequency of the data
      length( unique(.data$month ) ) ==  1 ~ "A", # if there is only one per year, annual
      length( unique(.data$month ) ) ==  4 ~ "Q", # if there are four months present, quarterly
      length( unique(.data$day )   ) >= 28 ~ "D", # if there are at least 28 DAYS, daily
      length( unique(.data$month)  ) == 12 ~ "M", # if there are at least 12 month, monthly
      TRUE ~"unknown"
    ))  %>%
    mutate ( estimate = if_else (
      condition = is.na(.data$value),
      true = "missing",
      false = .data$estimate )
    ) %>%
    mutate ( method = if_else (
      condition = is.na(.data$method),
      true = .data$estimate,
      false = .data$method )
      ) %>%
    relocate ( any_of ("unit"), .before = "geo") %>%
    relocate ( # we want to have indicator identification elements first
      any_of ( tolower(indicator_labels$code_name) ))

  tmp
}
