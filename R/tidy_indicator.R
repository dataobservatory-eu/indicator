#' Tidy raw indicator
#'
#' @param indic_raw A raw Eurostat indicator
#' @importFrom lubridate day month year
#' @importFrom dplyr case_when mutate if_else relocate
#' @importFrom tidyselect any_of
#' @keywords internal

tidy_indicator <- function ( indic_raw ) {

  indic_raw %>%
    mutate ( year  = as.integer(lubridate::year(.data$time)),
             month = as.integer(lubridate::month(.data$time)),
             day   = as.integer(lubridate::day(.data$time))
    ) %>%
    mutate ( frequency = case_when (
      length( unique(.data$month ) ) ==  1 ~ "A",
      length( unique(.data$month ) ) ==  4 ~ "Q",
      length( unique(.data$day )   ) >= 28 ~ "D",
      TRUE ~"M"
    )) %>%
    mutate ( unit = ifelse (
      # if_else cannot be used here as length(condition) != length(true)
      test = "unit" %in% names(.),
      yes  = .data$unit,
      no   = NA_character_ )
    ) %>%
    mutate ( validate = if_else (
      condition = is.na(.data$value),
      true = "missing",
      false = "actual")
    ) %>%
    relocate ( # we want to have indicator identification elements first
      any_of ( tolower(indicator_labels$code_name) )) %>%
    relocate ( any_of (c("geo", "time", "value", "unit")) )
}
