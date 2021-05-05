#' Test Unique Observations
#'
#' Approximation and other filling techniques require unique observations
#' @param indicator An indicator table to test.
#' @param path A path to save the database.
#' @importFrom dplyr select group_by add_count
#' @return \code{NULL} if the test is met, otherwise and error message.
#' @export

test_unique_observations <- function( indicator ) {

  uniqueness <- indicator %>%
    dplyr::select ( all_of(c("geo", "time", "value", "estimate")) ) %>%
    dplyr::group_by (  .data$geo, .data$time, .data$value ) %>%
    add_count() %>%
    filter ( n != 1 )

  if ( nrow(uniqueness)==0 ) return(NULL) else {
    stop ( uniqueness ,  "\n is not unique. This is an error." )
  }
}


#' Linear approximation of missing values
#'
#' @param ids Identifiers of Eurostat statistical products.
#' @param path A path to save the database.
#' @importFrom timetk tk_ts
#' @importFrom lubridate year
#' @importFrom tidyr pivot_wider
#' @importFrom dplyr select filter left_join mutate case_when full_join anti_join
#' @importFrom zoo na.approx
#' @return A tibble updated with with approximated values.
#' @export

na_approx <- function (indicator) {

  test_unique_observations(indicator)

  freq <- unique(indicator$frequency)

  tmp <- indicator %>%
      select ( all_of(c("time", "geo", "value", "method"))) %>%
      pivot_wider( names_from = "geo",
                   values_from = "value")

  start_value <- lubridate::year(min(tmp$time))

  indy_ts <- timetk::tk_ts(tmp ,
                           start = start_value,
                           frequency =   case_when ( freq == "A" ~ 1,
                                                     freq == "Q" ~ 4,
                                                     freq == "M" ~ 12),
                           silent = TRUE)

  approximated <- zoo::na.approx(indy_ts)

  long_form_approx <- as.data.frame (approximated) %>%
    bind_cols( tmp %>%
                 select ( all_of("time"))
               ) %>%
    pivot_longer( cols = -all_of("time"),
                  names_to = 'geo',
                  values_to = 'approx') %>%
    mutate ( method  = "approx") %>%
    filter ( !is.na("approx"))



  indicator %>%
    full_join ( anti_join (long_form_approx, indicator )  ) %>%
    format_return_value( type_approx = "approx") %>%
    mutate ( method = ifelse ( is.na(.data$value),
                               "missing", .data$method ))

}


#' Last observation carry forward
#'
#' @param indicator A tibble created by \code{\link{get_eurostat_indicator}}.
#' @importFrom timetk tk_ts
#' @importFrom lubridate year
#' @importFrom tidyr pivot_wider
#' @importFrom dplyr select filter left_join mutate case_when
#' @import zoo
#' @return A tibble updated with the forward carried values.
#' @export
#'

na_locf <- function (indicator) {


  test_unique_observations(indicator)

  freq <- unique(indicator$frequency)

  tmp <- indicator %>%
    select ( all_of(c("time", "geo", "value"))) %>%
    pivot_wider( names_from = "geo",
                 values_from = "value")

  start_value <- lubridate::year(min(tmp$time))

  indy_ts <- timetk::tk_ts(tmp ,
                           start = start_value,
                           frequency =   case_when ( freq == "A" ~ 1,
                                                     freq == "Q" ~ 4,
                                                     freq == "M" ~ 12),
                           silent = TRUE)

  locf <- zoo::na.locf(indy_ts)

  long_form_approx <- as.data.frame (approximated) %>%
    bind_cols( tmp %>%
                 select ( all_of("time"))
    ) %>%
    pivot_longer( cols = -all_of("time"),
                  names_to = 'geo',
                  values_to = 'approx') %>%
    mutate ( method  = "locf") %>%
    filter ( !is.na("approx"))

  indicator %>%
    full_join ( anti_join (long_form_approx, indicator )  ) %>%
    format_return_value( type_approx = "locf") %>%
    mutate ( method = ifelse ( is.na(.data$value),
                               "missing", .data$method ))

}

#' Next observation carry back
#'
#' @param indicator A tibble created by \code{\link{get_eurostat_indicator}}.
#' @importFrom timetk tk_ts
#' @importFrom lubridate year
#' @importFrom tidyr pivot_wider
#' @importFrom dplyr select filter left_join mutate
#' @return A tibble updated with the values carried back.
#' @export


na_nocb <- function (indicator) {

  test_unique_observations(indicator)

  freq <- unique(indicator$frequency)

  tmp <- indicator %>%
    select ( all_of(c("time", "geo", "value"))) %>%
    pivot_wider( names_from = "geo",
                 values_from = "value")

  start_value <- lubridate::year(min(tmp$time))

  indy_ts <- timetk::tk_ts(tmp ,
                           start = start_value,
                           frequency =   case_when ( freq == "A" ~ 1,
                                                     freq == "Q" ~ 4,
                                                     freq == "M" ~ 12),
                           silent = TRUE)

  nocb <- zoo::na.locf(indy_ts, fromLast = TRUE)

  long_form_approx <- as.data.frame (approximated) %>%
    bind_cols( tmp %>%
                 select ( all_of("time"))
    ) %>%
    pivot_longer( cols = -all_of("time"),
                  names_to = 'geo',
                  values_to = 'approx') %>%
    mutate ( method  = "nocb") %>%
    filter ( !is.na("approx"))

  indicator %>%
    dplyr::full_join (dplyr::anti_join (long_form_approx, indicator )  ) %>%
    format_return_value( type_approx = "nocb") %>%
    mutate ( method = ifelse ( is.na(.data$value),
                               "missing", .data$method ))
}

#' Format returned data table
#' @param  One of \code{nocb}, \code{locf}, \code{forecast}, \code{backcast}
#' @keywords internal

format_return_value <- function( long_form_approx,
                                 type_approx = "locf") {
  tmp  <- long_form_approx %>%
    mutate ( estimate = NA_character_) %>%
    mutate ( method = ifelse(is.na(.data$value), "missing", .data$method)) %>%
    mutate ( estimate = if_else(is.na(.data$value)&!is.na(.data$approx),
                                type_approx ,
                                .data$estimate)) %>%
    mutate ( value = if_else ( estimate == type_approx,
                               .data$approx,
                               .data$value ))   %>%
    tidy_indicator()   %>%
    select ( -all_of(c("approx")) ) %>%
    select ( any_of(c("geo", "time", "value", "year", "month", "day",
                      "frequency", "estimate", "method")))

  tmp
}


#' Forecast the indicator value
#'
#' @param indicator A tibble created by \code{\link{get_eurostat_indicator}}.
#' @importFrom timetk tk_ts tk_tbl
#' @importFrom lubridate year
#' @importFrom tidyr pivot_wider
#' @importFrom dplyr select filter left_join mutate case_when
#' @importFrom sweep sw_sweep
#' @importFrom stringr str_sub
#' @importFrom forecast forecast
#' @importFrom tidyselect all_of
#' @return A tibble updated with with forecasted values.
#' @export


indicator_forecast <- function (indicator) {

  test_unique_observations(indicator)

  freq <- unique(indicator$frequency)

  tmp <- indicator %>%
    select ( all_of(c("time", "geo", "value"))) %>%
    pivot_wider( names_from = "geo",
                 values_from = "value")

  start_value <- lubridate::year(min(tmp$time))

  indy_ts <- timetk::tk_ts(tmp ,
                           start = start_value,
                           frequency =   case_when ( freq == "A" ~ 1,
                                                     freq == "Q" ~ 4,
                                                     freq == "M" ~ 12),
                           silent = TRUE)


  forecast_periods <- case_when (
    freq == "A" ~ 3,
    freq == "M" ~ 12,
    freq == "Q" ~ 5,
    freq == "D" ~ 30,
    TRUE ~ 5)

  forecasted <- forecast::forecast(indy_ts, h = forecast_periods )

  fcast_tbl <- sweep::sw_sweep(forecasted, timetk_idx = TRUE)
  methods <- sweep::sw_sweep(forecasted$method, timetk_idx = TRUE)

  ff <- timetk::tk_tbl(fcast_tbl, silent=TRUE)

  forecast_df <- ff  %>%
    set_names ( snakecase::to_snake_case(names(.))) %>%
    select ( all_of (c("time", "series", "point_forecast"))) %>%
    rename ( geo = .data$series ) %>%
    left_join ( data.frame (
      geo = names(methods),
      forecast_method = as.character(methods)
    ),
    by = 'geo' )

  if (unique(indicator$frequency)=="A") {

    forecast_df <- forecast_df %>%
      mutate ( md = str_sub(as.character(indicator$time), 5,-1)[1]) %>%
      mutate ( time = as.Date ( paste0(as.character(time), md))) %>%
      select ( -all_of("md"))
  }




   indicator_with_method  <- indicator %>%
    full_join ( anti_join (forecast_df, indicator ) ) %>%
    mutate ( method = if_else(is.na(value) & !is.na(point_forecast),
                                true = paste0("forecast_", forecast_method),
                                false = estimate  )) %>%
    rename ( approx = .data$point_forecast ) %>%
    select (-any_of("forecast_method"))

    format_return_value( long_form_approx = indicator_with_method,
                         type_approx = "forecast")
}

