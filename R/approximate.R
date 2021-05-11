#' Test Unique Observations
#'
#' Approximation and other filling techniques require unique observations
#' @param indicator An indicator table to test.
#' @importFrom dplyr select group_by add_count filter
#' @return \code{TRUE} if the test is met, otherwise and error message.
#' @export

test_unique_observations <- function( indicator ) {

  uniqueness <- indicator %>%
    dplyr::select ( all_of(c("geo", "time", "value", "estimate")) ) %>%
    dplyr::group_by (  .data$geo, .data$time, .data$value ) %>%
    add_count() %>%
    filter ( n != 1 )

  if (nrow(uniqueness)==0) return(TRUE) else {
    stop ( uniqueness,  "\n is not unique. This is an error." )
  }
}

#' Create Time Series Object
#'
#' Create a time series object from tmp in approximation functions.
#' @param tmp A temporary indicator table created by an approximation function.
#' @importFrom timetk tk_ts
#' @importFrom dplyr case_when
#' @importFrom lubridate ymd
#' @importFrom glue glue
#' @return \code{TRUE} if the test is met, otherwise and error message.
#' @keywords internal

create_time_series <- function( tmp ) {

  freq <- unique(tmp$frequency)

  assertthat::assert_that(length(freq)==1,
                          msg =  glue::glue( "There are several frequency types found: {freq}. This is an error.") )

  start_value <- lubridate::ymd(min(tmp$time))

  timetk::tk_ts(tmp ,
                start = start_value,
                frequency =   case_when ( freq == "A" ~ 1,
                                          freq == "Q" ~ 4,
                                          freq == "M" ~ 12),
                silent = TRUE)
}


#' Linear approximation of missing values
#'
#'
#'
#' @param indicator A tibble created by \code{\link{get_eurostat_indicator}}. with time, geo,
#' value, frequency, estimate and method columns.
#' @importFrom tidyr pivot_wider pivot_longer
#' @importFrom dplyr select filter left_join mutate bind_cols
#' @importFrom zoo na.approx
#' @return A tibble updated with with approximated values.
#' @export

na_approx <- function (indicator) {

  test_unique_observations(indicator)

  tmp <- indicator %>%
    select ( all_of(c("time", "geo", "value", "frequency" ))) %>%
    pivot_wider( names_from = "geo",
                 values_from = "value")

  indicator_ts <- create_time_series(tmp)

  approximated <- zoo::na.approx(indicator_ts)

  long_form_approx <- as.data.frame (approximated) %>% # assinged for easier debugging
    bind_cols( tmp  %>%
                 select (
                   all_of(c("time", "frequency")))
    )   %>%
    pivot_longer( cols = -all_of(c("time", "frequency")),
                  names_to = 'geo',
                  values_to = 'value') %>%
    left_join ( indicator %>%
                 select (
                   all_of(c("time", "geo", "estimate", "method", "frequency"))
                   ),
                by = c("time", "frequency", "geo")
               )   %>%
    mutate ( method  = ifelse(!is.na(value)& method=="missing",
                              "approx", method))  %>%
    mutate ( estimate  = ifelse(!is.na(value)& estimate=="missing",
                              "approx", estimate))

  nrow ( long_form_approx ) == nrow( indicator )

  test_unique_observations(long_form_approx )

  long_form_approx

}


#' Last observation carry forward
#'
#' @param indicator A tibble created by \code{\link{get_eurostat_indicator}}. with time, geo,
#' value, frequency, estimate and method columns.
#' @importFrom tidyr pivot_wider pivot_longer
#' @importFrom dplyr select filter left_join mutate bind_cols
#' @importFrom zoo na.locf
#' @return A tibble updated with the forward carried values.
#' @export
#'

na_locf <- function (indicator) {

  test_unique_observations(indicator)

  tmp <- indicator %>%
    select ( all_of(c("time", "geo", "value", "frequency" ))) %>%
    pivot_wider( names_from = "geo",
                 values_from = "value")

  indicator_ts <- create_time_series(tmp)
  locf <- zoo::na.locf(indicator_ts)

  long_form_locf <- as.data.frame (locf) %>% # assigned for easier debugging
    bind_cols( tmp  %>%
                 select (
                   all_of(c("time", "frequency")))
    )   %>%
    pivot_longer( cols = -all_of(c("time", "frequency")),
                  names_to = 'geo',
                  values_to = 'value') %>%
    left_join ( indicator %>%
                  select (
                    all_of(c("time", "geo", "estimate", "method", "frequency"))
                  ),
                by = c("time", "frequency", "geo")
    )   %>%
    mutate ( method  = ifelse(!is.na(value)& method=="missing",
                              "locf", method))  %>%
    mutate ( estimate  = ifelse(!is.na(value)& estimate=="missing",
                                "locf", estimate))

  nrow ( long_form_locf ) == nrow( indicator )

  test_unique_observations(long_form_locf )

  long_form_locf

}

#' Next observation carry back
#'
#' @param indicator A tibble created by \code{\link{get_eurostat_indicator}}. with time, geo,
#' value, frequency, estimate and method columns.
#' @importFrom tidyr pivot_wider pivot_longer
#' @importFrom dplyr select filter left_join mutate bind_cols
#' @importFrom zoo na.locf
#' @return A tibble updated with the values carried back.
#' @export


na_nocb <- function (indicator) {

  test_unique_observations(indicator)

  tmp <- indicator %>%
    select ( all_of(c("time", "geo", "value", "frequency" ))) %>%
    pivot_wider( names_from = "geo",
                 values_from = "value")

  indicator_ts <- create_time_series(tmp)
  nocb <- zoo::na.locf(indicator_ts, fromLast = TRUE)

  long_form_nocb <- as.data.frame (nocb) %>% # assigned for easier debugging
    bind_cols( tmp  %>%
                 select (
                   all_of(c("time", "frequency")))
    )   %>%
    pivot_longer( cols = -all_of(c("time", "frequency")),
                  names_to = 'geo',
                  values_to = 'value') %>%
    left_join ( indicator %>%
                  select (
                    all_of(c("time", "geo", "estimate", "method", "frequency"))
                  ),
                by = c("time", "frequency", "geo")
    )   %>%
    mutate ( method  = ifelse(!is.na(value)& method=="missing",
                              "nocb", method))  %>%
    mutate ( estimate  = ifelse(!is.na(value)& estimate=="missing",
                                "nocb", estimate))

  nrow ( long_form_nocb ) == nrow( indicator )

  test_unique_observations(long_form_nocb )

  long_form_nocb
}

#' Format returned data table
#' @param  One of \code{nocb}, \code{locf}, \code{forecast}, \code{backcast}
#' @keywords internal

format_return_value <- function( long_form_approx,
                                 type_approx = "locf") {


  message ( "This function is no longer needed.")
  return(NULL)
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
#' @param indicator A tibble created by \code{\link{get_eurostat_indicator}}. with time, geo,
#' value, frequency, estimate and method columns.
#' @param forecast_periods The number of expected forecasts.  If set to default \code{NULL},
#' annual indicators will be forecasted to 3 periods, quarterly to 5 periods,
#' monthly to 12 periods and daily to 30 periods.
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


indicator_forecast <- function (indicator, forecast_periods = NULL) {

  test_unique_observations(indicator)

  message ("this should be rewritten")
  return(NULL)

  tmp <- indicator %>%
    select ( all_of(c("time", "geo", "value", "frequency" ))) %>%
    pivot_wider( names_from = "geo",
                 values_from = "value")

  indicator_ts <- create_time_series(tmp)

  if (is.null(forecast_periods)) {
    forecast_periods <- case_when (
      freq == "A" ~ 3,
      freq == "M" ~ 12,
      freq == "Q" ~ 5,
      freq == "D" ~ 30,
      TRUE ~ 5)
  }

  indicator$time

  forecasted <- forecast::forecast(indicator_ts, h = forecast_periods )

  fcast_tbl <- sweep::sw_sweep(forecasted$forecast, timetk_idx = TRUE)
  methods <- sweep::sw_sweep(forecasted$method, timetk_idx = TRUE)

  forecasted_values <-   matrix (
    rep(NA_real_, length(fcast_tbl)*forecast_periods),
    nrow = forecast_periods ) %>%
    as.data.frame() %>%
    purrr::set_names ( names(fcast_tbl) )

  sapply ( 1:3, function(x) fcast_tbl[[x]]$x )

  as.zoo(fcast_tbl[[1]]$x)


  ff <- timetk::tk_tbl(fcast_tbl, silent=TRUE)

  forecasted$forecast

  as.Date(as.integer(ff$Time))

  forecast_df <- ff  %>%
    set_names ( snakecase::to_snake_case(names(.))) %>%
    select ( all_of (c("time", "series", "point_forecast"))) %>%
    rename ( geo = .data$series ) %>%
    mutate ( time = as.Date(time))
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

