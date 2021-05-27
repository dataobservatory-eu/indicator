#' Linear approximation of missing values
#'
#' Fill missing values in a time series (or in a column of a longitudinal data set)
#' with the approximation method.
#'
#' @param indicator A tibble created by \code{\link{get_eurostat_indicator}}. with time, geo,
#' value, frequency, estimate and method columns.
#' @importFrom tidyr pivot_wider pivot_longer
#' @importFrom dplyr select filter left_join mutate bind_cols arrange
#' @importFrom zoo na.approx
#' @family approximation functions
#' @return A tibble updated with with approximated values.
#' @examples{
#' example_df <- data.frame (
#'    time = rep(as.Date (paste0(2018:2021, "-01-01")),3),
#'    geo = c( rep("NL", 4), rep("BE", 4), rep("LU", 4)),
#'    value = c(10,11, NA_real_, 12, NA_real_, 21,22,23, 5,6,7,NA_real_),
#'    frequency = rep("A", 12)
#' )
#' example_df$method <- ifelse(is.na(example_df$value), "missing", "actual")
#' example_df$estimate <- example_df$method
#'
#' na_approx ( example_df )
#' }
#' @export

na_approx <- function (indicator) {

  test_unique_observations(indicator)

  tmp <- indicator %>%
    select ( all_of(c("time", "geo", "value", "frequency"))) %>%
    dplyr::arrange (.data$time) %>%
    pivot_wider( names_from = "geo",
                 values_from = "value")

  indicator_ts <- create_time_series(tmp)

  approximated <- zoo::na.approx(indicator_ts)

  approx_df <- as.data.frame (approximated) %>% # assigned for easier debugging
    bind_cols( tmp  %>%
                 select (
                   all_of(c("time", "frequency")))
    )


  long_form_approx <- approx_df  %>%
    pivot_longer( cols = -all_of(c("time", "frequency")),
                  names_to = 'geo',
                  values_to = 'value') %>%
    left_join ( indicator %>%
                 select (
                   all_of(c("time", "geo", "estimate", "method", "frequency"))
                   ),
                by = c("time", "frequency", "geo")
               ) %>%
    mutate ( method  = ifelse(!is.na(.data$value) & .data$method=="missing",
                              "approx", .data$method),
             estimate  = ifelse(!is.na(.data$value) & .data$estimate=="missing",
                                "approx", .data$estimate))

  long_form_approx

}


#' Last observation carry forward
#'
#' Fill missing values in a time series (or in a column of a longitudional data set)
#' with the last observation carry forward method.
#'
#' @param indicator A tibble created by \code{\link{get_eurostat_indicator}}. with time, geo,
#' value, frequency, estimate and method columns.
#' @importFrom tidyr pivot_wider pivot_longer
#' @importFrom dplyr select filter left_join mutate bind_cols arrange
#' @importFrom zoo na.locf
#' @family approximation functions
#' @return A tibble updated with the forward carried values.
#' @examples{
#' example_df <- data.frame (
#'    time = rep(as.Date (paste0(2018:2021, "-01-01")),3),
#'    geo = c( rep("NL", 4), rep("BE", 4), rep("LU", 4)),
#'    value = c(10,11, NA_real_, 12, NA_real_, 21,22,23, 5,6,7,NA_real_),
#'    frequency = rep("A", 12)
#' )
#' example_df$method <- ifelse(is.na(example_df$value), "missing", "actual")
#' example_df$estimate <- example_df$method
#'
#' na_locf ( example_df )
#' }
#' @export
#'

na_locf <- function (indicator) {

  test_unique_observations(indicator)

  tmp <- indicator %>%
    select ( all_of(c("time", "geo", "value", "frequency" ))) %>%
    dplyr::arrange (.data$time) %>%
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
    mutate ( method  = ifelse(!is.na(.data$value) & .data$method=="missing",
                              "locf", .data$method),
             estimate  = ifelse(!is.na(.data$value) & .data$estimate=="missing",
                                "locf", .data$estimate))

  long_form_locf

}

#' Next observation carry back
#'
#' Fill missing values in a time series (or in a column of a longitudional data set)
#' with the next observation carry back method.
#'
#' @param indicator A tibble created by \code{\link{get_eurostat_indicator}}. with time, geo,
#' value, frequency, estimate and method columns.
#' @importFrom tidyr pivot_wider pivot_longer
#' @importFrom dplyr select filter left_join mutate bind_cols arrange
#' @importFrom zoo na.locf
#' @family approximation functions
#' @return A tibble updated with the values carried back.
#' @examples {
#' example_df <- data.frame (
#'    time = rep(as.Date (paste0(2018:2021, "-01-01")),3),
#'    geo = c( rep("NL", 4), rep("BE", 4), rep("LU", 4)),
#'    value = c(10,11, NA_real_, 12, NA_real_, 21,22,23, 5,6,7,NA_real_),
#'    frequency = rep("A", 12)
#' )
#' example_df$method <- ifelse(is.na(example_df$value), "missing", "actual")
#' example_df$estimate <- example_df$method
#'
#' na_nocb ( example_df )
#' }
#' @export


na_nocb <- function (indicator) {

  test_unique_observations(indicator)

  tmp <- indicator %>%
    select ( all_of(c("time", "geo", "value", "frequency" ))) %>%
    dplyr::arrange (.data$time) %>%
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
    mutate ( method  = ifelse(!is.na(.data$value) & .data$method=="missing",
                              "nocb", .data$method),
             estimate  = ifelse(!is.na(.data$value)& .data$estimate=="missing",
                                "nocb", .data$estimate))

  long_form_nocb
}

#' Create Time Series Object
#'
#' Create a time series object from tmp in approximation functions.
#'
#' @param tmp A temporary indicator table created by an approximation function.
#' @importFrom timetk tk_ts
#' @importFrom dplyr case_when
#' @importFrom lubridate ymd as_date
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

#' Add New Periods
#'
#' Create a time series object from tmp in approximation functions.
#'
#' @param indic A temporary indicator table created by an approximation function.
#' @param years The number of years to add to the indicator's data frame.  Positive values add after the last
#' observed time, negative values add before the first observed time.
#' @param days The number of years to add to the indicator's data frame.  Positive values add after the last
#' observed time, negative values add before the first observed time.
#' @importFrom dplyr mutate
#' @importFrom purrr set_names
#' @importFrom lubridate ymd as_date days years
#' @return A new data frame with the new observation times added with missing values, labelled as
#' \code{estimate='missing'} and \code{method='missing'}.
#' @keywords internal

add_new_periods <- function (  indic, years = NULL, days = NULL ) {

  #lubridate has no months?
  observation_time <- lubridate::as_date(indic$time)
  last_time <- max(observation_time)
  first_time  <- min(observation_time)
  freq <- unique(indic$frequency)

  if ( !is.null(years) ) {
    years <- round(years, 0)

    if (years>0) {
      new_periods <- last_time + lubridate::years(1:years)
    } else if (years<0) {
      new_periods <- first_time - lubridate::years(1:-years)
    }

    indic <- indic %>%
      dplyr::full_join (
        expand.grid(new_periods, unique(indic$geo)) %>%
          set_names (c("time", "geo")) %>%
          mutate ( frequency = freq,
                   method = 'missing',
                   estimate = 'missing'),
        by = c("time", "geo", "estimate", "frequency", "method")
        )
  }

  if ( !is.null(days) ) {
    days <- round(days, 0)
    if (days>0) {
      new_periods <- last_time + lubridate::days(1:days)
    } else if (days<0) {
      new_periods <- first_time - lubridate::days(1:-days)
    }
  }

  new_periods

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
#' @importFrom purrr possibly
#' @importFrom dplyr select filter left_join mutate case_when full_join bind_cols anti_join
#' @importFrom dplyr arrange
#' @importFrom stringr str_sub
#' @importFrom forecast forecast
#' @importFrom tidyselect all_of
#' @importFrom assertthat assert_that
#' @return A tibble updated with with forecasted values.
#' @export


indicator_forecast <- function (indicator, forecast_periods = NULL) {

  test_unique_observations(indicator)

  freq <- unique(indicator$frequency)

  if (is.null(forecast_periods)) {
    forecast_periods <- case_when (
      freq == "A" ~ 3,
      freq == "M" ~ 12,
      freq == "Q" ~ 5,
      freq == "D" ~ 30,
      TRUE ~ 5)
  }

  assertthat::assert_that(is.numeric(forecast_periods),
                          msg = "forecast_periods must be an (integer) number.")

  if (!is.null(forecast_periods)) {
    if ( freq == "A" ) {
      new_periods_df <- data.frame (
        time  = add_new_periods(indic = indicator,years = forecast_periods),
        frequency = freq)
    } else  if ( freq == "D" ) {
      new_periods_df <- data.frame (
        add_new_periods(indic = indicator,days = forecast_periods),
        frequency = freq)
    } else {
      stop ( "Forecasting for quarterly or monthly periods is not yet implemented.")
    }
  }

  assertthat::assert_that(inherits(new_periods_df, "data.frame"),
                          msg = 'new_periods_df was not created.')

  tmp <- indicator %>%
    select ( all_of(c("time", "geo", "value", "frequency" ))) %>%
    dplyr::arrange (.data$time) %>%
    pivot_wider( names_from = "geo",
                 values_from = "value")

  indicator_ts <- create_time_series(tmp)

  ## We cannot be sure that a time series can be forecasted.  In longitudional panel data
  ## some columns may yield a forecasts, others may not.

  possibly_forecast <- purrr::possibly(.f = forecast::forecast, NULL)

  forecast_per_geo <- apply (indicator_ts, 2, function(x) possibly_forecast(x, h = forecast_periods ))

  forecast_methods_geo <- lapply (forecast_per_geo, function(x) as.character(x$method))
  forecast_values_geo <- lapply (forecast_per_geo, function(x) as.numeric(x$mean))

  forecast_values_df <- as.data.frame(forecast_values_geo) %>%
    bind_cols ( new_periods_df ) %>%
    pivot_longer ( cols = -all_of(c("time", "frequency")),
                   names_to = "geo")

  forecast_methods_df <-  as.data.frame ( forecast_methods_geo  ) %>%
    pivot_longer ( everything(),
                   names_to = "geo",
                   values_to = "method")


  new_forecasted_values <- forecast_values_df %>%
    mutate ( estimate = 'forecast') %>%
    left_join (
      forecast_methods_df,
      by = "geo")   %>%
    mutate ( method   = ifelse ( is.na(.data$value), "missing", .data$method),
             estimate = ifelse ( is.na(.data$value), "missing", .data$estimate))

  indicator %>%
    dplyr::full_join (
      dplyr::anti_join (new_forecasted_values, indicator,
                        by = c("time", "frequency", "geo", "value", "estimate", "method")
                        ),
      by = c("time", "geo", "value", "estimate", "frequency", "method")
      )
}

