#' Linear approximation of missing values
#'
#' @param ids Identifiers of Eurostat statistical products.
#' @param path A path to save the database.
#' @importFrom timetk tk_ts
#' @importFrom lubridate year
#' @importFrom tidyr pivot_wider
#' @importFrom dplyr select filter left_join mutate case_when
#' @importFrom zoo na.approx
#' @return A tibble updated with with approximated values.
#' @export


na_approx <- function (indicator) {


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

  approximated <- zoo::na.approx(indy_ts)

  long_form_approx <- as.data.frame (approximated) %>%
    bind_cols( tmp %>%
                 select ( all_of("time"))
               ) %>%
    pivot_longer( cols = -all_of("time"),
                  names_to = 'geo',
                  values_to = 'approx') %>%
    left_join ( tmp %>% pivot_longer (cols = -all_of("time"),
                                      names_to = 'geo',
                                      values_to = 'value' ),
                by  = c('geo', 'time') )

  indicator %>%
    left_join ( long_form_approx, by = c("geo", "time", "value") ) %>%
    mutate ( validate = if_else(is.na(.data$value)&!is.na(.data$approx),
                                "approx",
                                .data$validate)) %>%
    mutate ( value = if_else ( validate == "approx",
                               .data$approx, .data$value )) %>%
    select ( -all_of("approx"))

}


#' Last observation carry forward
#'
#' @param indicator A tibble created by \code{get_eurostat_indicator}.
#' @importFrom timetk tk_ts
#' @importFrom lubridate year
#' @importFrom tidyr pivot_wider
#' @importFrom dplyr select filter left_join mutate case_when
#' @importFrom zoo na.lofc
#' @return A tibble updated with the forward carried values.
#' @export

na_locf <- function (indicator) {


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

  long_form_approx <- as.data.frame (locf) %>%
    bind_cols( tmp %>%
                 select ( all_of("time"))
    ) %>%
    pivot_longer( cols = -all_of("time"),
                  names_to = 'geo',
                  values_to = 'locf') %>%
    left_join ( tmp %>% pivot_longer (cols = -all_of("time"),
                                      names_to = 'geo',
                                      values_to = 'value' ),
                by  = c('geo', 'time') )

  indicator %>%
    left_join ( long_form_approx, by = c("geo", "time", "value") ) %>%
    mutate ( validate = if_else(is.na(.data$value)&!is.na(.data$locf),
                                "locf",
                                validate)) %>%
    mutate ( value = if_else ( validate == "locf",
                               .data$locf, .data$value )) %>%
    select ( -all_of("locf"))

}

#' Next observation carry back
#'
#' @param indicator A tibble created by \code{get_eurostat_indicator}.
#' @importFrom timetk tk_ts
#' @importFrom lubridate year
#' @importFrom tidyr pivot_wider
#' @importFrom dplyr select filter left_join mutate
#' @importFrom zoo na.lofc
#' @return A tibble updated with the values carried back.
#' @export


na_nocb <- function (indicator) {


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

  long_form_approx <- as.data.frame (locf) %>%
    bind_cols( tmp %>%
                 select ( all_of("time"))
    ) %>%
    pivot_longer( cols = -all_of("time"),
                  names_to = 'geo',
                  values_to = 'nocb') %>%
    left_join ( tmp %>% pivot_longer (cols = -all_of("time"),
                                      names_to = 'geo',
                                      values_to = 'value' ),
                by  = c('geo', 'time') )

  indicator %>%
    left_join ( long_form_approx, by = c("geo", "time", "value") ) %>%
    mutate ( validate = if_else(is.na(.data$value)&!is.na(.data$nocb),
                                "nocb",
                                validate)) %>%
    mutate ( value = if_else ( validate == "nocb",
                               .data$nocb, .data$value )) %>%
    select ( -all_of("nocb"))

}


#' Forecast the indicator value
#'
#' @param indicator A tibble created by \code{get_eurostat_indicator}.
#' @importFrom timetk tk_ts tk_tbl
#' @importFrom lubridate year
#' @importFrom tidyr pivot_wider
#' @importFrom dplyr select filter left_join mutate case_when
#' @importFrom sweep sw_sweep
#' @importFrom forecast forecast
#' @importFrom tidyselect all_of
#' @return A tibble updated with with forecasted values.
#' @export


indicator_forecast <- function (indicator) {


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

  forecasted <- forecast::forecast(indy_ts)


  fcast_tbl <- sweep::sw_sweep(forecasted, timetk_idx = TRUE)
  methods <- sweep::sw_sweep(forecasted$method, timetk_idx = TRUE)

  ff <- timetk::tk_tbl(fcast_tbl, silent=TRUE)

  forecast_df <- ff  %>% set_names ( snakecase::to_snake_case(names(.))) %>%
    select ( all_of (c("time", "series", "point_forecast"))) %>%
    rename ( geo = .data$series ) %>%
    left_join ( data.frame ( geo = names(methods),
                             forecast_method = as.character(methods) ),
                by = 'geo' )

  if (unique(indicator$frequency)=="A") {

    forecast_df <- forecast_df %>%
      mutate ( md = stringr::str_sub(as.character(indicator$time), 5,-1)[1]) %>%
      mutate ( time = as.Date ( paste0(as.character(time), md))) %>%
      select ( -all_of("md"))
  }

  indicator %>%
    left_join ( forecast_df, by = c("geo", "time") ) %>%
    mutate ( validate = if_else(is.na(value) & !is.na(point_forecast),
                             true = paste0("forecast_", forecast_method),
                             false = validate )) %>%
    mutate ( value = if_else(is.na(value) & !is.na(point_forecast),
                             true = point_forecast,
                             false = value ) ) %>%
    select (-all_of(c("forecast_method", "point_forecast")))



}

