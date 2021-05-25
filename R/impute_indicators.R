#' Impute Indicators
#'
#' In the following order tries to fill missing values:
#'
#' 1. Approximate missing values withing time series with \code{\link{na_approx}}
#' 2. Next observation carry forward for old missing values \code{\link{na_nocb}}
#' 3. Forecast the time series ahead \code{\link{indicator_forecast}}
#' 4. If the forecast did not work, try last observation carry forward \code{\link{na_locf}}.
#'
#' @param indic An imputed indicator from \code{\link{get_eurostat_indicator}}.
#' @importFrom dplyr mutate case_when group_by ungroup
#' @importFrom tidyselect all_of
#' @importFrom tidyr nest unnest
#' @importFrom purrr possibly
#' @family approximation functions
#' @return A data frame that with updated actual, missing, approximated, forecasted, nocb, locf estimates.
#' @export

impute_indicators <- function (indic) {
  possibly_approximate <- purrr::possibly(na_approx, NULL)
  possibly_nocb <- purrr::possibly(na_nocb, NULL)
  possibly_forecast <- purrr::possibly(indicator_forecast, NULL)
  possibly_locf <- purrr::possibly(na_locf, NULL)
  
  tmp_nested <- indic %>%
    group_by (
      .data$indicator_code,
      .data$description_indicator,
      .data$db_source_code,
      .data$unit
    ) %>%
    tidyr::nest () %>%
    mutate (approx = map(.data$data, possibly_approximate)) %>%
    mutate (nocb   = map(.data$approx, possibly_nocb)) %>%
    mutate (forecasted   = map(.data$approx, possibly_forecast)) %>%
    mutate (locf   = map(.data$approx, possibly_locf))
  
  success_matrix <- data.frame(
    locf = apply (tmp_nested, 1, function(x)
      ! is.null(x$locf)),
    forecasted = apply (tmp_nested, 1, function(x)
      ! is.null(x$forecasted)),
    nocb = apply (tmp_nested, 1, function(x)
      ! is.null(x$nocb)),
    approx = apply (tmp_nested, 1, function(x)
      ! is.null(x$approx))
  )
  
  tmp_nested$final = case_when (
    !is.null(tmp_nested$locf) ~ tmp_nested$locf,!is.null(tmp_nested$forecasted) ~ tmp_nested$forecasted,!is.null(tmp_nested$nocb) ~ tmp_nested$nocb,!is.null(tmp_nested$approx) ~ tmp_nested$approx,
    TRUE ~ tmp_nested$data
  )
  
  tmp_nested %>%
    select (all_of(
      c(
        "indicator_code",
        "description_indicator",
        "db_source_code",
        "unit",
        "final"
      )
    )) %>%
    unnest (.data$final) %>%
    ungroup()
  
}
