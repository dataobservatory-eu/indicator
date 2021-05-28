#' Population of Small Countries
#'
#' A dataset containing male and female population (all age groups) of three small
#' countries, Andorra, Liechtenstein and San Marino from 2010 to 2020.
#'
#' Processed with \code{\link{get_eurostat_indicator}}.
#'
#' @format A data frame with 99 rows and 9 variables:
#' \describe{
#'   \item{shortcode}{A curated short code, or the indicator_code, if not available.}
#'   \item{time}{Time of the observation}
#'   \item{geo}{Geographical dimension or location of the observation}
#'   \item{value}{Value of the observation}
#'   \item{estimate}{Actual, missing data, or the type of estimation used.}
#'   \item{method}{The method of estimation, if applicable.}
#'   \item{frequency}{A = annual, Q = quarterly, M = monthly, D = daily}
#'   \item{unit}{Unit of the observation or measurement}
#'   \item{description_at_source}{The original description of the observation}
#'   \item{indicator_code}{A machine-assigned unique identifier for the indicator}
#'   \item{code_at_source}{Identifier code used at original source}
#' }
#' @source \url{https://appsso.eurostat.ec.europa.eu/nui/show.do?dataset=demo_pjan&lang=en}
#' @family data files
"small_population_indicator"
