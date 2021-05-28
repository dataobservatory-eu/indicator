#' Labelling of the Small Countries' Population Indicator
#'
#' The labelling metadata of the indicator at source, containing the total population of three small
#' countries, Andorra, Liechtenstein and San Marino from 2010 to 2020.
#'
#' Processed with \code{\link{get_eurostat_indicator}}.
#'
#' @format A data frame with 6 rows and 5 variables:
#' \describe{
#'   \item{shortcode}{A curated short code, or the indicator_code, if not available}
#'   \item{indicator_code}{A machine-assigned unique identifier for the indicator}
#'   \item{var_code}{The original coding of a variable at source}
#'   \item{var_label}{The original labelling of the variable at source}
#'   \item{var_name}{The name of the variable at source}
#' }
#' @source \url{https://appsso.eurostat.ec.europa.eu/nui/show.do?dataset=demo_pjan&lang=en}
#' @family data files
"small_population_labelling"
