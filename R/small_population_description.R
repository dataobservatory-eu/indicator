#' Labelling of the Small Countries' Population Indicator
#'
#' The labelling metadata of the indicator at source, containing the total population of three small
#' countries, Andorra, Liechtenstein and San Marino from 2010 to 2020.
#'
#' Processed with \code{\link{get_eurostat_indicator}}.
#'
#' @format A data frame with 3 rows and 10 variables:
#' \describe{
#'   \item{shortcode}{A curated short code, or the indicator_code, if not available}
#'   \item{description}{A curated description text.}
#'   \item{keyword_1}{Keyword for observatory, for example \code{"music"}}
#'   \item{keyword_2}{Keyword for pillar, for example \code{"economy"}}
#'   \item{keyword_3}{Top level division of the pillar.}
#'   \item{keyword_4}{2nd level division of the pillar.}
#'   \item{futher_keywords}{Optional further keywords as a concantenated list}
#'   \item{shortcode}{A curated short code, or the indicator_code, if not available}
#'   \item{description_at_source}{A machine-created description from source descriptive metadata}
#'   \item{indicator_code}{A machine-created unqiue indicator code.}
#'   \item{original_source}{The source, if applicable, or our observatory}
#' }
#' @source \url{https://appsso.eurostat.ec.europa.eu/nui/show.do?dataset=demo_pjan&lang=en}
#' @family data files
"small_population_description"
