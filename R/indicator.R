#' Indicator for an Automated Data Observatory
#'
#' The class inherits all methods from a data frame, but has
#' many fixed attributes.
#'
#' @param x A data.frame or tibble with at least \code{geo}, \code{time},
#' \code{value}, and \code{estimate} columns.
#' @param indicator_name A unique name for the indicator.
#' @param shortcode A short, unique, programatically usable, memorizable indicator
#' ID, given by an observatory curator.
#' @param indicator_code A machine-generated code. If there is no machine-generated code present,
#' it is filled with \code{shortcode}.
#' @param description A precise character string describing the indicator
#' for the data catalogue of the data observatory.
#' @param description_at_source A machine read description from the source, may require
#' manual revision.
#' @param last_update_data The creation/refreshment date of the indicator. If omitted, calls
#' \code{Sys.Date()}.
#' @param date_earliest The date of the earliest observation. Can be a numeric containing a year.
#' @param date_latest The date of the latest observation.
#' @param original_source Defaults to {NA_character_}.
#' @param code_at_source The identifier in the original source, if applicable.
#' @param keyword1 The first keywords must be on of the observatories: \code{economy},
#' \code{greendeal}, \code{music} and planned ones: \code{competition}, \code{creative}.
#' @param keyword2 The second keyword must be one of the pillars of the
#' observatory.
#' @param keyword3 The third must be a topic within a pillar.
#' @param keyword4 A search term within the topic.
#' @param keywords A character vector of any optional, further keywords.
#' @param doi A document object identifier, if exists.
#' @rdname indicator
#' @return A data.frame or tibble with indicator attributes.
#' @importFrom dplyr distinct_all
#' @import assertthat
#' @importFrom pillar pillar_shaft
#' @examples
#' test_indicator <- indicator (
#'                      x <- data.frame (
#'                      geo = rep(c("NL", "BE", "LU"), 4),
#'                      time = rep(c(2016:2019),3),
#'                      value = runif(12, 1,100),
#'                      estimate = rep("actual", 12)
#'                      ),
#'    shortcode = "observatory_test_1",
#'    description = "A test indicator with random numbers",
#'    last_update_data = as.Date ( "2020-08-24"),
#'    date_earliest  = min (x$time, na.rm=TRUE),
#'    date_latest  =  max(x$time, na.rm=TRUE),
#'    keyword1 = "music",  keyword2 = "economy",  keyword3 = "demand", keyword4="pcr"
#' )
#'
#' ## Only the first 10 observations are printed
#' print (test_indicator)

#' @export

indicator <- function(x,
                      shortcode,
                      indicator_code = NULL,
                      indicator_name = NULL,
                      description,
                      description_at_source = NA_character_,
                      last_update_data = NULL,
                      date_earliest = NULL,
                      date_latest = NULL,
                      original_source = NA_character_,
                      code_at_source = NULL,
                      doi = NULL,
                      keyword1,
                      keyword2,
                      keyword3,
                      keyword4,
                      keywords = NA_character_ ) {

  if (is.null(last_update_data)) last_update_data <- Sys.Date()

  assertthat::assert_that(keyword1 %in% c("music", "economy", "greendeal", "creative", "competition"),
                          msg = "No observatory found.")

  assertthat::assert_that(inherits(x, "data.frame"),
                          msg = "x must be a data.frame or inherited from data.frame.")

  assertthat::assert_that(all ( c("geo", "time", "value", "estimate") %in% names(x)),
                          msg = "An indicator must have at least geo, time, value and estimate columns.")
  assertthat::assert_that(inherits ( x$value, "integer")| inherits(x$value, "numeric"),
                          msg = "x$value must be a numeric or an integer.")
  assertthat::assert_that(is_time(x$time),
                          msg = "x$time must be an integer, a Date, or a numeric that can be coerced into an integer.")
  assertthat::assert_that( inherits ( x$estimate, "character")|inherits ( x$estimate, "factor"),
                          msg = "x$estimate must be a character or a factor.")
  assertthat::assert_that(inherits ( x$geo, "character")|inherits ( x$geo, "factor"),
                          msg = "x$geo must be a character or a factor.")

  assertthat::assert_that(is_unique_observations(x),
                          msg = "The values of x must be unique observations")

  assertthat::assert_that(nchar(shortcode)>1, msg = 'Shortcode is too short or missing')
  assertthat::assert_that(nchar(keyword2)>1, msg = "keyword2 is too short or missing")
  assertthat::assert_that(nchar(keyword3)>1, msg = "keyword3 is too short or missing")
  assertthat::assert_that(nchar(keyword4)>1, msg = "keyword4 is too short or missing")
  assertthat::assert_that(nchar(description)>3, msg = "description is too short or missing")

  if(is.null(doi)) doi <- "<not yet assigned or unknown>"

  if (is.null(indicator_code)) indicator_code <- shortcode
  if (is.null(indicator_name)) indicator_name <- shortcode

  if ( is.null(date_earliest)) {
    date_earliest = min(x$time, na.rm=TRUE)
  }
  if ( is.null(description)) description <- description_at_source
  if ( is.na(description)) stop("The indicator must have a non-empty description.")
  if ( is.null(description_at_source)) description_at_source <- NA_character_
  if ( is.null(indicator_code)) indicator_code <- shortcode

  if ( is.null(date_latest)) {
    date_latest = max(x$time, na.rm=TRUE)
  }

  if ( length(keywords) == 1 & is.na(keywords[1])) {
    keywords <- c(keyword1, keyword2, keyword3, keyword4)
  } else {
    keywords <- c(keyword1, keyword2, keyword3, keyword4, keywords)
  }

  if (is.na(original_source)) {
    original_source <- paste0( keyword1, ".dataobservatory.eu")
    if(is.null(code_at_source)) code_at_sorce <- original_source
  } else if(is.null(code_at_source)) code_at_sorce <- NA_character_

  new_indicator (x = x,
                 shortcode = shortcode,
                 indicator_code  = indicator_code,
                 indicator_name = indicator_name,
                 description_at_source = description_at_source,
                 description = description,
                 last_update_data = last_update_data,
                 date_earliest = date_earliest,
                 date_latest = date_latest,
                 original_source = original_source,
                 code_at_source = code_at_source,
                 doi = doi,
                 keyword1 = keyword1,
                 keyword2 = keyword2,
                 keyword3 = keyword3,
                 keyword4 = keyword4,
                 keywords = keywords )

}


#' @rdname indicator
#' @export
is.indicator <- function (x) inherits(x, "indicator")

#' @rdname indicator
#' @export
print.indicator <- function(x, ... ) {

  cat(paste0("indicator [", attr(x, "shortcode"), "] ",
  paste( c( attr(x, "keyword1"),
            attr(x, "keyword2"),
            attr(x, "keyword3")
            ),
            collapse = " - "),
  "\n", attr(x, "description")))

  cat(paste0('\nSource: ', attr(x, "observatory")), "; DOI: ", attr(x, "doi"))

  n_observations <- attr(x, "observations")

  if ( n_observations > 10 ) {
    cat (paste0("\nThe first 10 observations of ", n_observations, "\n" ))
    print(head(as.data.frame(x),10))
  } else {
    print(as.data.frame(x))
  }
}

## not exported

new_indicator <- function(x,
                          indicator_name,
                          shortcode,
                          indicator_code,
                          code_at_source,
                          description,
                          description_at_source,
                          last_update_data,
                          date_earliest,
                          date_latest,
                          observations,
                          original_source,
                          doi,
                          keyword1,
                          keyword2,
                          keyword3,
                          keyword4,
                          keywords = NA_character_ ) {

  ## This is an internal function for creating an object of
  ## class indicator.

  indicator <- x
  attr(indicator, "shortcode") <- shortcode
  attr(indicator, "indicator_code") <- indicator_code
  attr(indicator, "indicator_name") <- indicator_name
  attr(indicator, "description") <- description
  attr(indicator, "description_at_source") <- description_at_source
  attr(indicator, "code_at_source") <- description_at_source
  attr(indicator, "original_source") <- original_source
  attr(indicator, "last_update_data") <- last_update_data
  attr(indicator, "date_earliest") <- date_earliest
  attr(indicator, "date_latest") <- date_latest
  attr(indicator, "observations") <- nrow(dplyr::distinct_all ( x ))

  attr(indicator, "doi") <- doi

  attr(indicator, "keyword1") <- keyword1
  attr(indicator, "keyword2") <- keyword2
  attr(indicator, "keyword3") <- keyword3
  attr(indicator, "keyword4") <- keyword4
  attr(indicator, "keywords") <- keywords

  attr(indicator, "observatory") <-  paste0(keyword1, '.dataobservatory.eu')

  class(indicator) <- c("indicator", class(indicator) )

  indicator
}
