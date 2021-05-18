#' Add Keywords
#'
#' Keywords help place the indicator in the long-form documentation ("pillar" or "chapter")
#' of the observatory.
#'
#' 1st keyword: which observatory, for example \code{"music"}
#' 2nd keyword: which pillar, for example \code{"economy"}
#' 3rd keyword: top level division of the pillar.
#' 4th keyword: 2nd level division of the pillar.
#'
#' Further keywords are added to \code{further_keywords} as a concantenated list.
#'
#' @param metadata A metadata table from \code{\link{get_eurostat_indicator}}.
#' @param keywords A list of at least four keywords to place to indicator in the observatory.
#' @importFrom lubridate day month year
#' @importFrom dplyr mutate select ungroup
#' @importFrom assertthat assert_that
#' @importFrom tidyselect all_of
#' @return A data frame of indicator codes, descriptions, original source codes, and at least keywords to place them in the observatory.
#' @family metadata functions
#' @export
#'

add_keywords <- function ( metadata, keywords) {
  assertthat::assert_that( is.list(keywords),
                           msg = "keywords must be keywords in a list object.")
  assertthat::assert_that( length(keywords) >= 4,
                           msg = "We need at least 4 keywords.")

  if ( length(keywords)>4) {
    further_keywords <- paste (sapply ( 5:length(keywords), function(x) keywords[[x]]), collapse = "__")
  } else {
    further_keywords <- NA_character_
  }

  metadata %>%
    ungroup() %>%
    select ( all_of(c("indicator_code", "description_indicator", "db_source_code"))) %>%
    mutate ( keyword_1 = keywords[[1]],
             keyword_2 = keywords[[2]],
             keyword_3 = keywords[[3]],
             keyword_4 = keywords[[4]],
             further_keywords = further_keywords )

}
