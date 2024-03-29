#' Load Eurostat metadata
#'
#' The function downloads and saves the Eurostat table of contents and
#' code dictionaries to the temporary directory of the session, if it does
#' not already exist there; then it loads them to the requested environment
#' to avoid repeated requests to the Eurostat data warehouse.
#'
#' @param envir The environment to load the metadata, defaults to the
#' global environment of your current R session.
#' @param path The path where you want to store the Eurostat table of contents and
#' dictionaries, defaults to \code{tempdir()}.
#' @importFrom eurostat get_eurostat_dic get_eurostat_toc
#' @importFrom dplyr filter relocate
#' @importFrom rlang .data
#' @export

load_eurostat_metadata <- function ( envir = globalenv(),
                                     path = tempdir() ) {

  if ( file.exists(
    file.path(tempdir(), "eurostat_metadata.rda"))
  ) {
   load ( file.path(tempdir(), "eurostat_metadata.rda") )
  } else {
    var_labels   <- eurostat::get_eurostat_dic ( "dimlst",    lang = "en" )
    value_labels <- eurostat::get_eurostat_dic ( "table_dic", lang = "en" )

    indic_dict <- var_labels %>%
      dplyr::filter ( grepl("indicator", .data$full_name ))

    eurostat_toc <- eurostat::get_eurostat_toc()

    save ( eurostat_toc, indic_dict, var_labels, value_labels,
           file = file.path(tempdir(), "eurostat_metadata.rda")
    )
  }

  if ( !is.null(envir) ) {
    assign ( "var_labels",   var_labels, envir )
    assign ( "indic_dict",   indic_dict, envir )
    assign ( "value_labels", value_labels, envir )
    assign ( "eurostat_toc", eurostat_toc, envir )
  }


}


