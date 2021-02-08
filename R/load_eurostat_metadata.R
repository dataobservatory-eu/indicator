#' Load Eurostat metadata
#'
#' The function loads metadata dictionaries and the table of contents
#' into the global environment to avoid repeated requests to the Eurostat
#' data warehouse.
#'
#' @param envir The environment to load the metadata, defaults to the
#' global environment of your current R session.
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


