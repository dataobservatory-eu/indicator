#' Create Observatory Color Palette
#'
#' Create consistent palettes for visualizing data.
#'
#' @param palette_name Currently only one palette is defined, \code{"european_countries"} which
#' will create a consistent color palette for European countries.
#' @return A named vector with color codes.
#' @family visualisation functions
#' @export
#' @examples{
#' create_discrete_observatory_palette ("european_countries" )
#' }


create_discrete_observatory_palette <- function(
  palette_name = "european_countries")  {

  my_palette <- c("#007CBB", "#4EC0E4", "#00843A", "#3EA135",
                  "#DB001C", "#5C2320", "#4E115A", "#00348A",
                  "#BAC615", "#FAE000", "#E88500", "#E4007F")


  names (my_palette) <- c(
    "blue", "lightblue", "darkgreen", "green",
    "red", "brown", "violet", "darkblue",
    "lightgreen", "yellow", "orange", "magenta")

  if ( palette_name == "european_countries") {
    return_palette <- c(
      "HU" = my_palette[['green']],
      "PL" = my_palette[['red']],
      "CH" = my_palette[['red']],
      'SK' = my_palette[['lightblue']],
      'CZ' = my_palette[['blue']],
      'LV' = my_palette[['red']],
      'LT' =  my_palette[['yellow']],
      'EE' = 'black',
      'BE' = 'black',
      'NL' = my_palette[['orange']],
      'LU' = my_palette[['lightblue']],
      'NO' = my_palette[['darkblue']],
      'GB' = my_palette[['darkblue']],
      'UK' = my_palette[['darkblue']],
      'GB-G' = my_palette[['darkblue']],
      'GB-N' = my_palette[['lightblue']],
      'IE' = my_palette[['darkgreen']],
      'DE' = 'black',
      'DE-W' = 'black',
      'DE-E' = my_palette[['yellow']],
      'AT' = my_palette[['red']],
      'SI'= my_palette[['blue']],
      'FR'= my_palette[['darkblue']],
      'ES'= my_palette[['yellow']],
      'PT'= my_palette[['red']],
      'IT'= my_palette[['green']],
      'SE'= my_palette[['yellow']],
      'DK'= my_palette[['red']],
      'FI'= my_palette[['darkblue']],
      'HR' = my_palette[['red']],
      'GR'= my_palette[['lightblue']],
      'EL'= my_palette[['lightblue']],
      'RO' = my_palette[['yellow']],
      'BG'= my_palette[['green']],
      'CY'= my_palette[['orange']],
      'MT'= my_palette[['red']],
      'MK'= my_palette[['yellow']],
      'AL' = my_palette[['red']],
      'ME' = my_palette[['red']],
      'RS' = my_palette[['blue']],
      'MD' = my_palette[['darkblue']],
      'BA' = my_palette[['darkblue']],
      'XK' = 'black',
      'IS' = my_palette[['violet']],
      'EU27'= my_palette[['blue']],
      'EU27_2020'= my_palette[['blue']],
      'EU28'= my_palette[['blue']],
      'EU25'= my_palette[['blue']],
      'EU23'= my_palette[['blue']],
      'EU12'= my_palette[['blue']],
      'EU13'= my_palette[['blue']],
      'EU14'= my_palette[['blue']],
      'EU15'= my_palette[['blue']],
      'EU16'= my_palette[['blue']],
      'EU17'= my_palette[['blue']],
      'EU18'= my_palette[['blue']],
      'EU19'= my_palette[['blue']]
    )
    return_palette <- return_palette [ sort (names ( return_palette )) ]
  }


  return_palette

}


add_country_groups <- function () {

  data.frame (
    geo = c("CZ", "HU", "SK", "PL"),
    country_group = "Visegrad"
    ) %>%
    rbind ( data.frame (
      geo = c("BE", "NL", "LU"),
      country_group = "Benelux"
    ) ) %>%
    rbind (
      data.frame (
        geo = c("FI", "SE", "NO", "IS", "DK"),
        country_group = ( "Nordic")
      )
      ) %>%
    rbind (
      data.frame (
        geo = c("PT", "FR", "IT", "ES", "MT"),
        country_group = "Southwest")

      ) %>%
    rbind (
      data.frame (
        geo = c("LT", "LV", "EE"),
        country_group  = ( "Baltic")
        )
      ) %>%
    rbind (
      data.frame (
        geo = c("CH", "DE", "AT", "DE-W", "DE-E"),
        country_group  = ( "Central")
      )
    ) %>%
    rbind (
      data.frame (
        geo = c("SI", "HR", "RO", "BG", "GR", "EL",  "CY"),
        country_group  = ( "Southeast")
      )

    )  %>%
    rbind (
      data.frame (
        geo = c("AL", "XK", "ME", "BA", "RS", "MK", "TR"),
        country_group  = ( "Balkan")
      )
    ) %>%
    rbind (
      data.frame (
        geo = c("UK", "GB", "GB-B", "GB-N", "IE"),
        country_group  = ( "West")
      )
    ) %>%
    rbind (
      data.frame (
        geo = c("EU27_2020", "EU28",  "EU27", "EU25", "EU23"),
        country_group  = ( "EU")
      )
    ) %>%
    rbind (
      data.frame (
        geo = c("EA12", "EA19", "EA18", "EA17", "EA15", "EA14", "EA13"),
        country_group  = ( "Eurozone")
      )
    )


}

add_country_groups()$geo

add_country_groups()$geo [! names ( create_discrete_observatory_palette ()) %in% add_country_groups()$geo ]

