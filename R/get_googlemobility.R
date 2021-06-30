###################################################################################################################################

#' @title get_googlemobility
#' @description Get Google Mobility.
#'
#' @importFrom magrittr `%>%`
#'
#' @export
#'
#' @examples
#' \dontrun{
#' mobility_df <- get_googlemobility()}

get_googlemobility <- function() {

  # Pulling in the Google mobility data
  gmob <- read.csv("https://www.gstatic.com/covid19/mobility/Global_Mobility_Report.csv", encoding="UTF-8")

  df <- gmob %>%
    dplyr::filter(sub_region_1 == "") %>%
    dplyr::mutate(country_region = dplyr::if_else(country_region == "Namibia", "NA", country_region),
                  Date           = as.Date(date)) %>%
    dplyr::select(Date,
                  country_region,
                  country_region_code,
                  retail_and_recreation_percent_change_from_baseline:residential_percent_change_from_baseline) %>%
    dplyr::rename(iso2code = country_region_code) %>%
    dplyr::left_join(dplyr::select(countries_data, iso2code, iso3code) %>% unique()) %>%
    dplyr::mutate(ou_date_match = paste(iso3code, Date, sep = "_"))

  return(list(gmob, df))
}
