#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' @title get_country_coords
#' @description Add dates to country data
#' @param world User prompt to import shapefile.
#' Output is available through the package as "country_coords," but this function can be used to recreate this dataset.
#' To regenerate and make the data available again for the package, run the following in dev and rebuild package:
#' 1. country_coords <- get_country_coords()
#' 2. usethis::use_data(country_coords, overwrite=T)
#'
#' @importFrom magrittr `%>%`
#'
#' @export
#'
#' @examples
#' \dontrun{
#' country_coords <- get_country_coords()}
#'

get_country_coords <- function(world = file.choose()){

  df <- rgdal::readOGR(world) %>%
    sp::spTransform(sp::CRS("+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")) %>%
    sf::st_as_sf() %>%
    dplyr::select(TYPE, ADMIN, ISO_A3) %>%
    dplyr::mutate(iso3code = passport::parse_country(ADMIN, to="iso3c")) %>%
    dplyr::mutate(iso3code = dplyr::if_else(ADMIN == "eSwatini","SWZ",iso3code)) %>%
    dplyr::mutate(iso3code = dplyr::if_else(ADMIN == "Kosovo", "XKX", iso3code)) %>%
    dplyr::filter(!iso3code == "ATA" & !iso3code == 'FJI') %>% #remove Antarctica and Fiji
    dplyr::filter(!ADMIN == 'Northern Cyprus') #remove Northern Cyprus

  return(df)
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' @title get_country_populations
#' @description Get country populations
#' get_covid_df() matched with UN Estimates for 2020. Remainders matched with CIA Factbook (Guernsey, Jersey, Pitcairn Islands, Kosovo).
#' Output is available through the package as "ountry_populations," but this function can be used to recreate this dataset.
#' To regenerate and make the data available again for the package, run the following in dev and rebuild package:
#' 1. country_populations <- get_country_populations()
#' 2. usethis::use_data(country_populations, overwrite=T)
#' @importFrom magrittr `%>%`
#'
#' @export
#'
#' @examples
#' \dontrun{
#' country_populations <- get_country_populations()}

get_country_populations <- function(){

  df_un <- openxlsx::read.xlsx("https://population.un.org/wpp/Download/Files/1_Indicators%20(Standard)/EXCEL_FILES/1_Population/WPP2019_POP_F01_1_TOTAL_POPULATION_BOTH_SEXES.xlsx",
                               sheet = 1, startRow = 17) %>%
    dplyr::filter(Type == "Country/Area") %>%
    dplyr::select(un_country = 3, un_countrycode = 5, `2020`) %>%
    dplyr::mutate(`2020` = as.numeric(`2020`) * 1000)

  df_un2 <- openxlsx::read.xlsx("https://population.un.org/wpp/Download/Files/1_Indicators%20(Standard)/EXCEL_FILES/1_Population/WPP2019_POP_F08_1_TOTAL_POPULATION_BY_BROAD_AGE_GROUP_BOTH_SEXES.xlsx",
                                sheet = 1, startRow = 17) %>%
    dplyr::filter(`Reference.date.(as.of.1.July)` == 2020 & Type == "Country/Area") %>%
    dplyr::select(c(un_country = 3, un_countrycode = 5, 9, 48)) %>%
    dplyr::mutate(`Total` = as.numeric(`Total`) * 1000,
                  `18+`   = as.numeric(`18+`)   * 1000)

  df_un3 <- full_join(df_un, df_un2, by = "un_countrycode") %>%
    dplyr::left_join(
      openxlsx::read.xlsx("https://population.un.org/wpp/Download/Files/4_Metadata/WPP2019_F01_LOCATIONS.XLSX",
                          sheet = 1, startRow = 17) %>%
        select(country = 2, 4, 5),
      by = c("un_countrycode" = "Location.code")
    ) %>%
    dplyr::select(country, `ISO3.Alpha-code`, un_countrycode, `2020`, `18+`) %>%
    dplyr::add_row(country = "Guernsey",         `ISO3.Alpha-code` = "GGY", `2020` =  67334)   %>% # CIA
    dplyr::add_row(country = "Jersey",           `ISO3.Alpha-code` = "JEY", `2020` =  101476)  %>% # CIA
    dplyr::add_row(country = "Pitcairn Islands", `ISO3.Alpha-code` = "PCN", `2020` =  50)      %>% # CIA
    dplyr::add_row(country = "Kosovo",           `ISO3.Alpha-code` = "XKX", `2020` =  1935259)     # CIA

  return(df_un3)

}
