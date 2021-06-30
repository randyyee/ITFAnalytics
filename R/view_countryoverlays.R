###################################################################################################################################

#' @title view_countryoverlays
#' @description Create dataset for country overlays using covid_sources and Google mobility data.
#'
#' @importFrom magrittr `%>%`
#'
#' @export
#'

view_countryoverlays <- function(covid_sources, googlemobility) {

  # If NCOV base dataframe is missing as input, then call the script to generate it
  if (missing(covid_sources)) {
    covid_sources <- get_covid_sources()
  }

  # If google mobility not present as input, download it from google
  if (missing(googlemobility)) {
    googlemobility <- read.csv("https://www.gstatic.com/covid19/mobility/Global_Mobility_Report.csv", encoding="UTF-8")
  }

  ncov_data <- covid_sources %>%
    dplyr::select(!`Country Code`) %>%
    dplyr::rename_all(tolower) %>%
    dplyr::rename_all(gsub, pattern=" ", replacement="_") %>%
    dplyr::mutate(new_cases = dplyr::case_when(new_cases < 0 ~ 0, new_cases >= 0 ~ new_cases),
           new_deaths = dplyr::case_when(new_deaths < 0 ~ 0, new_deaths >= 0 ~ new_deaths)) %>%
    dplyr::mutate(mort = 1000000 * new_deaths / population_2020,
           inc = 100000 * new_cases / population_2020,
           mort_cum = 1000000 * cumulative_deaths / population_2020,
           inc_cum = 100000 * cumulative_cases / population_2020) %>%
    dplyr::mutate(ou_date_match = paste(country_code, date, sep="_")) %>%
    dplyr::filter(population_2020>0) %>%
    dplyr::arrange(country, data_source, date) %>%
    dplyr::group_by(country, data_source) %>%
    dplyr::mutate(inc_ma7 = zoo::rollmean(inc, 7, align='right',fill=NA),
           mort_ma7 = zoo::rollmean(mort, 7, align='right',fill=NA)) %>%
    dplyr::filter(date >= min(date)+6) %>%
    dplyr::ungroup() %>%
    dplyr::select(date, country, country_code, ou_date_match, data_source, inc_ma7, mort_ma7) %>%
    tidyr::pivot_longer(cols=c("inc_ma7", "mort_ma7"), names_to="cases_death_type", values_to="cases_death_value")

  #Get Policy Stringency Data
  df.oxford.raw <- read.csv("https://raw.githubusercontent.com/OxCGRT/covid-policy-tracker/master/data/OxCGRT_latest.csv")
  df.oxford <- df.oxford.raw %>%
    dplyr::rename_all(tolower) %>%
    dplyr::filter(jurisdiction == "NAT_TOTAL") %>%
    dplyr::select(countryname, countrycode, date, stringencyindex) %>%
    dplyr::mutate(date = as.Date(as.character(date), "%Y%m%d")) %>%
    dplyr::mutate(ou_date_match = paste(countrycode, date, sep="_")) %>%
    dplyr::rename(country_name = countryname,
           country_code = countrycode)


  #Get Movement Data
  df.movement <- googlemobility %>%
    dplyr::rename_all(tolower) %>%
    dplyr::select_all(~gsub("_percent_change_from_baseline","",.)) %>%
    dplyr::filter(sub_region_1=="" & sub_region_2=="" & metro_area=="") %>%
    dplyr::rename(country = country_region) %>%
    dplyr::mutate(date = as.Date(date)) %>%
    dplyr::mutate(country_code = passport::parse_country(country, to="iso3c")) %>%
    dplyr::mutate(ou_date_match = paste(country_code, date, sep="_")) %>%
    dplyr::arrange(country_code, date) %>%
    dplyr::group_by(country_code) %>%
    dplyr::mutate(retail_and_recreation = zoo::rollmean(retail_and_recreation, 7, align="right", fill=NA)) %>%
    dplyr::mutate(grocery_and_pharmacy = zoo::rollmean(grocery_and_pharmacy, 7, align="right", fill=NA)) %>%
    dplyr::mutate(parks = zoo::rollmean(parks, 7, align="right", fill=NA)) %>%
    dplyr::mutate(transit_stations = zoo::rollmean(transit_stations, 7, align="right", fill=NA)) %>%
    dplyr::mutate(workplaces = zoo::rollmean(workplaces, 7, align="right", fill=NA)) %>%
    dplyr::mutate(residential = zoo::rollmean(residential, 7, align="right", fill=NA)) %>%
    dplyr::filter(date >= min(date)+6) %>%
    dplyr::ungroup() %>%
    tidyr::pivot_longer(cols=c("retail_and_recreation", "grocery_and_pharmacy", "parks", "transit_stations", "workplaces", "residential"), names_to="mobility_type", values_to="mobility_value") %>%
    dplyr::select(date, country, country_code, ou_date_match, mobility_type, mobility_value)

  overlay_list <- list("cases_deaths" = ncov_data, "stringency" = df.oxford, "mobility" = df.movement)

  return(overlay_list)
}
