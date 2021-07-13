#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' @title get_countries
#' @description (Legacy) Get countries and their continents, ISO codes, WHO region, and total population.
#' Output is available through the package as "countries_data," but this function can be used to recreate this dataset.
#' To regenerate and make the data available again for the package, run the following in dev and rebuild package:
#' 1. countries_data <- get_countries()
#' 2. usethis::use_data(countries_data, overwrite=T)
#'
#' @importFrom magrittr `%>%`
#'
#' @export
#'
#' @examples
#' \dontrun{
#' countries_data <- get_countries()}

get_countries <- function(){

  # urls
  WHO_url        <- "https://covid19.who.int/WHO-COVID-19-global-data.csv"
  continents_url <- "https://pkgstore.datahub.io/JohnSnowLabs/country-and-continent-codes-list/country-and-continent-codes-list-csv_csv/data/b7876b7f496677669644f3d1069d3121/country-and-continent-codes-list-csv_csv.csv"
  CIA_url        <- "https://www.cia.gov/the-world-factbook/field/population/country-comparison"

  # getting table with country and ISO alpha 3 codes
  iso_df <- maps::iso3166 %>%
    dplyr::filter(!is.na(a2)) %>%
    dplyr::filter(a2 != "??") %>%
    dplyr::select(iso2code = a2, iso3code = a3) %>%
    unique() %>%
    dplyr::mutate(iso2code = dplyr::if_else(iso3code == "NAM", "NA", iso2code),
                  iso3code = dplyr::if_else(iso2code == "XK",  "XKX", iso3code))


  # adding WHO regions
  who_region_df <- read.csv(WHO_url, stringsAsFactors = FALSE, encoding="UTF-8") %>%
    dplyr::mutate(Country      = dplyr::if_else(Country %in% c("Bonaire","Sint Eustatius", "Saba"), "Bonaire, Sint Eustatius, and Saba", Country)) %>%
    dplyr::mutate(Country_code = dplyr::case_when(Country == "Bonaire, Sint Eustatius, and Saba" ~"BQ",
                                                  Country == "Namibia"                           ~"NA",
                                                  TRUE ~Country_code)) %>%
    dplyr::select(Country, iso2code = Country_code, who_region = WHO_region) %>%
    unique()


  # joining ISO and WHO regions
  iso_who_df <- dplyr::full_join(iso_df, who_region_df) %>%
    dplyr::mutate(iso2code   = dplyr::case_when(who_region == "Other"        ~"OT",
                                                Country    == "Namibia"      ~"NA",
                                                TRUE ~iso2code)) %>%
    dplyr::mutate(iso3code   = dplyr::case_when(who_region == "Other"        ~"OTH",
                                                Country    == "Namibia"      ~"NAM",
                                                TRUE ~iso3code),
                  Country    = dplyr::case_when(Country    == "Kosovo[1]"    ~"Kosovo",
                                                iso3code   == "TWN"          ~"Taiwan",
                                                iso3code   == "HKG"          ~"Hong Kong",
                                                iso3code   == "MAC"          ~"Macau",
                                                iso3code   == "ESH"          ~"Western Sahara",
                                                TRUE ~Country),
                  who_region = dplyr::case_when(iso3code %in% c("HKG","MAC") ~"WPRO",
                                                iso3code %in% c("TWN","ESH") ~"Other",
                                                TRUE ~who_region)) %>%
    dplyr::filter(!is.na(who_region))


  # adding Continents
  # note: some countries have multiple continents - they are all in the WHO Euro region, so remove the rows where they have Asia as the continent
  continents_df <- read.csv(continents_url, stringsAsFactors = FALSE, encoding="UTF-8") %>%
    dplyr::select(Continent_Name,
                  iso2code = Two_Letter_Country_Code,
                  iso3code = Three_Letter_Country_Code) %>%
    dplyr::mutate(iso2code = dplyr::if_else(iso3code == "NAM", "NA", iso2code)) %>%
    dplyr::filter(!(iso3code %in% c("ARM","AZE","CYP","GEO","KAZ","RUS","TUR") & Continent_Name == "Asia"))


  # adding population totals from CIA
  # note: manually add in North Macedonia
  totalpop_df <- CIA_url %>%
    rvest::read_html() %>%
    rvest::html_nodes(xpath='//*[@class="content-table table-auto"]') %>%
    rvest::html_table() %>%
    as.data.frame() %>%
    dplyr::mutate(pop_2020yr = as.numeric(stringr::str_replace_all(Var.3, ",", ""))) %>%
    dplyr::mutate(iso3code = dplyr::case_when(Country == "Eswatini" ~ passport::parse_country("Swaziland", to = "iso3c", language = c("en")),
                                              Country == "Kosovo"   ~ "XKX",
                                              TRUE ~ passport::parse_country(Country, to = "iso3c", language = c("en")))) %>%
    dplyr::select(iso3code, pop_2020yr) %>%
    dplyr::group_by(iso3code) %>%
    dplyr::summarise_all(list(~sum(., na.rm = T))) %>%
    dplyr::ungroup() %>%
    dplyr::bind_rows(data.frame(iso3code = "MKD", pop_2020yr = 2128262))


  # final joins
  df <- dplyr::left_join(dplyr::left_join(iso_who_df, totalpop_df), continents_df) %>%
    dplyr::mutate(Continent_Name = dplyr::case_when(Country == "Kosovo" ~"Europe",
                                                    Country == "Other"  ~"Other",
                                                    TRUE ~Continent_Name)) %>%
    dplyr::select(country = Country, iso3code, iso2code, who_region, pop_2020yr, Continent_Name) %>%
    unique()

  return(df)
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' @title add_country_dates
#' @description (Legacy) Add dates to country data
#' @param countries_df Optional.
#' If not provided, will use the package provided countries_data.
#' See get_countries for description of required input.
#'
#' @importFrom magrittr `%>%`
#'
#' @export
#'
#' @examples
#' \dontrun{
#' countries_with_dates <- add_country_dates(countries_data)}

add_country_dates <- function(countries_df){

  if(missing(countries_df)){
    countries_df <- countries_data
  }

  start_date <- as.Date("2020-01-01")
  end_date   <- as.Date(Sys.Date())
  all_dates  <- seq(from = start_date, to = end_date, by = 1)

  # Joining countries and dates to get Cartesian product
  df <- merge.data.frame(countries_df, all_dates, by = NULL) %>%
    dplyr::mutate(ou_date_match = paste(iso3code, y, sep="_")) %>%
    dplyr::select(ou_date_match, country, date = y, iso3code, iso2code, who_region, pop_2020yr, Continent_Name) %>%
    unique()

  return(df)
}


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
    dplyr::mutate(iso3code = dplyr::if_else(ADMIN == "Kosovo","XKX",iso3code)) %>%
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
#' get_country_populations <- get_country_populations()}

get_country_populations <- function(){

  df <- openxlsx::read.xlsx("https://population.un.org/wpp/Download/Files/1_Indicators%20(Standard)/EXCEL_FILES/1_Population/WPP2019_POP_F01_1_TOTAL_POPULATION_BOTH_SEXES.xlsx",
                            sheet = 1, startRow = 17) %>%
    dplyr::filter(Type == "Country/Area") %>%
    dplyr::select(country = `Region,.subregion,.country.or.area.*`, `2020`) %>%
    dplyr::mutate(`2020` = as.numeric(`2020`) * 1000) %>%
    dplyr::mutate(country = dplyr::recode(country,
                            "Bonaire, Sint Eustatius and Saba" = "Bonaire, Sint Eustatius, and Saba",
                            "Micronesia (Fed. States of)"      = "Micronesia (Federated States of)",
                            "Côte d'Ivoire"                    = "Cote d'Ivoire",
                            "United Kingdom"                   = "The United Kingdom",
                            "Dem. People's Republic of Korea"  = "Democratic People's Republic of Korea",
                            "Saint Martin (French part)"       = "Saint Martin",
                            "Northern Mariana Islands"         = "Northern Mariana Islands (Commonwealth of the)",
                            "State of Palestine"               = "occupied Palestinian territory, including east Jerusalem",
                            "Sint Maarten (Dutch part)"        = "Sint Maarten",
                            "Wallis and Futuna Islands"        = "Wallis and Futuna",
                            "China, Hong Kong SAR"             = "Hong Kong",
                            "China, Taiwan Province of China"  = "Taiwan",
                            "China, Macao SAR"                 = "Macau")) %>%
    dplyr::add_row(country = "Guernsey",         `2020` =  67334)  %>% # CIA
    dplyr::add_row(country = "Jersey",           `2020` =  101476) %>%
    dplyr::add_row(country = "Pitcairn Islands", `2020` =  50)     %>%
    dplyr::add_row(country = "Kosovo",           `2020` =  1935259)

  #test <- left_join(who_list, df)

  return(df)

}
