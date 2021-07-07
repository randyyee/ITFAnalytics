#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' @title get_covid_sources
#' @description Get a JHU and WHO prepared dataset.
#'
#' @importFrom magrittr `%>%`
#'
#' @export

get_covid_sources <- function(){

  df <- rbind(get_jhu_covid() %>% dplyr::mutate(data_source = "JHU"),
              get_who_covid() %>% dplyr::mutate(data_source = "WHO")) %>%
    dplyr::mutate(ou_date_src_match = paste(ou_date_match, data_source, sep="_")) %>%
    dplyr::mutate(country_code = iso3code) %>%
    dplyr::mutate(`Population 2018.x` = `pop_2020yr`) %>%
    dplyr::select(Country             = "country",
                  Date                = "date",
                  `New Cases`         = "cases_new",
                  `Cumulative Cases`  = "cases_cum",
                  `New Deaths`        = "deaths_new",
                  `Cumulative Deaths` = "deaths_cum",
                  `WHO Region`        = "who_region",
                  `Population 2020`   = "pop_2020yr",
                  `Population 2018.x`,
                  `Country Code`      = "iso3code",
                  country_code,
                  ou_date_match,
                  ou_date_src_match,
                  data_source,
                  iso2code
    ) %>%
    dplyr::mutate_if(is.numeric, ~replace(., is.na(.), 0))

  return(df)
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' @title get_jhu_covid
#' @description Get and prepare JHU data.
#'
#' @importFrom magrittr `%>%`
#'
#' @export

get_jhu_covid <- function(countries_dates){

  cases <- read.csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv", as.is=TRUE, stringsAsFactors=FALSE, check.names=FALSE)
  deaths <- read.csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv", as.is=TRUE, stringsAsFactors = FALSE, check.names=FALSE)

  if (missing(countries_dates)) {
    countries_dates <- add_country_dates(countries_data)
  }

  # Convert to Long Data
  cases <- cases %>%
    tidyr::gather("Date", "Cumulative Cases", c(5:length(names(cases)))) %>%
    dplyr::mutate(Date = as.Date(as.character(Date), format="%m/%d/%y"))

  deaths <- deaths %>%
    tidyr::gather("Date", "Cumulative Deaths", c(5:length(names(cases)))) %>%
    dplyr::mutate(Date = as.Date(as.character(Date), format="%m/%d/%y"))


  # Combine Case and Death Data
  data.long <- merge(cases, deaths, by=c("Province/State","Country/Region","Lat","Long","Date"), all=TRUE)

  # Calculate Daily New Cases and New Deaths, Then Make 0 if negative)
  data.long$Country.Province <- paste0(data.long$`Country/Region`," - ", data.long$`Province/State`)
  data.long$`New Cases`  <- ave(data.long$`Cumulative Cases`, factor(data.long$Country.Province), FUN=function(x) c(NA,diff(x)))
  data.long$`New Deaths` <- ave(data.long$`Cumulative Deaths`, factor(data.long$Country.Province), FUN=function(x) c(NA,diff(x)))
  data.long$`New Cases`[data.long$`New Cases` <0] <- 0
  data.long$`New Deaths`[data.long$`New Deaths` <0] <- 0


  # Aggregate data to the country level
  # Keep Taiwan, Hong Kong, and Macao separate from Mainland China
  data.long$Country <- data.long$`Country/Region`
  data.long$Country[data.long$`Province/State` %in% c("Hong Kong","Macau")] <- data.long$`Province/State`[data.long$`Province/State` %in% c("Hong Kong","Macau")]
  data.long$Country                                                 <- gsub("*","",data.long$Country, fixed=TRUE)

  data.countries                                                    <- aggregate(data.long[c("New Cases","New Deaths","Cumulative Cases","Cumulative Deaths")], by=list(Country=data.long$Country, Date=data.long$Date), sum)
  data.countries$Country.clean                                      <- passport::parse_country(data.countries$Country, to="en-iso")
  data.countries$Country.clean[is.na(data.countries$Country.clean)] <- data.countries$Country[is.na(data.countries$Country.clean)]
  data.countries$Country                                            <- data.countries$Country.clean
  data.countries$Country[data.countries$Country %in% c("Diamond Princess", "MS Zaandam")] <- "International Conveyance"
  data.countries <- data.countries[c("Country","Date","New Cases","Cumulative Cases","New Deaths","Cumulative Deaths")]
  data.countries <- data.countries[order(data.countries$Country, data.countries$Date),]


  df <- data.countries %>%
    dplyr::rename(
      country    = Country,
      date       = Date,
      cases_new  = `New Cases`,
      cases_cum  = `Cumulative Cases`,
      deaths_new = `New Deaths`,
      deaths_cum = `Cumulative Deaths`
    ) %>%
    dplyr::mutate(iso3code = dplyr::case_when(country == "International Conveyance" ~ "OTH",
                                              country == "Eswatini" ~passport::parse_country("Swaziland", to = "iso3c", language = c("en")),
                                              country == "Kosovo" ~ "XKX",
                                              TRUE ~passport::parse_country(country, to = "iso3c", language = c("en")))) %>%
    dplyr::filter(!is.na(iso3code)) %>%
    dplyr::select(-country) %>%
    dplyr::group_by(iso3code) %>%
    # Adding back all the first cases
    dplyr::mutate(firstcase  = dplyr::if_else(date == min(date), 1, 0)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(cases_new  = dplyr::if_else(firstcase == 1, cases_cum, as.integer(cases_new))) %>%
    dplyr::mutate(deaths_new = dplyr::if_else(firstcase == 1, deaths_cum, as.integer(deaths_new))) %>%
    dplyr::select(-firstcase) %>%
    # Sum over cruise ships
    dplyr::group_by(iso3code,date) %>%
    dplyr::summarise(cases_new  = sum(cases_new),
                     deaths_new = sum(deaths_new),
                     cases_cum  = sum(cases_cum),
                     deaths_cum = sum(deaths_cum)) %>%
    dplyr::ungroup()


  # Adding population from the World Bank and JHU
  countries_dates <- countries_dates %>%
    dplyr::filter(date <= max(df$date)) %>%
    dplyr::filter(date >= min(df$date)) %>%
    dplyr::filter(iso3code %in% unique(df$iso3code))


  # Getting the basic final dataset
  base_data <- dplyr::left_join(countries_dates, df) %>%
    dplyr::select(country,
                  date,
                  cases_new,
                  cases_cum,
                  deaths_new,
                  deaths_cum,
                  who_region,
                  pop_2020yr,
                  iso3code,
                  ou_date_match,
                  iso2code) %>%
    dplyr::mutate_if(is.numeric, ~ replace(., is.na(.), 0))

  return(base_data)
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' @title get_who_covid
#' @description Get and prepare WHO COVID data.
#' @param countries_dates Optional.
#' If not provided, will use the package provided countries_data and add_country_dates to make the most current dataset.
#' See get_countries for description of required input.
#'
#' @importFrom magrittr `%>%`
#'
#' @export

get_who_covid <- function(countries_dates){

  if (missing(countries_dates)) {
    countries_dates <- add_country_dates(countries_data)
  }

  df <- read.csv("https://covid19.who.int/WHO-COVID-19-global-data.csv", stringsAsFactors=FALSE, encoding="UTF-8") %>%
    dplyr::mutate(Country = dplyr::case_when(Country == "Kosovo[1]"                              ~"Kosovo",
                                             Country %in% c("Bonaire", "Sint Eustatius", "Saba") ~"Bonaire, Sint Eustatius, and Saba",
                                             TRUE ~Country)) %>%
    dplyr::mutate(Country_code = dplyr::case_when(Country == "Namibia"                           ~"NA",
                                                  Country == "Other"                             ~"OT",
                                                  Country == "Bonaire, Sint Eustatius, and Saba" ~"BQ",
                                                  TRUE ~Country_code)) %>%
    dplyr::rename("date" = names(.)[1]) %>%
    dplyr::group_by_if(is.character) %>%
    dplyr::summarize_all(list(~sum(., na.rm=T))) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(date = as.Date(date))%>%
    dplyr::select(iso2code = Country_code,
                  date,
                  New_cases,
                  New_deaths,
                  Cumulative_cases,
                  Cumulative_deaths)

  # Expand the time series so all countries have the same number of records
  # Create data frame with all countries and all dates
  countries_dates <- countries_dates %>%
    dplyr::filter(date <= max(df$date)) %>%
    dplyr::filter(date >= min(df$date)) %>%
    dplyr::filter(iso2code %in% unique(df$iso2code)) %>%
    dplyr::right_join(df) %>%
    dplyr::select(country,
                  date,
                  cases_new  = New_cases,
                  cases_cum  = Cumulative_cases,
                  deaths_new = New_deaths,
                  deaths_cum = Cumulative_deaths,
                  who_region,
                  pop_2020yr,
                  iso3code,
                  ou_date_match,
                  iso2code) %>%
    dplyr::mutate_if(is.numeric, ~replace(., is.na(.), 0)) %>%
    dplyr::filter(date != Sys.Date()) %>%
    # There is no Taiwan data in the WHO data set but it is in the JHU dataset
    dplyr::filter(iso3code != "TWN")


  return(countries_dates)
}
