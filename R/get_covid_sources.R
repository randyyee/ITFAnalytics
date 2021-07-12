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

  #Convert to Long Data
  cases.long <- cases %>%
    tidyr::gather("Date.Orig", "Cumulative Cases", c(5:length(names(cases))))

  deaths.long <- deaths %>%
    tidyr::gather("Date.Orig", "Cumulative Deaths", c(5:length(names(cases))))


  #Clean up Date and Time
  cases.long$Date <- as.Date(as.character(cases.long$Date.Orig), format="%m/%d/%y")
  deaths.long$Date <- as.Date(as.character(deaths.long$Date.Orig), format="%m/%d/%y")

  #Combine Case and Death Data
  data.long <- merge(cases.long, deaths.long, by=c("Province/State","Country/Region","Lat","Long","Date"), all=TRUE)
  # Remove the redundant date column
  data.long[c("Date.Orig.x","Date.Orig.y")] <- NULL

  # Calculate Daily New Cases and New Deaths, Then Make 0 if negative)
  data.long$Country.Province <- paste0(data.long$`Country/Region`," - ", data.long$`Province/State`)
  data.long$`New Cases`  <- ave(data.long$`Cumulative Cases`, factor(data.long$Country.Province), FUN=function(x) c(NA,diff(x)))
  data.long$`New Deaths` <- ave(data.long$`Cumulative Deaths`, factor(data.long$Country.Province), FUN=function(x) c(NA,diff(x)))
  data.long$`New Cases`[data.long$`New Cases`   <0]   <- 0
  data.long$`New Deaths`[data.long$`New Deaths` <0]   <- 0


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
    dplyr::rename(country    = Country,
                  date       = Date,
                  cases_new  = `New Cases`,
                  cases_cum  = `Cumulative Cases`,
                  deaths_new = `New Deaths`,
                  deaths_cum = `Cumulative Deaths`) %>%
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


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' @title get_covid_df
#' @description Get and prepare COVID data.
#'
#' @importFrom magrittr `%>%`
#'
#' @export

get_covid_df <- function(){

  df <- read.csv("https://covid19.who.int/WHO-COVID-19-global-data.csv", stringsAsFactors=FALSE, encoding="UTF-8") %>%
    dplyr::rename_all(tolower) %>%
    dplyr::mutate(country = dplyr::case_when(country == "Kosovo[1]"                              ~"Kosovo",
                                             country %in% c("Bonaire", "Sint Eustatius", "Saba") ~"Bonaire, Sint Eustatius, and Saba",
                                             TRUE ~country)) %>%
    dplyr::mutate(country_code = dplyr::case_when(country == "Namibia"                           ~"NA",
                                                  country == "Other"                             ~"OT",
                                                  country == "Bonaire, Sint Eustatius, and Saba" ~"BQ",
                                                  TRUE ~country_code)) %>%
    dplyr::rename("date" = names(.)[1]) %>%
    dplyr::group_by_if(is.character) %>%
    dplyr::summarize_all(list(~sum(., na.rm=T))) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(date = as.Date(date),
                  source = "WHO") %>%

    dplyr::bind_rows(
      read.csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv",
               as.is=TRUE, stringsAsFactors=FALSE, check.names=FALSE) %>%
        dplyr::rename_all(tolower) %>%
        dplyr::filter(`country/region` %in% c("Taiwan*", "China")) %>%
        dplyr::mutate(`country/region` = dplyr::case_when(`province/state` == "Hong Kong" ~ "Hong Kong",
                                                          `province/state` == "Macau"     ~ "Macau",
                                                          TRUE ~ `country/region`)) %>%
        dplyr::select(-lat,-long) %>%
        dplyr::group_by(`country/region`) %>%
        dplyr::summarise_if(is.numeric, sum, na.rm = TRUE) %>%
        dplyr::ungroup() %>%
        tidyr::pivot_longer(cols = where(is.numeric), names_to = "date", values_to = "cumulative_cases") %>%
        dplyr::mutate(date             = lubridate::mdy(date)) %>%
        dplyr::mutate(`country/region` = dplyr::recode(`country/region`, "Taiwan*" = "Taiwan")) %>%
        dplyr::group_by(`country/region`) %>%
        dplyr::mutate(new_cases        = cumulative_cases - dplyr::lag(cumulative_cases)) %>%
        dplyr::ungroup() %>%
        dplyr::left_join(
          read.csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv",
                   as.is=TRUE, stringsAsFactors = FALSE, check.names=FALSE) %>%
            dplyr::rename_all(tolower) %>%
            dplyr::filter(`country/region` %in% c("Taiwan*", "China")) %>%
            dplyr::mutate(`country/region` = dplyr::case_when(`province/state` == "Hong Kong" ~ "Hong Kong",
                                                              `province/state` == "Macau"     ~ "Macau",
                                                              TRUE ~ `country/region`)) %>%
            dplyr::select(-lat,-long) %>%
            dplyr::group_by(`country/region`) %>%
            dplyr::summarise_if(is.numeric, sum, na.rm = TRUE) %>%
            dplyr::ungroup() %>%
            tidyr::pivot_longer(cols = where(is.numeric), names_to = "date", values_to = "cumulative_deaths") %>%
            dplyr::mutate(date             = lubridate::mdy(date)) %>%
            dplyr::mutate(`country/region` = dplyr::recode(`country/region`, "Taiwan*" = "Taiwan")) %>%
            dplyr::group_by(`country/region`) %>%
            dplyr::mutate(new_cases        = cumulative_deaths - dplyr::lag(cumulative_deaths)) %>%
            dplyr::ungroup()
        ) %>%
        dplyr::rename(country = `country/region`) %>%
        dplyr::mutate(who_region = "WPRO",
                      country_code = dplyr::case_when(country == "China"     ~"CN",
                                                      country == "Taiwan"    ~"TW",
                                                      country == "Hong Kong" ~"HK",
                                                      country == "Macau"     ~"MO"),
                      source = "JHU")%>%
        dplyr::arrange(country, date)
    ) %>%
    dplyr::mutate(country    = dplyr::recode(country, "Côte d’Ivoire" = "Cote d'Ivoire")) %>%
    dplyr::mutate(who_region = factor(who_region, levels= c("AMRO","EURO","SEARO","EMRO","AFRO","WPRO"))) %>%
    dplyr::mutate(region     = dplyr::case_when(who_region == "AMRO"  ~"Americas",
                                                who_region == "EURO"  ~"Europe",
                                                who_region == "SEARO" ~"Southeast Asia",
                                                who_region == "EMRO"  ~"Eastern Mediterranean",
                                                who_region == "AFRO"  ~"Africa",
                                                who_region == "WPRO"  ~"Western Pacific"),
                  region     = factor(region, levels= c("Americas","Europe","Southeast Asia","Eastern Mediterranean","Africa","Western Pacific")))

}
