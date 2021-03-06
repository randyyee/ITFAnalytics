#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' @title get_vax
#' @description Get vaccinations from OWID
#'
#' @importFrom magrittr `%>%`
#'
#' @export
#'

get_vax <- function() {

  df <- read.csv("https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/vaccinations/vaccinations.csv",
                 as.is = TRUE,
                 stringsAsFactors = FALSE,
                 check.names = FALSE) %>%
    dplyr::mutate(date = as.Date(date)) %>%
    dplyr::mutate(iso_code = dplyr::recode(iso_code, "OWID_KOS" = "XKX")) %>%
    dplyr::filter(!grepl("OWID", iso_code)) %>%
    dplyr::mutate(location = dplyr::recode(location,
                             "United Kingdom"            = "The United Kingdom",
                             "Syria"                     = "Syrian Arab Republic",
                             "South Korea"               = "Republic of Korea",
                             "Sint Maarten (Dutch part)" = "Sint Maarten",
                             "Russia"                    = "Russian Federation",
                             "Pitcairn"                  = "Pitcairn Islands",
                             "Moldova"                   = "Republic of Moldova",
                             "Macao"                     = "Macau",
                             "Laos"                      = "Lao People's Democratic Republic",
                             "Iran"                      = "Iran (Islamic Republic of)",
                             "Curacao"                   = "Curaçao",
                             "Cape Verde"                = "Cabo Verde"))
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' @title get_vax_manufacturers
#' @description Get vaccinations from OWID
#'
#' @importFrom magrittr `%>%`
#'
#' @export
#'

get_vax_manufacturers <- function() {

  df <- read.csv("https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/vaccinations/locations.csv",
                 as.is = TRUE,
                 stringsAsFactors = FALSE,
                 check.names = FALSE) %>%
    dplyr::mutate(last_observation_date = as.Date(last_observation_date)) %>%
    dplyr::mutate(iso_code = dplyr::recode(iso_code, "OWID_KOS" = "XKX")) %>%
    dplyr::filter(!grepl("OWID", iso_code)) %>%
    dplyr::mutate(location = dplyr::recode(location,
                                           "United Kingdom"            = "The United Kingdom",
                                           "Syria"                     = "Syrian Arab Republic",
                                           "South Korea"               = "Republic of Korea",
                                           "Sint Maarten (Dutch part)" = "Sint Maarten",
                                           "Russia"                    = "Russian Federation",
                                           "Pitcairn"                  = "Pitcairn Islands",
                                           "Moldova"                   = "Republic of Moldova",
                                           "Macao"                     = "Macau",
                                           "Laos"                      = "Lao People's Democratic Republic",
                                           "Iran"                      = "Iran (Islamic Republic of)",
                                           "Curacao"                   = "Curaçao",
                                           "Cape Verde"                = "Cabo Verde"))
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' @title get_vaccinations
#' @description Get vaccinations from OWID
#'
#' @importFrom magrittr `%>%`
#'
#' @export
#'

get_vaccinations <- function() {

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # ~~~~~~~~~~~~~~~~ Setting up list element for re-formatting~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  map_data_field <-
    setNames(
      c(
        "Cumulative Doses Administered",
        "Cumulative Doses Administered",
        "Cumulative People Vaccinated (1 or more)",
        "Cumulative People Vaccinated (1 or more)",
        "Cumulative People Vaccinated (Fully)",
        "Cumulative People Vaccinated (Fully)",
        "Daily Doses Administered (7-day average)",
        "Daily Doses Administered (7-day average)"
      ),
      c(
        "total_vaccinations",
        "total_vaccinations_per_hundred",
        "people_vaccinated",
        "people_vaccinated_per_hundred",
        "people_fully_vaccinated",
        "people_fully_vaccinated_per_hundred",
        "daily_vaccinations",
        "daily_vaccinations_per_million"
      )
    )
  map_count_or_rate <-
    setNames(
      c("Count",
        "Rate",
        "Count",
        "Rate",
        "Count",
        "Rate",
        "Count",
        "Rate"
      ),
      c("total_vaccinations",
        "total_vaccinations_per_hundred",
        "people_vaccinated",
        "people_vaccinated_per_hundred",
        "people_fully_vaccinated",
        "people_fully_vaccinated_per_hundred",
        "daily_vaccinations",
        "daily_vaccinations_per_million"
      )
    )
  map_data_suffix <-
    setNames(
      c(" ",
        "Per 100 people",
        " ",
        "Per 100 people",
        " ",
        "Per 100 people",
        " ",
        "Per 1M people"
      ),
      c("total_vaccinations",
        "total_vaccinations_per_hundred",
        "people_vaccinated",
        "people_vaccinated_per_hundred",
        "people_fully_vaccinated",
        "people_fully_vaccinated_per_hundred",
        "daily_vaccinations",
        "daily_vaccinations_per_million"
      )
    )

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # ~~~~~~~~~~~~~~~~ Function to generate datasets ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  vax_raw <-
    read.csv(
      'https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/vaccinations/vaccinations.csv',
      as.is = TRUE,
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
  loc_raw <-
    read.csv(
      'https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/vaccinations/locations.csv',
      as.is = TRUE,
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
  man_raw <-
    read.csv(
      'https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/vaccinations/vaccinations-by-manufacturer.csv',
      as.is = TRUE,
      stringsAsFactors = FALSE,
      check.names = FALSE
    )

  # Drop all rows that do not have valid ISO 3 Code
  # This includes World data column, useful for displaying world data
  vax <- vax_raw %>%
    dplyr::mutate(date = as.Date(date, format = "%Y-%m-%d")) %>%
    tidyr::drop_na(iso_code) %>%
    dplyr::filter(iso_code != "OWID_EUN" & iso_code != "")
  vax <- vax[,!(names(vax) %in% c("daily_vaccinations_raw"))]
  loc <- loc_raw %>%
    tidyr::drop_na(iso_code) %>%
    dplyr::filter(iso_code != "OWID_EUN" & iso_code != "") %>%
    dplyr::rename(vaccine_manufacturer_list = vaccines)
  man <- man_raw %>%
    tidyr::drop_na(location) %>%
    dplyr::filter(
      location != "European Union" &
        location != "Wales" & location != "Scotland" & location != ""
    )

  # Flag the last and first dates recorded for each country
  vax_by_country <- vax %>%
    dplyr::arrange(desc(date)) %>%
    dplyr::group_by(iso_code) %>%
    dplyr::mutate(is_latest = dplyr::row_number(iso_code) == 1) %>%
    dplyr::mutate(is_first  = dplyr::row_number(iso_code) == dplyr::n())

  map_location <- setNames(loc$location, loc$iso_code)
  all_dates <-
    seq.Date(from = as.Date(min(vax$date)),
             to = as.Date(max(vax$date)),
             by = "day")

  # Change date to Date object
  # Fill in missing dates for the dataset
  vax_all <- vax_by_country %>%
    tidyr::complete(iso_code, date = all_dates) %>%
    dplyr::ungroup()

  vax_all$location[is.na(vax_all$location)] <-
    map_location[vax_all$iso_code[is.na(vax_all$location)]]


  # Split the vaccine manufacturer list and separate for each row
  loc$vaccine_manufacturer <- loc$vaccine_manufacturer_list
  loc <- loc %>%
    tidyr::separate_rows(vaccine_manufacturer, sep = ",")
  loc$vaccine_manufacturer <- stringr::str_trim(loc$vaccine_manufacturer)

  vax_all.long <- vax_all %>%
    tidyr::gather("raw_field",
                  "data_value",
                  total_vaccinations:daily_vaccinations_per_million)
  vax_all.long$data_field <- map_data_field[vax_all.long$raw_field]
  vax_all.long$data_suffix <-
    map_data_suffix[vax_all.long$raw_field]
  vax_all.long$count_or_rate <-
    map_count_or_rate[vax_all.long$raw_field]
  vax_all.long$ou_date_match <-
    paste(vax_all.long$iso_code, vax_all.long$date, sep = "_")

  count_vaccinated_filter <-
    function(df) {
      return(
        df$raw_field == "total_vaccinations" |
          df$raw_field == "people_vaccinated" |
          df$raw_field == "people_fully_vaccinated"
      )
    }
  count_vaccinated_breaks <- c(0, 1e3, 1e4, 1e5, 1e6, 1e7, Inf)
  count_vaccinated_ticks <- c(1e3, 1e4, 1e5, 1e6, 1e7, 5e8)
  rate_vaccinated_filter <-
    function(df) {
      return(
        df$raw_field == "people_vaccinated_per_hundred" |
          df$raw_field == "people_fully_vaccinated_per_hundred"
      )
    }
  rate_vaccinated_breaks <- c(0, 0.05, 1, 5, 10, 20, Inf)
  rate_vaccinated_ticks <- c(0.05, 1, 5, 10, 20, 100)
  rate_doses_filter <-
    function(df) {
      return(df$raw_field == "total_vaccinations_per_hundred")
    }
  rate_doses_breaks <- c(0, 0.05, 1, 5, 10, 20, Inf)
  rate_doses_ticks <- c(0.05, 1, 5, 10, 20, 200)
  count_daily_filter <-
    function(df) {
      return(df$raw_field == "daily_vaccinations")
    }
  count_daily_breaks <- c(0, 10, 1e2, 1e3, 1e4, 1e5, Inf)
  count_daily_ticks <- c(10, 1e2, 1e3, 1e4, 1e5, 5e6)
  rate_daily_filter <-
    function(df) {
      return(df$raw_field == "daily_vaccinations_per_million")
    }
  rate_daily_breaks <- c(0, 1, 10, 50, 100, 1e3, Inf)
  rate_daily_ticks <- c(1, 10, 50, 100, 1e3, 10e4)

  vax_all.long$data_cat_num <- NA

  vax_all.long$data_cat_num[count_vaccinated_filter(vax_all.long)] <-
    cut(vax_all.long$data_value[count_vaccinated_filter(vax_all.long)],
        breaks = count_vaccinated_breaks,
        na.rm = TRUE)
  vax_all.long$data_cat_num[rate_vaccinated_filter(vax_all.long)] <-
    cut(vax_all.long$data_value[rate_vaccinated_filter(vax_all.long)],
        breaks = rate_vaccinated_breaks,
        na.rm = TRUE)
  vax_all.long$data_cat_num[rate_doses_filter(vax_all.long)] <-
    cut(vax_all.long$data_value[rate_doses_filter(vax_all.long)],
        breaks = rate_doses_breaks,
        na.rm = TRUE)
  vax_all.long$data_cat_num[count_daily_filter(vax_all.long)] <-
    cut(vax_all.long$data_value[count_daily_filter(vax_all.long)],
        breaks = count_daily_breaks,
        na.rm = TRUE)
  vax_all.long$data_cat_num[rate_daily_filter(vax_all.long)] <-
    cut(vax_all.long$data_value[rate_daily_filter(vax_all.long)],
        breaks = rate_daily_breaks,
        na.rm = TRUE)

  vax_all.long$data_cat <- NA
  vax_all.long$data_cat[!is.na(vax_all.long$data_cat_num)] <-
    paste("cat", vax_all.long$data_cat_num)[!is.na(vax_all.long$data_cat_num)]

  cats = expand.grid(
    cat_num = c(1:6),
    raw_field = c(
      "total_vaccinations",
      "total_vaccinations_per_hundred",
      "people_vaccinated",
      "people_vaccinated_per_hundred",
      "people_fully_vaccinated",
      "people_fully_vaccinated_per_hundred",
      "daily_vaccinations",
      "daily_vaccinations_per_million"
    )
  )

  cats$data_field <- map_data_field[cats$raw_field]
  cats$data_suffix <- map_data_suffix[cats$raw_field]
  cats$count_or_rate <- map_count_or_rate[cats$raw_field]
  cats$tick_value <- 0
  cats$tick_value[count_vaccinated_filter(cats)] <-
    count_vaccinated_ticks[cats$cat_num[count_vaccinated_filter(cats)]]
  cats$tick_value[rate_vaccinated_filter(cats)] <-
    rate_vaccinated_ticks[cats$cat_num[rate_vaccinated_filter(cats)]]
  cats$tick_value[rate_doses_filter(cats)] <-
    rate_doses_ticks[cats$cat_num[rate_doses_filter(cats)]]
  cats$tick_value[count_daily_filter(cats)] <-
    count_daily_ticks[cats$cat_num[count_daily_filter(cats)]]
  cats$tick_value[rate_daily_filter(cats)] <-
    rate_daily_ticks[cats$cat_num[rate_daily_filter(cats)]]

  vax_list <-
    list(
      "all" = vax_all.long,
      "manufacturers" = loc,
      "rollout" = man,
      "categories" = cats
    )

  return(vax_list)
}
