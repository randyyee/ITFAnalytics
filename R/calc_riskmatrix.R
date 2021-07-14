#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' @title calc_riskmatrix
#' @description Calculate a risk matrix.
#' @importFrom magrittr `%>%`
#'
#' @export
#'
#' @examples
#' \dontrun{
#' riskmatrix_df <- calc_riskmatrix()}

calc_riskmatrix <- function(covid_sources, rts) {

  if (missing(covid_sources)) {
    covid_sources <- get_covid_sources()
  }

  if (missing(rts)) {
    rts <- calc_rts()
  }

  df <- dplyr::left_join(dplyr::select(covid_sources, -ou_date_match),
                         dplyr::select(rts, ou_date_src_match, mean.mtf)) %>%
    dplyr::mutate(cases_cum    = dplyr::if_else(is.na(`Cumulative Cases`), 0, `Cumulative Cases`)) %>%
    dplyr::group_by(data_source, country_code) %>%
    dplyr::arrange(Date) %>%
    dplyr::mutate(wkcase       = cases_cum - dplyr::lag(cases_cum, 7)) %>%
    dplyr::ungroup() %>%
    # zero out negative values
    dplyr::mutate_if(is.numeric, ~replace(., . < 0, 0)) %>%
    dplyr::mutate(inci         = dplyr::if_else(`Population 2020` > 0, ((wkcase/`Population 2020`)/7) * 100000, NA_real_)) %>%
    dplyr::rename(Rt           = mean.mtf) %>%
    dplyr::mutate(ou_src_match = paste(country_code, data_source, sep="_"))

  crossx <- df %>%
    dplyr::select(-ou_date_src_match) %>%
    dplyr::group_by(data_source) %>%
    dplyr::mutate(datex = max(Date)) %>%
    dplyr::ungroup() %>%
    dplyr::filter(Date == datex) %>%
    dplyr::select(-datex)

  return(list(df,crossx))
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' @title calc_riskmatrix_v2
#' @description Calculate a risk matrix.
#' @importFrom magrittr `%>%`
#'
#' @export
#'
#' @examples
#' \dontrun{
#' riskmatrix_v2_df <- calc_riskmatrix_v2()}
#'

calc_riskmatrix_v2 <- function(covid_sources){

  if (missing(covid_sources)) {
    covid_sources <- get_covid_sources()
  }

  df <- covid_sources %>%
    dplyr::select(-ou_date_match)%>%
    dplyr::mutate(cases_cum     = dplyr::if_else(is.na(`Cumulative Cases`), 0, `Cumulative Cases`)) %>%
    dplyr::mutate_if(is.numeric, ~replace(., .<0, 0)) %>%
    dplyr::group_by(data_source, country_code) %>%
    dplyr::arrange(Date) %>%
    dplyr::mutate(wkcase        = cases_cum - dplyr::lag(cases_cum, 7)) %>%
    dplyr::mutate(prev_wkcase   = dplyr::lag(cases_cum, 7) - dplyr::lag(cases_cum, 14)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate_if(is.numeric, ~replace(., .<0, NA_real_)) %>%
    dplyr::mutate(case_diff     = wkcase - prev_wkcase) %>%
    dplyr::mutate(wkcase_change = dplyr::if_else(prev_wkcase > 0, (case_diff)/prev_wkcase, NA_real_)) %>%
    dplyr::mutate(inci          = dplyr::if_else(`Population 2018.x` > 0,((wkcase/`Population 2018.x`)/7) * 100000, NA_real_)) %>%
    dplyr::mutate(Rt            = 1) %>%
    dplyr::mutate(ou_src_match  = paste(country_code, data_source, sep="_"))

  crossx <- df %>%
    dplyr::select(-ou_date_src_match) %>%
    dplyr::group_by(data_source) %>%
    dplyr::mutate(datex = max(Date)) %>%
    dplyr::ungroup() %>%
    dplyr::filter(Date == datex) %>%
    dplyr::select(-datex)

  return(list(df, crossx))
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' @title calc_riskmatrix_v3
#' @description Calculate a risk matrix.
#' @importFrom magrittr `%>%`
#'
#' @export
#'
#' @examples
#' \dontrun{
#' riskmatrix_v3_df <- calc_riskmatrix_v3()}
#'

calc_riskmatrix_v3 <- function(df_ncov){

  if (missing(df_ncov)) {
    df_ncov <- get_covid_sources()
  }

  df <- df_ncov %>%
    dplyr::select(-ou_date_match) %>%
    dplyr::mutate(cases_cum      = dplyr::if_else(is.na(`Cumulative Cases`),  0, `Cumulative Cases`)) %>%
    dplyr::mutate(deaths_cum     = dplyr::if_else(is.na(`Cumulative Deaths`), 0, `Cumulative Deaths`)) %>%
    dplyr::mutate_if(is.numeric, ~replace(., . < 0, 0)) %>%
    dplyr::group_by(data_source, country_code) %>%
    dplyr::arrange(Date) %>%
    dplyr::mutate(wkcase         =  cases_cum - dplyr::lag(cases_cum, 7)) %>%
    dplyr::mutate(prev_wkcase    =  dplyr::lag(cases_cum, 7) - dplyr::lag(cases_cum, 14)) %>%
    dplyr::mutate(wkdeath        =  deaths_cum - dplyr::lag(deaths_cum, 7)) %>%
    dplyr::mutate(prev_wkdeath   =  dplyr::lag(deaths_cum, 7) - dplyr::lag(deaths_cum, 14)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate_if(is.numeric, ~replace(., .<0, NA_real_)) %>%
    dplyr::mutate(case_diff      = wkcase-prev_wkcase) %>%
    dplyr::mutate(death_diff     = wkdeath-prev_wkdeath) %>%
    dplyr::mutate(wkcase_change  = dplyr::if_else(prev_wkcase  > 0, (case_diff)/prev_wkcase, NA_real_)) %>%
    dplyr::mutate(wkdeath_change = dplyr::if_else(prev_wkdeath > 0, (death_diff)/prev_wkdeath, NA_real_)) %>%
    dplyr::mutate(inci           = dplyr::if_else(`Population 2018.x` > 0, ((wkcase/`Population 2018.x`)/7) * 100000,  NA_real_)) %>%
    dplyr::mutate(incideath      = dplyr::if_else(`Population 2018.x` > 0, ((wkdeath/`Population 2018.x`)/7) * 100000, NA_real_)) %>%
    dplyr::mutate(pct_chng       = dplyr::if_else(!(is.na(wkcase_change)),   wkcase_change * 100, NA_real_)) %>%
    dplyr::mutate(pct_chngdeath  = dplyr::if_else( !(is.na(wkdeath_change)), wkdeath_change * 100, NA_real_)) %>%
    dplyr::mutate(Rt             = 1) %>%
    dplyr::mutate(ou_src_match   = paste(country_code, data_source, sep="_"))

  crossx <- df %>%
    dplyr::select(-ou_date_src_match) %>%
    dplyr::group_by(data_source) %>%
    dplyr::mutate(datex = max(Date)) %>%
    dplyr::ungroup() %>%
    dplyr::filter(Date == datex) %>%
    dplyr::select(-datex)

  return(list(df, crossx))
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' @title calc_gen_riskmatrix
#' @description Calculate a risk matrix for a generalized df input.
#'
#' @param df Dataframe with cumulative_cases and cumulative_deaths
#' @param population Population dataframe with population
#'
#' @importFrom magrittr `%>%`
#'
#' @export
#'
#' @examples
#' \dontrun{
#' riskmatrix_v3_df <- calc_riskmatrix_v3()}
#'

calc_gen_riskmatrix <- function(df, population){

  df <- df %>%
    dplyr::left_join(population) %>%
    dplyr::mutate_if(is.numeric, ~replace(., . < 0, 0)) %>%
    dplyr::group_by(source, country_code) %>%
    dplyr::arrange(date) %>%
    dplyr::mutate(daily_case_incidence  = dplyr::if_else(population > 0, ((new_cases/population)) * 100000,  NA_real_)) %>%
    dplyr::mutate(daily_death_incidence = dplyr::if_else(population > 0, ((new_deaths/population)) * 100000, NA_real_)) %>%
    dplyr::mutate(weekdate              = lubridate::floor_date(date, "week", week_start = 1)) %>%
    dplyr::mutate(week_case             = cumulative_cases - dplyr::lag(cumulative_cases, 7)) %>%
    dplyr::mutate(prev_week_case        = dplyr::lag(cumulative_cases, 7) - dplyr::lag(cumulative_cases, 14)) %>%
    dplyr::mutate(week_death            = cumulative_deaths - dplyr::lag(cumulative_deaths, 7)) %>%
    dplyr::mutate(prev_week_death       = dplyr::lag(cumulative_deaths, 7) - dplyr::lag(cumulative_deaths, 14)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate_if(is.numeric, ~replace(., .<0, NA_real_)) %>%
    dplyr::mutate(diff_case             = week_case-prev_week_case) %>%
    dplyr::mutate(diff_death            = week_death-prev_week_death) %>%
    dplyr::mutate(week_case_change      = dplyr::if_else(prev_week_case  > 0, (diff_case)/prev_week_case, NA_real_)) %>%
    dplyr::mutate(week_death_change     = dplyr::if_else(prev_week_death > 0, (diff_death)/prev_week_death, NA_real_)) %>%
    dplyr::mutate(week_case_incidence   = dplyr::if_else(population > 0, ((week_case/population)/7) * 100000,  NA_real_)) %>%
    dplyr::mutate(week_death_incidence  = dplyr::if_else(population > 0, ((week_death/population)/7) * 100000, NA_real_)) %>%
    dplyr::mutate(percent_change_case   = dplyr::if_else(!(is.na(week_case_change)),   week_case_change * 100, NA_real_)) %>%
    dplyr::mutate(percent_change_death  = dplyr::if_else( !(is.na(week_death_change)), week_death_change * 100, NA_real_))

  return(df)
}
