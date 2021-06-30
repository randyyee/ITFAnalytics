###################################################################################################################################

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
    dplyr::mutate(inci         = if_else(`Population 2020` > 0, ((wkcase/`Population 2020`)/7) * 100000, NA_real_)) %>%
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

###################################################################################################################################

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

###################################################################################################################################

#' @title calc_riskmatrix_v3
#' @description Caculate a risk matrix.
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
    dplyr::mutate(cases_cum      = dplyr::if_else(is.na(`Cumulative Cases`), 0, `Cumulative Cases`)) %>%
    dplyr::mutate(deaths_cum     = dplyr::if_else(is.na(`Cumulative Deaths`), 0, `Cumulative Deaths`)) %>%
    dplyr::mutate_if(is.numeric, ~replace(., . < 0, 0)) %>%
    dplyr::group_by(data_source, country_code) %>%
    dplyr::arrange(Date) %>%
    dplyr::mutate(wkcase         =  cases_cum - dplyr::lag(cases_cum, 7)) %>%
    dplyr::mutate(prev_wkcase    =  dplyr::lag(cases_cum, 7) - dplyr::lag(cases_cum, 14)) %>%
    dplyr::mutate(wkdeath        =  deaths_cum - dplyr::lag(deaths_cum, 7)) %>%
    dplyr::mutate(prev_wkdeath   =  ldplyr::ag(deaths_cum, 7) - dplyr::lag(deaths_cum, 14)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate_if(is.numeric, ~replace(., .<0, NA_real_)) %>%
    dplyr::mutate(case_diff      = wkcase-prev_wkcase) %>%
    dplyr::mutate(death_diff     = wkdeath-prev_wkdeath) %>%
    dplyr::mutate(wkcase_change  = dplyr::if_else(prev_wkcase > 0, (case_diff)/prev_wkcase, NA_real_)) %>%
    dplyr::mutate(wkdeath_change = dplyr::if_else(prev_wkdeath > 0, (death_diff)/prev_wkdeath, NA_real_)) %>%
    dplyr::mutate(inci           = dplyr::if_else(`Population 2018.x` > 0, ((wkcase/`Population 2018.x`)/7) * 100000,  NA_real_)) %>%
    dplyr::mutate(incideath      = dplyr::if_else(`Population 2018.x` > 0, ((wkdeath/`Population 2018.x`)/7) * 100000, NA_real_)) %>%
    dplyr::mutate(pct_chng       = dplyr::if_else(!(is.na(wkcase_change)), wkcase_change * 100, NA_real_)) %>%
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
