###################################################################################################################################

#' @title calc_whocriteria
#' @description create binary variables for Mitigation re-opening for WHO criteria
#'
#' @param country Optional.
#'
#' @importFrom magrittr `%>%`
#'
#' @export
#'

calc_whocriteria <- function(covid_sources, df_testing_long, rts){

  if (missing(covid_sources)) {
    covid_sources <- get_covid_sources()
  }

  if (missing(df_testing_long)) {
    df_testing_long <- get_testing()[[2]]
  }

  if (missing(rts)) {
    rts <- calc_rts()
  }

  dfx <- covid_sources %>%
    dplyr::mutate(cases_cum   = dplyr::if_else(is.na(`Cumulative Cases`), 0, `Cumulative Cases`)) %>%
    dplyr::mutate(deaths_cum  = dplyr::if_else(is.na(`Cumulative Deaths`), 0, `Cumulative Deaths`)) %>%
    dplyr::mutate_if(is.numeric, ~replace(., .<0, 0)) %>%
    dplyr::group_by(data_source, country_code) %>%
    dplyr::arrange(Date) %>%
    dplyr::mutate(wkcase      =  cases_cum - dplyr::lag(cases_cum, 7)) %>%
    dplyr::mutate(prev_wkcase =  lag(cases_cum, 7) - dplyr::lag(cases_cum, 14)) %>%
    dplyr::mutate(wkdeath     =  deaths_cum - dplyr::lag(deaths_cum, 7)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate_if(is.numeric, ~replace(., .<0, NA_real_)) %>%
    dplyr::group_by(data_source, country_code) %>%
    dplyr::arrange(Date) %>%
    # Moving average of weekly cases and weekly deaths
    dplyr::mutate(mavr_wk = zoo::rollmean(wkcase, k = 7, fill = NA, align = "right"),
           mort_wk = zoo:rollmean(wkdeath, k = 7, fill = NA, align = "right")) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(mavr_wk_inci = dplyr::if_else(`Population 2020` > 0, (mavr_wk/`Population 2020`)*100000, NA_real_)) %>%
    # getting variable for growth and decline
    dplyr::group_by(data_source, country_code) %>%
    dplyr::arrange(Date) %>%
    dplyr::mutate(trajx   = dplyr::case_when(mavr_wk_inci >  dplyr::lag(mavr_wk_inci)  ~ "growth",
                                             mavr_wk_inci <  dplyr::lag(mavr_wk_inci)  ~ "decline",
                                             mavr_wk_inci == dplyr::lag(mavr_wk_inci)  ~ "plateau",
                                             TRUE ~ "not estimated")) %>%
    dplyr::mutate(traj    = dplyr::case_when(mavr_wk_inci >  dplyr::lag(mavr_wk_inci)  ~ "growth",
                                             mavr_wk_inci <  dplyr::lag(mavr_wk_inci)  ~ "decline",
                                             mavr_wk_inci == dplyr::lag(mavr_wk_inci)  ~ NA_character_,
                                             TRUE ~ "not estimated")) %>%
    dplyr::mutate(mortraj = dplyr::case_when(mort_wk      >  dplyr::lag(mort_wk)       ~ "growth",
                                             mort_wk      <  dplyr::lag(mort_wk)       ~ "decline",
                                             mort_wk      == dplyr::lag(mort_wk)       ~ "plateau",
                                             TRUE ~ "not estimated")) %>%
    dplyr::ungroup() %>%
    # Change plateaus flanked by growth or decline as growth or decline respectively
    dplyr::arrange(data_source, country_code, Date) %>%
    tidyr::fill(traj, .direction = "down") %>%
    dplyr::mutate(numdaysx       = dplyr::if_else(traj == dplyr::lag(traj, 1), 1, 0)) %>%
    dplyr::mutate(numdays_mort   = dplyr::if_else(mortraj == dplyr::lag(mortraj, 1),1,0)) %>%
    dplyr::mutate(varx           = 1) %>%
    dplyr::mutate(varx_mort      = 1) %>%
    # Getting growth infection points
    dplyr::mutate(growth_inflect = dplyr::if_else(numdaysx==1 & lead(numdaysx,1) == 0 & traj == "growth", 1, 0))



  # Setting up counter for getting days in each category
  for(i in c(1:length(dfx$varx))){   #day number times the number of days it's been on a trajectory,
    if(i == 1){dfx$varx[i] = 1} else {  # plus 1 becasue the second day is listed as day 1
      dfx$varx[i] <- (dfx$varx[i-1]*dfx$numdaysx[i])+1
    }}
  for(i in c(1:length(dfx$varx_mort))){ #same thing for mortality
    if(i==1){dfx$varx_mort[i] = 1} else {
      dfx$varx_mort[i] <- (dfx$varx_mort[i-1]*dfx$numdays_mort[i])+1
    }}

  df1 <- dfx %>%
    # get last peak incidence value
    dplyr::mutate(lastpeak_inc = dplyr::if_else(growth_inflect==1, mavr_wk_inci, NA_real_)) %>%
    dplyr::arrange(data_source, country_code, Date) %>%
    dplyr::fill(lastpeak_inc, .direction = "down") %>%
    dplyr::mutate(epi_cat = dplyr::case_when(growth_inflect==1                                                ~ "Peak",
                                             traj == "decline" & varx>=21 &  mavr_wk_inci<=(0.5*lastpeak_inc) ~ "Decline (criteria met)",
                                             trajx == "growth"                                                ~ "Growth",
                                             trajx == "decline" & varx<21                                     ~ "Decline (<3 wks)",
                                             traj == "decline" & varx>=21 &  mavr_wk_inci>(0.5*lastpeak_inc)  ~ "Decline (3+wks, criteria not met)",
                                             trajx == "plateau"                                               ~ "Plateau",
                                             TRUE ~ "not estimated")) %>%
    mutate(death_cat = dplyr::case_when(mortraj == "decline" & varx_mort>=21                                  ~"Decline (criteria met)",
                                        mortraj == "growth"                                                   ~"Growth",
                                        mortraj == "decline" & varx_mort<21                                   ~"Decline (<3 wks)",
                                        mortraj == "plateau"                                                  ~"Plateau",
                                        TRUE ~ "not estimated"))


  #testing criteria: <= 5% positive for 2 weeks
  names(df_testing_long)[which(colnames(df_testing_long)=="source")] <- "Source_testing"
  dfx2 <- merge(df1,df_testing_long,by=c("ou_date_match"),all.x=T)
  k=15
  dfx2 <- dfx2 %>%
    #group_by(data_source,country_code) %>%
    dplyr::arrange(Source_testing, data_source, country_code, Date) %>%
    dplyr::mutate(test_5               =  perc_positive_testing < 5) %>%
    dplyr::mutate(test_5_3weeks        = zoo::rollapplyr(test_5, k, function(x) all(x[k]==T & (x[k] == x[-k])), fill = NA)) %>%
    dplyr::mutate(who_criteria_testing = ifelse(test_5_3weeks == T, "Criteria Met","Criteria Not Met")) %>%
    ungroup()
  ##New addition
  last_date <- dfx2 %>%
    dplyr::filter(!is.na(who_criteria_testing)) %>%
    dplyr::arrange(Source_testing,data_source, country_code, Date) %>%
    dplyr::group_by(Source_testing, data_source, country_code) %>%
    dplyr::summarise(who_criteria_testing_last  = dplyr::last(who_criteria_testing) ,
                     date_criteria_testing_last = dplyr::last(Date)) %>%
    dplyr::ungroup()
  dfx2 <- dplyr::left_join(dfx2,last_date)

  dfx3 <- merge(dfx2,rts,by=c("ou_date_src_match"),all.x=T)

  k=15
  dfx3 <- dfx3 %>%
    dplyr::arrange(data_source.x, country_code, Date) %>%
    dplyr::mutate(rt_1            = mean.mtf  < 1) %>%
    dplyr::mutate(rt_1_2weeks     = rollapplyr(rt_1, k, function(x) all(x[k] == T & (x[k] == x[-k])), fill = NA)) %>%
    dplyr::mutate(who_criteria_Rt = ifelse(rt_1_2weeks == T, "Criteria Met","Criteria Not Met")) %>%
    dplyr::ungroup()



  #Decline in deaths for 3 weeks
  k=22
  dfx4 <- dfx3 %>%
    dplyr::mutate(case_fatality           = ifelse(cases_cum == 0, NA, deaths_cum/cases_cum)) %>%
    dplyr::arrange(data_source.x, country_code, Date) %>%
    dplyr::mutate(death_dec               = case_fatality < dplyr::lag(case_fatality)) %>%
    dplyr::mutate(death_diff2             = zoo::rollapplyr(death_dec, k, function(x) all(x[k] == T & (x[k] == x[-k])), fill = NA)) %>%
    dplyr::mutate(who_criteria_death_decs = ifelse(death_diff2 == T, "Criteria Met","Criteria Not Met")) %>%
    dplyr::ungroup()

  dfx4$who_criteria_cases <- "Criteria Not Met"
  dfx4$who_criteria_cases[dfx4$epi_cat == "Decline (criteria met)"] <- "Criteria Met"
  dfx4$who_criteria_death_decs2 <- "Criteria Not Met"
  dfx4$who_criteria_death_decs2[dfx4$death_cat=="Decline (criteria met)"] <-"Criteria Met"


  # Removing a lot of the columns in this dataset b/c it is too large, many columns are duplicates from other data tables
  namesdf <- c("Country","Date","WHO Region","Country Code","ou_date_src_match","ou_date_match.x",
               "who_criteria_cases","who_criteria_death_decs2","perc_positive_testing","who_criteria_testing",
               "who_criteria_Rt","who_criteria_death_decs","Source_testing")
  vars <- names(dfx4) %in% namesdf
  finaldf <- dfx4[vars]

  finaldf
}
