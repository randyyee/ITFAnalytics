###################################################################################################################################
pkg.env <- new.env()

pkg.env$mtf_mean <- 5.12
pkg.env$mtf_stdv <- 4.28
pkg.env$ni_mean  <- 4.7
pkg.env$ni_stdv  <- 2.9
pkg.env$li_mean  <- 7.5
pkg.env$li_stdv  <- 3.4

###################################################################################################################################

#' @title epi_curve_14
#' @description Run EpiEstim.
#' @param country Optional.
#' If not provided, will use the package provided countries_data and add_country_dates to make the most current dataset.
#' See get_countries for description of required input.
#'
#' @importFrom magrittr `%>%`
#'
#' @export
#'

epi_curve_14 <- function(country, ctyname){
  #run EpiEstim
  index2   <- 15:nrow(country)
  T0       <- nrow(country)
  t_start  <- seq(2, T0-13) # starting at 2 as conditional on the past observations
  t_end    <- t_start + 13

  res_biweekly_ni <- EpiEstim::estimate_R(country[,c("date", "I")],
                                          method = "parametric_si",
                                          config = EpiEstim::make_config(list(
                                            t_start = t_start,
                                            t_end   = t_end,
                                            mean_si =  pkg.env$ni_mean,
                                            std_si  =  pkg.env$ni_stdv  ))
  )$R[,c("Mean(R)", "Quantile.0.025(R)", "Quantile.0.975(R)")]
  names(res_biweekly_ni) <- c("mean.ni", "lower.ni", "upper.ni")

  ######################################################################

  res_biweekly_li <- EpiEstim::estimate_R(country[,c("date", "I")],
                                          method = "parametric_si",
                                          config = EpiEstim::make_config(list(
                                            t_start = t_start,
                                            t_end   = t_end,
                                            mean_si = pkg.env$li_mean,
                                            std_si  = pkg.env$li_stdv))
  )$R[,c("Mean(R)", "Quantile.0.025(R)", "Quantile.0.975(R)")]
  names(res_biweekly_li) <- c("mean.li", "lower.li", "upper.li")

  ######################################################################

  res_biweekly_mtf <- EpiEstim::estimate_R(country[,c("date", "I")],
                                           method = "parametric_si",
                                           config = EpiEstim::make_config(list(
                                             t_start = t_start,
                                             t_end   = t_end,
                                             mean_si = pkg.env$mtf_mean,
                                             std_si  = pkg.env$mtf_stdv))
  )$R[,c("Mean(R)", "Quantile.0.025(R)", "Quantile.0.975(R)")]
  names(res_biweekly_mtf) <- c("mean.mtf", "lower.mtf", "upper.mtf")

  ######################################################################

  cbind(country=ctyname, date=country$date[index2], ncase=country$I[index2], res_biweekly_ni, res_biweekly_li, res_biweekly_mtf)
}


###################################################################################################################################

#' @title calc_rts
#' @description Run EpiEstim on JHU and WHO.
#' @param df_jhu Optional.
#' If not provided, will use the package provided countries_data and add_country_dates to make the most current dataset.
#' See get_countries for description of required input.
#'
#' @importFrom magrittr `%>%`
#'
#' @export
#'

calc_rts <- function(covid_sources){

  # If JHU dataframe is missing as input, then call the script to generate it
  if (missing(covid_sources)) {
    # Function to get JHU data
    df <- get_covid_sources()
  }

  df <- rbind(calc_jhu_rt() %>% dplyr::mutate(data_source = "JHU"),
              calc_who_rt() %>% dplyr::mutate(data_source = "WHO")) %>%
    dplyr::mutate(ou_date_src_match = paste(ou_date_match, data_source, sep="_"))

  return(df)
}

###################################################################################################################################

#' @title calc_jhu_rt
#' @description Run EpiEstim on JHU.
#' @param df_jhu Optional.
#' If not provided, will use the package provided countries_data and add_country_dates to make the most current dataset.
#' See get_countries for description of required input.
#'
#' @importFrom magrittr `%>%`
#'
#' @export
#'

calc_jhu_rt <- function(df_jhu){

  # If JHU dataframe is missing as input, then call the script to generate it
  if (missing(df_jhu)) {
    # Function to get JHU data
    df_jhu <- get_jhu_covid()
  }

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # ============= Overall function to generate Rt dataset ~~~~~~~===============
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  base_data <- df_jhu
  row.names(base_data)<-NULL

  dx <- base_data
  ######country data
  #d <- read_excel(paste0(dir.data, "JHU Data by Country - Long.xlsx"))
  index <- which(dx$date==min(dx$date))
  dx <- dx[-index,]

  dy <- dx %>% dplyr::select(c(1:6)) %>%
    # Take out negative values
    dplyr::mutate_if(is.numeric, ~replace(., .<0, 0))


  names(dy) <- c("country","date","I","ccase","ndeath","cdeath")
  dy$date <- as.Date(dy$date,"%m/%d/%Y")


  d <- dy %>%
    dplyr::group_by(country) %>%
    dplyr::arrange(date) %>%
    dplyr::mutate(mav = zoo::rollmean(I, k = 7, fill = NA)) %>%
    dplyr::mutate(mavr = zoo::rollmean(I, k = 7, fill = NA, align = "right")) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(I = mavr) %>%
    dplyr::filter(!is.na(I))



  dsplit<-split(d[,c("country","date","I")],d$country)
  countries <- names(dsplit)
  N<-length(countries)


  # epi_output<-lapply(1:N,function(i,x,y,z) epi_curve(x[[i]],y[i]),x=dsplit,y=countries)

  # epi_output_7  <- purrr::map2(.x = dsplit, .y=countries, .f = ~epi_curve_7(.x, .y))
  epi_output_14 <- purrr::map2(.x = dsplit, .y=countries, .f = ~epi_curve_14(.x, .y))

  # epi_7 <- dplyr::bind_rows(epi_output_7) %>% mutate(windowx = "7-day")
  epi_14 <- dplyr::bind_rows(epi_output_14) %>% dplyr::mutate(windowx = "14-day")

  combodf <- dplyr::bind_rows(epi_14)

  row.names(combodf)<-NULL

  # Getting cumulative cases to filter out values less than 12
  combined_output <- combodf %>%
    dplyr::mutate(ncasex = dplyr::if_else(is.na(ncase), 0, ncase)) %>%
    dplyr::group_by(country, windowx) %>%
    dplyr::mutate(cum_cases = cumsum(ncase)) %>%
    dplyr::ungroup()

  combined_output[which(combined_output$cum_cases<12), c(4:12)] <- NA

  # Creating the match variable so datasets can be joined in Power BI
  # using Country, Date ---> combines country code and date
  names(base_data) <- tolower(names(base_data))

  base_frame <- base_data %>%
    dplyr::select(ou_date_match, country, date, iso3code) %>%
    unique()

  finaldf <- dplyr::left_join(combined_output, base_frame) %>%
    dplyr::mutate(Rt_baseline = 1) %>%
    dplyr::mutate(
      mtf_mean = pkg.env$mtf_mean,
      mtf_stdv = pkg.env$mtf_stdv,
      ni_mean  = pkg.env$ni_mean ,
      ni_stdv  = pkg.env$ni_stdv ,
      li_mean  = pkg.env$li_mean ,
      li_stdv  = pkg.env$li_stdv
    )

  finaldf }


###################################################################################################################################

#' @title calc_who_rt
#' @description Run EpiEstim on WHO
#' @param df_jhu Optional.
#' If not provided, will use the package provided countries_data and add_country_dates to make the most current dataset.
#' See get_countries for description of required input.
#'
#' @importFrom magrittr `%>%`
#'
#' @export
#'

calc_who_rt <- function(df_who){

  # If WHO dataframe is missing as input, then call the script to generate it
  if (missing(df_who)) {
    # Function to get WHO data
    df_who <- get_who_covid()
  }


  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # ============= Overall function to generate Rt dataset ~~~~~~~===============
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  base_data <- df_who
  row.names(base_data)<-NULL

  dx <- base_data
  ######country data
  #d <- read_excel(paste0(dir.data, "JHU Data by Country - Long.xlsx"))
  index<-which(dx$date==min(dx$date))
  dx<-dx[-index,]

  dy <- dx %>% dplyr::select(c(1:6)) %>%
    # Take out negative values
    dplyr::mutate_if(is.numeric, ~replace(., .<0, 0))


  names(dy)<-c("country","date","I","ccase","ndeath","cdeath")
  dy$date <- as.Date(dy$date,"%m/%d/%Y")


  d <- dy %>%
    dplyr::group_by(country) %>%
    dplyr::arrange(date) %>%
    dplyr::mutate(mav = zoo::rollmean(I, k = 7, fill = NA)) %>%
    dplyr::mutate(mavr = zoo::rollmean(I, k = 7, fill = NA, align = "right")) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(I = mavr) %>%
    dplyr::filter(!is.na(I))



  dsplit<-split(d[,c("country","date","I")],d$country)
  countries <- names(dsplit)
  N<-length(countries)


  # epi_output<-lapply(1:N,function(i,x,y,z) epi_curve(x[[i]],y[i]),x=dsplit,y=countries)

  # epi_output_7  <- purrr::map2(.x = dsplit, .y=countries, .f = ~epi_curve_7(.x, .y))
  epi_output_14 <- purrr::map2(.x = dsplit, .y=countries, .f = ~epi_curve_14(.x, .y))

  # epi_7 <- dplyr::bind_rows(epi_output_7) %>% mutate(windowx = "7-day")
  epi_14 <- dplyr::bind_rows(epi_output_14) %>%
    dplyr::mutate(windowx = "14-day")

  combodf <- dplyr::bind_rows(epi_14)

  row.names(combodf)<-NULL

  # Getting cumulative cases to filter out values less than 12
  combined_output <- combodf %>%
    dplyr::mutate(ncasex = dplyr::if_else(is.na(ncase), 0, ncase)) %>%
    dplyr::group_by(country, windowx) %>%
    dplyr::mutate(cum_cases = cumsum(ncase)) %>%
    dplyr::ungroup()

  combined_output[which(combined_output$cum_cases<12), c(4:12)] <- NA

  # Creating the match variable so datasets can be joined in Power BI
  # using Country, Date ---> combines country code and date
  names(base_data) <- tolower(names(base_data))

  base_frame <- base_data %>%
    dplyr::select(ou_date_match, country, date, iso3code) %>%
    unique()

  finaldf <- dplyr::left_join(combined_output, base_frame) %>%
    dplyr::mutate(Rt_baseline = 1) %>%
    dplyr::mutate(
      mtf_mean = pkg.env$mtf_mean,
      mtf_stdv = pkg.env$mtf_stdv,
      ni_mean  = pkg.env$ni_mean ,
      ni_stdv  = pkg.env$ni_stdv ,
      li_mean  = pkg.env$li_mean ,
      li_stdv  = pkg.env$li_stdv
    )

  finaldf }
