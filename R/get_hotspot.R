###################################################################################################################################

#' @title get_hotspots
#' @description Hotspots for cumulative incidence or mortality. Need to supply JHU and WHO combined dataset.
#' Original code by Kathryn Winglee
#' Modified by Adam Vaughan & Michelle Schmitz / James Fuller / Imran Mujawar
#' May 1, 2020
#' This version adopts the script for Power BI
#' updated 9/24 with new algorithm from CBS GitHub page: https://github.com/cdcent/covid-response-analyses - Andrea Stewart
#' @importFrom magrittr `%>%`
#'
#' @export
#'
#' @examples
#' \dontrun{
#' hotspot_df <- get_hotspot()}

get_hotspots <- function(df_ncov){

  # If base data frame is missing, then call the script to generate it
  if (missing(df_ncov)) {
    # Function to get base dataframe with cases and deaths daily/cumulative
    df_ncov <- get_covid_sources()
  }


  final_hotspot  <- function(dfsource){

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # ~~~~~~~~~~~~~~~~ NEW hotspot algorithm (from CBS github 9/24) ~~~~~~~~~~~~~~~~~
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    df.Countriesx <- df_ncov # Pulling in case and death data
    df.Countries.Daily <- df.Countriesx %>%
      dplyr::filter(data_source== dfsource)

    #consolidate the df.Countries.Daily dataset as some areas have multiple observations
    df.cases_fin <- df.Countries.Daily[c("country_code","Date", "Population 2020", "Cumulative Cases")] %>%
      dplyr::group_by(country_code, Date,`Population 2020`) %>%
      dplyr::summarise(Cumulative_Cases = sum(`Cumulative Cases`, na.rm=T)) %>%
      dplyr::ungroup() %>%
      dplyr::filter(`Population 2020` > 0 & !is.na(`Population 2020`)) %>%
      dplyr::mutate(Cumulative_Cases = dplyr::case_when(Cumulative_Cases < 0    ~0,
                                                        is.na(Cumulative_Cases) ~0,
                                                        TRUE ~Cumulative_Cases)) %>%
      #select(-data_source,-country_code) %>%
      #rename to align with CBS code names
      dplyr::rename(
        id=country_code,
        cases=Cumulative_Cases,
        pop=`Population 2020`,
        date=Date)

    #DEATHS
    # df.deaths_fin <- df.Countries.Daily[c("country_code","Date", "Population 2020", "Cumulative Deaths")] %>%
    #   dplyr::group_by(country_code, Date, `Population 2020`) %>%
    #   dplyr::summarise(Cumulative_Deaths = sum(`Cumulative Deaths`, na.rm=T)) %>%
    #   dplyr::ungroup() %>%
    #   dplyr::filter(`Population 2020` > 0 & !is.na(`Population 2020`)) %>%
    #   dplyr::mutate(Cumulative_Deaths=dplyr::case_when(Cumulative_Deaths < 0~0,
    #                                                    is.na(Cumulative_Deaths)~0,
    #                                                    TRUE ~Cumulative_Deaths)) %>%
    #   dplyr::rename(
    #     id=country_code,
    #     cases=Cumulative_Deaths,
    #     pop=`Population 2020`,
    #     date=Date)

    alldata <- hotspot(df.cases_fin,"case", 0.1, 10, 100, 7, 0.6, dsource=dfsource)

    allthedata <- alldata %>%
      dplyr::mutate(data_source = dfsource,
                    datatype="Case") %>%
      dplyr::mutate(ou_date_match = paste(id, dates, sep="_")) %>%
      #Creating variable that is not date dependent
      dplyr::mutate(ou_cut_match = paste(id, slope_cut, rate_cut, sep="_")) %>%
      dplyr::rename(country_code = id) %>%
      dplyr::rename(date = dates) %>%
      dplyr::mutate(ou_date_src_match = paste(ou_date_match, data_source, sep="_")) %>%
      dplyr::mutate(ou_cut_src_match = paste(ou_cut_match, data_source, sep="_")) %>%
      dplyr::select(-ou_cut_match)

    return(allthedata)
  }


  trajdf <- dplyr::bind_rows(final_hotspot("WHO"), final_hotspot("JHU"))
  mapdf <- trajdf %>%
    dplyr::select(-ou_date_match, -ou_date_src_match) %>%
    dplyr::filter(date %in% max(date))

  return(list(trajdf,mapdf))
}


###################################################################################################################################

#' @title hotspot
#' @description Add dates to country data
#'
#' @param input.data get_covid_sources()
#' @param base.data case
#' @param slope.cut 0.1
#' @param rate.cut 10
#' @param hb.cut 100
#' @param n 7
#' @param spar.param 0.6
#' @param dsource Either "WHO" or "JHU"
#'
#' @importFrom magrittr `%>%`
#'
#' @export

hotspot <- function(input.data,
                    base.data,
                    slope.cut,
                    rate.cut,
                    hb.cut,
                    n,
                    spar.param,
                    dsource) {


  #consolidate the df.Countries.Daily dataset as some areas have multiple observations
  df <-input.data %>%
    dplyr::arrange(id, date) %>%
    dplyr::group_by(id) %>%
    dplyr::mutate(
      n.cum = cases,
      n.2wk = n.cum - dplyr::lag(n.cum, n = 14),
      n.2wk = ifelse(is.na(n.2wk), n.cum, n.2wk),
      inc.raw = 100000 * (n.cum / pop),
      inc = zoo::rollmean(inc.raw, k = n, fill = 0, align = "right"),
      ci.2wk = inc.raw - dplyr::lag(inc.raw, n = 14),
      ci.2wk = ifelse(is.na(ci.2wk), inc.raw, ci.2wk),
      daily.inc.raw = n.cum - dplyr::lag(n.cum),
      daily.inc.raw = ifelse(is.na(daily.inc.raw), n.cum, daily.inc.raw),
      daily.inc.raw = 100000 * (daily.inc.raw / pop),
      daily.inc.raw = ifelse(daily.inc.raw < 0, 0, daily.inc.raw),
      values = inc - dplyr::lag(inc),
      values = ifelse(is.na(values), daily.inc.raw, values),
      values = ifelse(values < 0, 0, values),
      row = dplyr::row_number()) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(id,date) %>%
    dplyr::relocate(id)

  #.....................................................................................
  # > PIVOT TO DATA.TABLE AND PREPARE FOR PROCESSING
  #.....................................................................................
  N = length(unique(df$date))
  df.table <- data.table::as.data.table(df)
  df.table[, threshmet:= row >= 15, by = c("id", "date")]
  values.shifted <- data.table::setnames(df.table[, data.table::shift(get("values"), 0:(N - 1), type = "lag"), by = c("id")],
                             old = 2:(N + 1),
                             new = paste0("split", 1:N))

  data.splits <- values.shifted %>%
    data.table::melt(id = c("id"),
         measure = patterns("split"),
         value.name = "values",
         variable.name = "date_num")
  data.shifted <- data.splits[!is.na(values), ]
  rm(data.splits)
  data.shifted[, row := seq_len(.N), by = c("id", "date_num")]
  data.shifted[, date_num:= N - as.integer(date_num) + 1]
  data.shifted[df.table, on = c("id", "row"), ":="  (date = i.date, inc = i.inc, n.2wk = i.n.2wk, ci.2wk = i.ci.2wk)]
  data.shifted[, threshmet := date_num >= 15, by = c("id", "date")]
  data.shifted <- data.shifted %>%
    data.table::setorderv(cols = c("date_num", "date"))

  return_spline <- function(x, y, grp){

    .spline <- smooth.spline(x = as.numeric(x), y = y, spar = spar.param)
    .fitted <- predict(.spline, as.numeric(x))$y
    .deriv1 <- predict(.spline, deriv = 1)$y

    return(list(.fitted, .deriv1))
  }

  #.....................................................................................
  # > ITERATIVE SPLINE PROCESSING ####
  #.....................................................................................
  data.shifted[threshmet == TRUE, c("spline", "deriv1") := return_spline(date, values, date_num), by = c("id", "date_num")]

  #.....................................................................................
  # > EPICURVE CATEGORIES ####
  #.....................................................................................
  data.shifted[, spline := data.table::fifelse(date_num <= 14, 0, spline)]
  data.shifted[, deriv1 := data.table::fifelse(date_num <= 14, 0, deriv1)]
  data.shifted[, category := data.table::fcase(inc == 0, "none",
                                   deriv1 >= 0 & deriv1 < slope.cut & date_num >= 15, "plateau",
                                   deriv1 >= slope.cut, "growth",
                                   deriv1 < 0, "decline",
                                   date_num <= 14, "none")]
  # Setting initial 5 days of decline to plateau suspended for the time being
  # data.shifted[, grp := rleid(category), by = c("id", "date_num")]
  # data.shifted[, counter := sequence(.N), by = c("id", "date_num", "grp")]
  # data.shifted[, period_total := max(counter), by = c("id", "date_num", "grp")]
  # data.shifted[, category := data.table::fifelse(category == "decline" & counter <= 5 & date_num >= 15, "plateau", category)]
  data.shifted[, category := data.table::fifelse(ci.2wk <= rate.cut & !is.na(ci.2wk) & category == "plateau", "low incidence plateau",
                                     data.table::fifelse(ci.2wk > rate.cut & category == "plateau", "elevated incidence plateau", category))]
  data.shifted[, category := data.table::fifelse(ci.2wk <= rate.cut & !is.na(ci.2wk) & category == "growth", "low incidence growth",
                                     data.table::fifelse(ci.2wk > rate.cut & category == "growth", "elevated incidence growth", category))]
  data.shifted[, category := data.table::fifelse(inc <= rate.cut & is.na(ci.2wk) & category == "plateau", "low incidence plateau",
                                     data.table::fifelse(ci.2wk > rate.cut & category == "plateau", "elevated incidence plateau", category))]
  data.shifted[, category := data.table::fifelse(inc <= rate.cut & is.na(ci.2wk) & category == "growth", "low incidence growth",
                                     data.table::fifelse(ci.2wk > rate.cut & category == "growth", "elevated incidence growth", category))]
  data.shifted[, category := data.table::fifelse(n.2wk <= 5 & !is.na(n.2wk) & n.2wk > 0 & date_num >= 15, "lessthan5", category)]
  data.shifted[, category := data.table::fifelse(ci.2wk == 0 & inc > 0, "nonepast2wk", category)]
  data.shifted[, category := data.table::fifelse(ci.2wk == 0 & inc == 0, "none", category)]
  data.shifted[, cat2 := "none"]
  data.shifted[, cat2 := data.table::fifelse(ci.2wk > hb.cut & deriv1 >= slope.cut, "hbg", "not hbg")]
  data.shifted[, cat2 := data.table::fifelse(n.2wk <= 5 & !is.na(n.2wk) & inc != 0 & date_num >= 15, "lessthan5", cat2)]
  data.shifted[, cat2 := data.table::fifelse(is.na(n.2wk) & inc != 0, "not hbg", cat2)]
  data.shifted[, cat2 := data.table::fifelse(ci.2wk == 0 & inc > 0, "nonepast2wk", cat2)]
  data.shifted[, cat2 := data.table::fifelse(ci.2wk == 0 & inc == 0, "none", cat2)]
  data.shifted[, cat2 := data.table::fifelse(is.na(cat2), "not hbg", cat2)]
  data.shifted[, cat2 := data.table::fifelse(date_num <= 14, "none", cat2)]

  #.....................................................................................
  # > TRAJECTORY AND REBOUND ####
  #.....................................................................................
  data.shifted <- data.table::setorderv(data.shifted, cols = c("id", "date", "date_num"))
  geography.cats.final <- data.shifted[, .SD[.N], by = c("id", "date_num")]
  rm(data.shifted)
  geography.cats.final[category %in% c("elevated incidence plateau", "elevated incidence growth"), hasBeenEI := TRUE]
  geography.cats.final[, hasBeenEI := zoo::na.locf(hasBeenEI, na.rm = FALSE), by = "id"]
  geography.cats.final[, hasBeenEI := data.table::fifelse(is.na(hasBeenEI), FALSE, hasBeenEI)]
  geography.cats.final[, EI := data.table::fifelse(hasBeenEI == TRUE & category %in% c("elevated incidence plateau", "elevated incidence growth", "low incidence growth"), TRUE, FALSE)]
  geography.cats.final[, DT := data.table::fifelse((category != "none")
                                       & (hasBeenEI == TRUE)
                                       & (category == "decline" | category %in% c("low incidence plateau", "lessthan5", "nonepast2wk")), 1, 0)]
  # Grace criteria suspended for the time being
  # geography.cats.final[, DT := data.table::fcase(DT == 1, 1,
  #                                    (DT == 0 & shift(DT, n = 1) == 1), 1,
  #                                    (DT == 0 & shift(DT, n = 2) == 1), 1,
  #                                    (DT == 0 & shift(DT, n = 3) == 1), 1,
  #                                    (DT == 0 & shift(DT, n = 4) == 1), 1,
  #                                    (DT == 0 & shift(DT, n = 5) == 1), 1,
  #                                    default = 0), by = "id"]
  geography.cats.final[, DT := seq(.N) * DT, by = .(id, data.table::rleid(DT))]
  geography.cats.final[, maxDaysDownward := cummax(DT), by = "id"]
  geography.cats.final[, rebound := data.table::fcase(category %in% c("none", "lessthan5", "nonepast2wk"), category,
                                          maxDaysDownward >= 14 & DT == 0, "rebound",
                                          data.table::between(maxDaysDownward, 1, 13) & DT == 0, "Upnot14",
                                          date_num <= 14, "no",
                                          default = "none"), by = "id"]

  #.....................................................................................
  # > EXIT PROCESSING  ####
  #.....................................................................................
  df.merger <- df %>%
    dplyr::arrange(id, date) %>%
    dplyr::select(pop,
                  cases,
                  n.cum,
                  inc.raw,
                  daily.inc.raw)

  cat3 <- geography.cats.final %>%
    as.data.frame() %>%
    dplyr::arrange(id, date) %>%
    dplyr::bind_cols(df.merger) %>%
    # mutate(id = as.integer(id)) %>%
    #  mutate(id = as.factor(id)) %>%
    dplyr::select(
      id,
      dates = date,
      pop,
      values,
      inc,
      inc.raw = daily.inc.raw,
      ci.2wk,
      n.2wk,
      n.cum,
      spline,
      deriv1,
      category,
      cat2,
      numDaysDownward = DT,
      rebound) %>%
    dplyr::group_by(id) %>%
    dplyr::mutate(
      spline = ifelse(spline < 0, 0, spline),
      spline = ifelse(is.na(spline), 0, spline),
      deriv1 = ifelse(is.na(deriv1), 0, deriv1),
      pct.n.2wk = ifelse(n.cum > 0, 100 * (n.2wk / n.cum), 0)
    ) %>%
    dplyr::rename(daily.inc = values,
                  cum.inc = inc,
                  daily.ci.change = deriv1,
                  epi.curve.cat = category,
                  hbg.cat = cat2) %>%
    dplyr::ungroup()



  #stop new code, existing code remains below

  if (base.data == "Mortality") {
    colnames(cat3)[colnames(cat3) == "inc"] <- "mort"
    colnames(cat3)[colnames(cat3) == "ci.2wk"] <- "mort.2wk"
  }

  ds <- cat3 %>%
    dplyr::mutate(rate_cut = rate.cut,
                  slope_cut = slope.cut,
                  hb_cut = hb.cut,
                  datatype = base.data) %>%
    dplyr::mutate(date_text = as.character(format(dates, "%Y-%m-%d")))


  ds$epi.curve.cat[is.na(ds$epi.curve.cat)] <- "none" # These NAs happen in 3 counties after merging with the shapefile
  ds$epi.curve.cat <- as.character(ds$epi.curve.cat)

  #if a date is in the first 14 days, the algorithm does not calculate a trajectory category
  ds[1:14,]$epi.curve.cat<-"first 14 days"

  # For the map, rebound is combined with the epi curve status
  ds$epi.curve.map.cat <- ifelse(ds$rebound %in% c("rebound"), "rebound", ds$epi.curve.cat)

  ds$epi.curve.map.cat[is.na(ds$epi.curve.map.cat)] <- "none"

  ds$epi.curve.cat <- factor(ds$epi.curve.cat, levels = c("low incidence growth",
                                                          "elevated incidence growth",
                                                          "elevated incidence plateau",
                                                          "decline",
                                                          "low incidence plateau",
                                                          "lessthan5",
                                                          "nonepast2wk",
                                                          "none",
                                                          "first 14 days"))

  ds$epi.curve.map.cat <- factor(ds$epi.curve.map.cat, levels = c("first 14 days",
                                                                  "low incidence growth",
                                                                  "elevated incidence growth",
                                                                  "elevated incidence plateau",
                                                                  "decline",
                                                                  "low incidence plateau",
                                                                  "rebound",
                                                                  "lessthan5",
                                                                  "nonepast2wk",
                                                                  "none"))

  ds$growth.cat.labels <- as.character(ds$epi.curve.cat)

  ds$growth.cat.labels[ds$growth.cat.labels == "low incidence growth"] <- "Low incidence growth"
  ds$growth.cat.labels[ds$growth.cat.labels == "low incidence plateau"] <- "Low incidence plateau"
  ds$growth.cat.labels[ds$growth.cat.labels == "elevated incidence growth"] <- "Elevated incidence growth"
  ds$growth.cat.labels[ds$growth.cat.labels == "elevated incidence plateau"] <- "Elevated incidence plateau"

  ds$growth.cat.labels[ds$growth.cat.labels == "decline"] <- "Sustained decline"
  ds$growth.cat.labels[ds$growth.cat.labels == "none"] <- "No reported cases"
  ds$growth.cat.labels[ds$growth.cat.labels == "lessthan5"] <- "1-5 cases in the past two weeks"
  ds$growth.cat.labels[ds$growth.cat.labels == "nonepast2wk"] <- "0 cases in the past two weeks"
  ds$growth.cat.labels[ds$growth.cat.labels == "first 14 days"] <- "First 14 days of data"

  ds$growth.map.cat.labels <- as.character(ds$epi.curve.map.cat)
  ds$growth.map.cat.labels[ds$growth.map.cat.labels == "low incidence growth"] <- "Low incidence growth"
  ds$growth.map.cat.labels[ds$growth.map.cat.labels == "low incidence plateau"] <- "Low incidence plateau"
  ds$growth.map.cat.labels[ds$growth.map.cat.labels == "elevated incidence growth"] <- "Elevated incidence growth"
  ds$growth.map.cat.labels[ds$growth.map.cat.labels == "elevated incidence plateau"] <- "Elevated incidence plateau"
  ds$growth.map.cat.labels[ds$growth.map.cat.labels == "rebound"] <- "Rebound"

  ds$growth.map.cat.labels[ds$growth.map.cat.labels == "decline"] <- "Sustained decline"
  ds$growth.map.cat.labels[ds$growth.map.cat.labels == "none"] <- "No reported cases"
  ds$growth.map.cat.labels[ds$growth.map.cat.labels == "lessthan5"] <- "1-5 cases in the past two weeks"
  ds$growth.map.cat.labels[ds$growth.map.cat.labels == "nonepast2wk"] <- "0 cases in the past two weeks"
  ds$growth.map.cat.labels[ds$growth.map.cat.labels == "first 14 days"] <- "First 14 days of data"


  finaldf <- ds

  return(finaldf)
}
