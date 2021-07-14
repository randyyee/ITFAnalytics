#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' @title plot_epicurve
#' @description Visualize epi curve by epi-weeks (Monday-Sunday).
#' Default viz for WHO regions.
#' @param df A dataframe with the following: region, country, date, new_cases.
#' For WHO default, region should be factors with levels of AMRO, EURO, SEARO, EMRO, AFRO, and WPRO.
#' Produces an epi curve, region stacked bar plot for each epi-week (Monday-Sunday).
#' @param transparent Default TRUE - returns a transparent plot.
#' @importFrom magrittr `%>%`
#'
#' @export

plot_epicurve <- function(df, transparent = T){

  who.col.pal  <- c("#aa001e", "#e7b351", "#00818a", "#d26230", "#005e70", "#d4ece8")
  regions      <- c("Americas", "Europe", "Southeast Asia", "Eastern \nMediterranean", "Africa", "Western Pacific")
  region_label <- "WHO Region"
  gtitle       <- "Confirmed COVID-19 cases by week of report and WHO region"

  g <- ggplot2::ggplot(data     = df,
                       mapping = aes(x    = lubridate::floor_date(date, "week", week_start = 1),
                                     y    = new_cases,
                                     fill = region)) +
    ggplot2::geom_bar(position = "stack",
                      stat     = "identity",
                      alpha    = 0.9) +
    ggplot2::theme_classic() +
    ggplot2::scale_fill_manual(values = who.col.pal,
                               labels = regions) +
    ggplot2::ylab("Weekly Cases") +
    ggplot2::xlab("Week of Reporting") +
    ggplot2::labs(fill = region_label) +
    ggplot2::scale_x_date(limits = c(lubridate::floor_date(min(df$date, na.rm = T)-7, "week", week_start = 1),
                                     lubridate::floor_date(max(df$date, na.rm = T)+7, "week", week_start = 1)),
                          breaks = seq.Date(from = as.Date(lubridate::floor_date(min(df$date, na.rm = T), "week", week_start = 1)),
                                            to   = as.Date(lubridate::floor_date(max(df$date, na.rm = T)+7, "week", week_start = 1)),
                                            by   = "3 weeks"),
                          date_labels = "%d\n%b",
                          expand      = c(0, 0)) +
    ggplot2::scale_y_continuous(expand = c(0, 5000),
                                labels = scales::comma) +
    ggplot2::labs(title    = gtitle,
                  subtitle = paste0(format(min(df$date, na.rm = T), "%B %d, %Y"), " - ",
                                    format(max(df$date, na.rm = T), "%B %d, %Y"))) +
    ggplot2::theme(plot.title   = ggplot2::element_text(size  = 18, face = "bold"),
                   axis.text    = ggplot2::element_text(size  = 8),
                   axis.title   = ggplot2::element_text(size  = 10),
                   legend.title = ggplot2::element_text(size  = 12, face = "bold"),
                   legend.text  = ggplot2::element_text(size  = 9)) +
    ggplot2::guides(fill = ggplot2::guide_legend(overide.aex  = list(size = 9)))

  if(transparent == T){
    return(g +
             ggplot2::theme(panel.background  = ggplot2::element_rect(fill = "transparent"),
                            plot.background   = ggplot2::element_rect(fill = "transparent"),
                            panel.grid        = ggplot2::element_blank(),
                            legend.background = ggplot2::element_rect(fill = "transparent")))
  }else{
    return(g)
  }

}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' @title plot_epicurve_ind
#' @description Visualize epi curve by cases and deaths.
#' Default viz for individual countries.
#'
#' @param df A dataframe with the following: country, date, cases and/or deaths
#'
#' @param type Default cases.
#'
#' @param incidence Default TRUE. Specify inputs are incidence values or not.
#'
#' @importFrom magrittr `%>%`
#'
#' @export

plot_epicurve_ind <- function(df, type = "cases", incidence = T){

  if(!type %in% c("cases", "deaths")){
    stop("Wrong Type! You must use either cases or deaths!")
  }

  if(!incidence %in% c(T, F)){
    stop("Wrong Incidence! You must use either TRUE or FALSE!")
  }

  if(incidence == F){
    df %>%
      ggplot2::ggplot(aes(x = date, y = if(type == "cases") {cases} else {deaths})) +
      ggplot2::geom_bar(stat = "identity", alpha = 0.9, fill = if(type == "cases") {"dodgerblue4"} else {"red4"}) +
      ggplot2::theme_classic() +
      ggplot2::ylab(if(type == "cases") {"Daily Cases"} else {"Daily Deaths"}) +
      ggplot2::xlab("Date of Reporting") +
      ggplot2::scale_x_date(breaks       = c(by = "3 weeks"),
                            date_labels  = "%d\n%b") +
      ggplot2::scale_y_continuous(labels = comma) +
      ggplot2::labs(title    = if(type == "cases") {paste0("COVID-19 Cases: ", unique(df$country))} else {paste0("COVID-19 Deaths:", unique(df$country))},
                    subtitle = paste0(format(min(df$date, na.rm = T), "%B %d, %Y"), " - ", format(max(df$date, na.rm = T), "%B %d, %Y"))) +
      ggplot2::theme(plot.title   = ggplot2::element_text(size = 14, face = "bold"),
                     axis.text    = ggplot2::element_text(size = 8),
                     axis.title   = ggplot2::element_text(size = 10),
                     legend.title = ggplot2::element_text(size = 12, face = "bold"),
                     legend.text  = ggplot2::element_text(size = 9))
  } else {
    df %>%
      ggplot2::ggplot(aes(x = date, y = if(type == "cases") {cases} else {deaths})) +
      ggplot2::geom_bar(stat = "identity", alpha = 0.9, fill = if(type == "cases") {"dodgerblue4"} else {"red4"}) +
      ggplot2::theme_classic() +
      ggplot2::ylab(if(type == "cases") {"Daily Cases per 100,000 People"} else {"Daily Deaths per 100,000 People"}) +
      ggplot2::xlab("Date of Reporting") +
      ggplot2::scale_x_date(breaks       = c(by = "3 weeks"),
                            date_labels  = "%d\n%b") +
      ggplot2::scale_y_continuous(labels = comma) +
      ggplot2::labs(title    = if(type == "cases") {paste0("COVID-19 Cases per 100,000 People: ", unique(df$country))} else {paste0("COVID-19 Deaths per 100,000 People: ", unique(df$country))},
                    subtitle = paste0(format(min(df$date, na.rm = T), "%B %d, %Y"), " - ", format(max(df$date, na.rm = T), "%B %d, %Y"))) +
      ggplot2::theme(plot.title   = ggplot2::element_text(size = 14, face = "bold"),
                     axis.text    = ggplot2::element_text(size = 8),
                     axis.title   = ggplot2::element_text(size = 10),
                     legend.title = ggplot2::element_text(size = 12, face = "bold"),
                     legend.text  = ggplot2::element_text(size = 9))
  }
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' @title plot_epicurve_double
#' @description Visualize epi curve by cases and deaths.
#' Default viz for individual countries.
#'
#' @param df A dataframe with the following: country, date, cases and deaths
#'
#'
#' @importFrom magrittr `%>%`
#'
#' @export

plot_epicurve_double <- function(df){

  ylim.prim <- c(min(df$case, na.rm = T),
                 max(df$case, na.rm = T))

  ylim.sec  <- c(min(df$death, na.rm = T),
                 max(df$death, na.rm = T))

  b <- diff(ylim.prim)/diff(ylim.sec)
  a <- ylim.prim[1] - b * ylim.sec[1]

  ggplot2::ggplot(df) +
    ggplot2::geom_bar(aes(x = weekdate, y = case, color = "Cases"), stat = "identity", alpha = 0.9, fill = "lightblue") +
    ggplot2::geom_line(aes(x = weekdate, y = a + death * b, group = 1, color = "Deaths"), size = 1) +
    ggplot2::scale_color_manual(breaks = c("Cases", "Deaths"),
                                values = c("lightblue", "red")) +
    ggplot2::theme_classic() +
    ggplot2::scale_x_date(breaks       = c(by = "4 weeks"),
                          date_labels  = "%d\n%b") +
    ggplot2::scale_y_continuous("Weekly Cases", labels = comma,
                                sec.axis = sec_axis(~ (. - a)/b, name = "Weekly Deaths", labels = comma)) +
    ggplot2::xlab("Date of Reporting") +
    ggplot2::labs(title    = paste0("COVID-19: ", unique(df$country)),
                  subtitle = paste0("Week of:", format(min(df$weekdate, na.rm = T), "%B %d, %Y"), " - ", format(max(df$weekdate, na.rm = T), "%B %d, %Y"))) +
    ggplot2::theme(plot.title      = ggplot2::element_text(size = 14, face = "bold"),
                   axis.text       = ggplot2::element_text(size = 8),
                   axis.title      = ggplot2::element_text(size = 12),
                   legend.position = "top",
                   legend.key      = element_blank(),
                   legend.title    = ggplot2::element_blank(),
                   legend.text     = ggplot2::element_text(size = 9))  +
    ggplot2::guides(color          = ggplot2::guide_legend(override.aes = list(fill = c("lightblue", NA))))
}
