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

  df <- ggplot2::ggplot(data    = df,
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
    ggplot2::theme(plot.title   = ggplot2::element_text(size = 18, face = "bold"),
                   axis.text    = ggplot2::element_text(size = 8),
                   axis.title   = ggplot2::element_text(size = 10),
                   legend.title = ggplot2::element_text(size = 12, face = "bold"),
                   legend.text  = ggplot2::element_text(size = 9)) +
    ggplot2::guides(fill = ggplot2::guide_legend(overide.aex = list(size = 9)))

  if(transparent == T){
    return(df +
             ggplot2::theme(panel.background  = ggplot2::element_rect(fill = "transparent"),
                            plot.background   = ggplot2::element_rect(fill = "transparent"),
                            panel.grid        = ggplot2::element_blank(),
                            legend.background = ggplot2::element_rect(fill = "transparent")))
  }else{
    return(df)
  }

}
