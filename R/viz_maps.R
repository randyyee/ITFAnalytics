#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' @title map_template
#' @description Cross-sectional map.
#' @param df A dataframe with the following: country, geometry, date, result = factor value
#' @param world A dataframe with the following: geometry
#' @param category_color_labels List of labels that should map to the factor values of the df
#' @param category_color_values List of color values for mapping the labels. Needs to have the same length as category_color_labels!
#' @importFrom magrittr `%>%`
#'
#' @export

map_template <- function(df, world, category_color_labels, category_color_values){

  if(length(category_color_labels) != length(category_color_values)){
    stop("Your category labels are of different lengths!")
  }

  ggplot2::ggplot(df) + # Param
    ggplot2::geom_sf(data = world, # Param
                     aes(geometry = geometry),
                     fill = "grey99",
                     size = 0.3) +
    ggplot2::geom_sf(data = df,
                     aes(geometry = geometry,
                         fill = result), # Param
                     size = 0.2) +
    ggplot2::theme_void() +
    ggplot2::scale_fill_manual(values       = category_color_values, # Param
                               na.value     = "grey99",
                               drop         = F,
                               labels       = category_color_labels, # Param
                               na.translate = F) +
    ggplot2::theme(plot.title            = ggplot2::element_text(size = 18, face="bold"),
                   plot.subtitle         = ggplot2::element_text(size = 12),
                   plot.caption          = ggplot2::element_text(size = 8),
                   plot.caption.position = "plot",
                   legend.position       = c(0.02, 0.00),
                   legend.justification  = c("left", "bottom"),
                   legend.box.just       = "left",
                   legend.margin         = ggplot2::margin(3, 3, 3, 3),
                   legend.title          = ggplot2::element_text(size = 10),
                   legend.text           = ggplot2::element_text(size = 8))

}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' @title map_burden
#' @description Cross-sectional map: Average daily incidence for the past 7 days for each country.
#' @param df A dataframe with the following: region, country, date, incidence as 4-level factors (0- <1, 1- <10, 10- <25, 25+)
#' Produces an epi curve, region stacked bar plot for each epi-week (Monday-Sunday).
#' Input df SHOULD ONLY HAVE ONE DATE!
#' @param world A dataframe with the following: admin, geometry, iso3code
#' @importFrom magrittr `%>%`
#'
#' @export

map_burden <- function(df, world){

  if(length(unique(df$date))){
    warning("Your dataframe has more than 1 date! This is a cross-sectional visualization!")
  }

  cat_labs <- c("0- <1", "1- <10", "10- <25", "25+")
  cat_vals <- c("#f1e5a1", "#e7b351", "#d26230", "#aa001e")

  map_template(df, world, cat_labs, cat_vals) +
    ggplot2::labs(fill = "Average Daily \nIncidence \n(past 7 days) \nper 100,000") +
    ggplot2::labs(title = "Burden",
                  subtitle = paste0("Average daily incidence over the past 7 days per 100,000 population, ", format(max(df$date), "%B %d, %Y")))
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' @title map_trend
#' @description Cross-sectional map: Average daily incidence for the past 7 days for each country.
#' @param df A dataframe with the following: region, country, date, percent_change as 6-level factors (0- <1, 1- <10, 10- <25, 25+).
#' Input df SHOULD ONLY HAVE ONE DATE!
#' @param world A dataframe with the following: admin, geometry, iso3code
#' @importFrom magrittr `%>%`
#'
#' @export

map_trend <- function(df, world){

  if(length(unique(df$date))){
    warning("Your dataframe has more than 1 date! This is a cross-sectional visualization!")
  }

  cat_labs <- c(">=50% decrease", "0 - <50% decrease", ">0 - <=50% increase", ">50 - <=100% increase", ">100 - <=200% increase", ">200% increase")
  cat_vals <- c("#1f9fa9", "#c0ebec", "#e57e51", "#d26230", "#c92929", "#7c0316")

  map_template(df, world, cat_labs, cat_vals) +
    ggplot2::labs(fill = "Percent \nChange From \nPrevious Week")+
    ggplot2::labs(title = "Recent Trends",
                  subtitle = paste0("Percent change in cases from 7-day period ending ", format((unique(df$date)), "%B %d, %Y"), '\ncompared to previous 7-day period ending ', format(max(who_long1$Date)-7, "%B %d, %Y")))
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' @title map_vaccinations
#' @description Cross-sectional map: People vaccinated per 100 for each country.
#' @param df A dataframe with the following: region, country, date, percent_change AS 6-level factors (<1 1- <3, 3- <10, 10 -<30, 30+).
#'
#' @param world A dataframe with the following: admin, geometry, iso3code
#' @importFrom magrittr `%>%`
#'
#' @export

map_vaccinations <- function(df, world){

  cat_vals = c("#d4ece8","#a2d9d2", "#1f9fa9", "#005e70", "#27343a")
  cat_labs = c("<1", "1- <3", "3- <10", "10- <30", "30+")

  map_template(df, world, cat_labs, cat_vals) +
    labs(title    = paste0("People Vaccinated per 100 People, ", format(max(df$date), "%B %d, %Y")),
         subtitle = "Number of people out of 100 who received at least one vaccine dose; does not represent percent of \npopulation fully vaccinated",
         caption  = "Note:
       -Countries in white do not have data reported for total people vaccinated
       -Vaccine data are incomplete and data may be out of date")
}
