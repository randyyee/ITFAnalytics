#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' @title table_countriesofconcern
#' @description Table for displaying stats for list of countries of concern.
#' @param df_risk A dataframe with the following: country, date, new_cases, week_case_incidence, week_case, prev_week_case, percent_change_case,
#' new_deaths, week_death_incidence, week_death, prev_week_death, percent_change_death
#' @param df_vaccinations A dataframe with the following: location, date, people_vaccinated_per_hundred, total_vaccinations_per_hundred
#' @param df_manufacturers A dataframe with the following: location, date, vaccines
#' @importFrom magrittr `%>%`
#'
#' @export

table_countriesofconcern <- function(df_risk, df_vaccinations, df_vaccinations_maufacturers, country_list){

  as.data.frame(
    t(
      dplyr::filter(df_risk, country %in% country_list) %>%
        dplyr::filter(date == max(date)) %>%
        dplyr::mutate(Country                                   = country,
                      Date                                      = date,
                      `New Cases (Incidence per 100,000)`       = paste0(comma(round(new_cases)), " (", round(week_case_incidence, 2),")"),
                      `7 Day Cases`                             = scales::comma(round(week_case)),
                      `Previous 7 Day Cases`                    = scales::comma(round(prev_week_case)),
                      `% Change in Cases from Previous 7 Days`  = scales::percent(percent_change_case, scale = 1),
                      `New Deaths (Incidence per 100,000)`      = paste0(comma(round(new_deaths)), " (", round(week_death_incidence, 2),")"),
                      `7 Day Deaths`                            = scales::comma(round(week_death)),
                      `Previous 7 Day Deaths`                   = scales::comma(round(prev_week_death)),
                      `% Change in Deaths from Previous 7 Days` = percent(percent_change_death, scale = 1)) %>%
        dplyr::select(Country:`% Change in Deaths from Previous 7 Days`) %>%
        dplyr::left_join(
          dplyr::filter(df_vaccinations, location %in% country_list) %>%
            dplyr::group_by(location) %>%
            dplyr::filter(date == max(date)) %>%
            dplyr::mutate(`Most Recent Date for Vaccinations` = date,
                          `People Vaccinated Per 100 People`  = people_vaccinated_per_hundred,
                          `Total Vaccinations Per 100 People` = total_vaccinations_per_hundred) %>%
            dplyr::select(location,`Most Recent Date for Vaccinations`:`Total Vaccinations Per 100 People`), by = c("Country" = "location")
        ) %>%
        dplyr::left_join(
          dplyr::filter(df_vaccinations_maufacturers, location %in% country_list) %>%
            dplyr::group_by(location) %>%
            dplyr::filter(last_observation_date == max(last_observation_date)) %>%
            dplyr::mutate(`Vaccines in Use` = vaccines,
                          `% Delta` = "",
                          `Variant Comments` = "") %>%
            dplyr::select(location, `Vaccines in Use`, `% Delta`, `Variant Comments`), by = c("Country" = "location")
        )
    ))  %>%
    tibble::rownames_to_column(" ") %>%
    purr::set_names(.[1,]) %>%
    dplyr::filter(Country != "Country") %>%
    flextable::flextable()

}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' @title table_10mostcases
#' @description Table for displaying top 10's.
#' @param df A dataframe with the following and in this order: country, value1 - cases, value2 - percent change
#' @importFrom magrittr `%>%`
#'
#' @export

table_10mostcases <- function(df){
  gt(df) %>%
    gt::tab_header(title        = gt::html(paste0("<b>10 Countries/ Areas with Most New Cases", "</b>"))) %>%
    gt::data_color(columns      = vars(value2),
                   colors       = scales::col_bin(
                     palette    = c("#1f9fa9", "#c0ebec", "#e57e51", "#d26230", "#c92929", "#7c0316"),
                     bins       = c(-Inf, -50, 0, 50, 100, 200, Inf))) %>%
    gt::fmt_number(columns      = vars(value1),
                   sep_mark     = ",",
                   decimals     = 0)  %>%
    gt::fmt_number(columns      = vars(value2),
                   decimals     = 1) %>%
    gt::cols_label(country = gt::html("Country/ Area"),
                   value1  = gt::html("New Cases<br>This Week"),
                   value2  = gt::html("% Change<br>Last Week")) %>%
    gt::cols_align("center") %>%
    gt::cols_width(vars(country) ~ gt::px(175),
                   vars(value1)  ~ gt::px(100),
                   vars(value2)  ~ gt::px(100)) %>%
    gt::tab_options(table.width               = gt::px(400),
                    column_labels.font.weight = "bold",
                    table.font.weight         = "bold",
                    footnotes.font.size       = gt::pct(70),
                    source_notes.font.size    = gt::pct(70),
                    source_notes.padding      = 0,
                    footnotes.padding         = 0) %>%
    gt::tab_source_note(source_note = gt::md("Data Source: WHO Coronavirus Disease (COVID-19) Dashboard ")) %>%
    gt::tab_source_note(source_note = paste0("Data as of ", format(max(df$date), "%B %d, %Y"))) %>%
    gt::tab_footnote(footnote       = "Percent change in cases of most recent 7 days to 7 days prior",
                     locations      = cells_column_labels(columns = vars(value2)))
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' @title table_10incidence
#' @description Table for displaying top 10's.
#' @param df A dataframe with the following and in this order: country, value1 - incidence, value2 - percent change, date
#' @importFrom magrittr `%>%`
#'
#' @export

table_10incidence <- function(df){
  gt(df) %>%
    gt::tab_header(title   = gt::html(paste0("<b>10 Countries/ Areas with Highest Incidence", "</b>"))) %>%
    gt::data_color(columns = vars(value2),
                   colors  = scales::col_bin(palette = c("#1f9fa9", "#c0ebec", "#e57e51", "#d26230", "#c92929", "#7c0316"),
                                             bins    = c(-Inf, -50, 0, 50, 100, 200, Inf))) %>%
    gt::fmt_number(columns  = vars(value2),
                   decimals = 1) %>%
    gt::fmt_number(columns  = vars(value1),
                   sep_mark = ",",
                   decimals = 0)  %>%
    gt::cols_label(country = gt::html("Country/ Area"),
                   value1  = gt::html("New Cases<br>This Week"),
                   value2  = gt::html("% Change<br>Last Week")) %>%
    gt::cols_align("center") %>%
    gt::cols_width(vars(country) ~ gt::px(175),
                   vars(value1)  ~ gt::px(100),
                   vars(value2)  ~ gt::px(100)) %>%
    gt::tab_options(table.width               = gt::px(400),
                    column_labels.font.weight = "bold",
                    table.font.weight         = "bold",
                    footnotes.font.size       = gt::pct(70),
                    source_notes.font.size    = gt::pct(70),
                    source_notes.padding      = 0,
                    footnotes.padding         = 0) %>%
    gt::tab_source_note(source_note = gt::md("Data Source: WHO Coronavirus Disease (COVID-19) Dashboard ")) %>%
    gt::tab_source_note(source_note = paste0("Data as of ", format(max(df$date), "%B %d, %Y"))) %>%
    gt::tab_footnote(footnote       = "Percent change in cases of most recent 7 days to 7 days prior",
                     locations      = cells_column_labels(columns = vars(value2)))

}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' @title table_10percentchange
#' @description Table for displaying top 10's.
#' @param df A dataframe with the following and in this order: country, value1 - last week percent change, value2 - 4 week percent change, date
#' @importFrom magrittr `%>%`
#'
#' @export

table_10percentchange <- function(df){
  gt(df) %>%
    gt::tab_header(title   = gt::html(paste0("<b>10 Countries/ Areas with <br> Highest Percent Change Last Week", "</b>"))) %>%
    gt::data_color(columns = vars(value1),
                   colors  = scales::col_bin(palette = c("#1f9fa9", "#c0ebec", "#e57e51", "#d26230", "#c92929", "#7c0316"),
                                             bins    = c(-Inf, -50, 0, 50, 100, 200, Inf))) %>%
    gt::data_color(columns = vars(value2),
                   colors  = col_bin(palette = c("#1f9fa9", "#c0ebec", "#e57e51", "#d26230", "#c92929", "#7c0316"),
                                     bins    = c(-Inf, -50, 0, 50, 100, 200, Inf))) %>%
    gt::fmt_number(columns  = vars(value1, value2),
                   decimals = 1) %>%
    gt::cols_label(country = gt::html("Country/ Area"),
                   value1 = gt::html("% Change<br>Last Week"),
                   value2 = gt::html("% Change<br> 4 Weeks")) %>%
    gt::cols_align("center") %>%
    gt::cols_width(vars(country) ~ gt::px(175),
                   vars(value1)  ~ gt::px(125),
                   vars(value2)  ~ gt::px(125)) %>%
    gt::tab_options(table.width               = gt::px(400),
                    column_labels.font.weight = "bold",
                    table.font.weight         = "bold",
                    footnotes.font.size       = pct(70),
                    source_notes.font.size    = pct(70),
                    source_notes.padding      = 0,
                    footnotes.padding         = 0) %>%
    gt::tab_source_note(source_note = "Note: Countries with population size less than 10 million were excluded.") %>%
    gt::tab_source_note(source_note = gt::md("Data Source: WHO Coronavirus Disease (COVID-19) Dashboard ")) %>%
    gt::tab_source_note(source_note = paste0("Data as of ", format(max(df$date), "%B %d, %Y"))) %>%
    gt::tab_footnote(footnote  = "Percent change in cases of most recent 7 days to 7 days prior",
                     locations = cells_column_labels(columns = vars(value1)))  %>%
    gt::tab_footnote(footnote  = "Percent change in cases of most recent 7 days to 4 weeks prior",
                     locations = cells_column_labels(columns = vars(value2)))
}
