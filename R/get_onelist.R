#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' @title get_onelist
#' @description One list to rule them all and in keys bind them!
#' WB Income list <- Modified WHO COVID reporting list <- UN Pop Estimates
#'
#' @importFrom magrittr `%>%`
#'
#' @export

get_onelist <- function(){

  pages  <- as.numeric(jsonlite::fromJSON(rawToChar(httr::GET("http://api.worldbank.org/v2/country?format=json")$content))[[1]]$pages)

  wb <- data.frame()

  for (p in 1:pages){
    res  <- httr::GET(paste0("http://api.worldbank.org/v2/country?format=json&page=", p))
    data <- jsonlite::fromJSON(rawToChar(res$content), flatten = T)[[2]]
    wb   <- dplyr::bind_rows(wb, data)
  }

  df <- full_join(wb,
                  country_list <- get_covid_df() %>%
                    dplyr::select(who_region, region, country, country_code) %>%
                    unique(),
                  by = c("iso2Code" = "country_code")) %>%
    dplyr::rename_all(tolower) %>%
    dplyr::filter(region.value != "Aggregates" | is.na(region.value)) %>%
    dplyr::select(id,
                  iso2code,
                  wb_country = name,
                  wb_region_name = region.value,
                  incomelevel_id = incomelevel.id,
                  incomelevel_value = incomelevel.value,
                  who_region,
                  who_region_name = region,
                  who_country = country)
}
#write.table(df, "clipboard-16384", sep="\t", row.names=FALSE, na = "")


