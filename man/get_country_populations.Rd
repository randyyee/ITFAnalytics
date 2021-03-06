% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_countries.R
\name{get_country_populations}
\alias{get_country_populations}
\title{get_country_populations}
\usage{
get_country_populations()
}
\description{
Get country populations
get_covid_df() matched with UN Estimates for 2020. Remainders matched with CIA Factbook (Guernsey, Jersey, Pitcairn Islands, Kosovo).
Output is available through the package as "ountry_populations," but this function can be used to recreate this dataset.
To regenerate and make the data available again for the package, run the following in dev and rebuild package:
1. country_populations <- get_country_populations()
2. usethis::use_data(country_populations, overwrite=T)
}
\examples{
\dontrun{
country_populations <- get_country_populations()}
}
