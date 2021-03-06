% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_countries.R
\name{get_countries}
\alias{get_countries}
\title{get_countries}
\usage{
get_countries()
}
\description{
(Legacy) Get countries and their continents, ISO codes, WHO region, and total population.
Output is available through the package as "countries_data," but this function can be used to recreate this dataset.
To regenerate and make the data available again for the package, run the following in dev and rebuild package:
1. countries_data <- get_countries()
2. usethis::use_data(countries_data, overwrite=T)
}
\examples{
\dontrun{
countries_data <- get_countries()}
}
