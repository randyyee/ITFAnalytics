
<!-- README.md is generated from README.Rmd. Please edit that file -->

# ITFAnalytics

<!-- badges: start -->
<!-- badges: end -->

The goal of ITFAnalytics is to collect the functions needed to access
and prepare various COVID data sources.

This package is divided into two main sections: analytical datasets and
data visualizations.

## Installation

You can install the released version of ITFAnalytics from
[Github](https://github.com/randyyee/ITFAnalytics) with:

``` r
devtools::install_github("randyyee/ITFAnalytics")
```

## Legacy COVAD - COVID Analytical Datasets

| Metadata Functions  | Get Functions       | Calculation Functions | Production Functions |
|---------------------|---------------------|-----------------------|----------------------|
| get\_countries      | get\_jhu\_covid     | calc\_jhu\_rt         | prod\_dashboard      |
| add\_country\_dates | get\_who\_covid     | calc\_who\_rt         |                      |
|                     | get\_covid\_sources | calc\_rts             |                      |
|                     | get\_vaccinations   | calc\_riskmatrix      |                      |
|                     | get\_testing        | calc\_riskmatrix\_v2  |                      |
|                     | get\_googlemobility | calc\_riskmatrix\_v3  |                      |
|                     |                     | view\_countryoverlays |                      |

## COVIS - COVID Visualizations

| Plot Functions | Map Functions     | Table Functions        |
|----------------|-------------------|------------------------|
| plot\_epicurve | map\_template     | table\_10mostcases     |
|                | map\_burden       | table\_10mostincidence |
|                | map\_trend        | table\_10percentchange |
|                | map\_vaccinations |                        |
