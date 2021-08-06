
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

## COVAD - COVID Analytical Datasets

| Metadata Functions | Get Functions       | Calculation Functions | Production Functions |
|--------------------|---------------------|-----------------------|----------------------|
| one\_table         | get\_covid\_sources | calc\_add\_risk       | prod\_sitrep         |
| country\_coords    | get\_vaccinations   |                       |                      |
|                    | get\_testing        |                       |                      |

## COVAC - COVID Analytical Checks

## COVIS - COVID Visualizations

| Plot Functions              | Map Functions     | Table Functions        |
|-----------------------------|-------------------|------------------------|
| plot\_epicurve              | map\_template     | table\_10mostcases     |
| plot\_epicurve\_ind         | map\_burden       | table\_10mostincidence |
| plot\_epicurve\_epidouble   | map\_trend        | table\_10percentchange |
| plot\_epicurve\_dailydouble | map\_vaccinations | table\_10vaccinations  |
| plot\_riskmatrix            |                   |                        |
| plot\_vaxcoverage           |                   |                        |
