#TODO!!!!
rfunctions.dir <- "https://raw.githubusercontent.com/CDCgov/ITF_Power_BI/master/Rfunctions/"


testthat::test_that("Countries table the same!", {

  fun_country <- dget(paste0(rfunctions.dir, "get_country.R"))
  old_df <- fun_country(rfunctions.dir)

  testthat::expect_true(compare::compare(old_df, countries_data)$result, TRUE)
})



testthat::test_that("Countries Date table the same!", {

  fun_country <- dget(paste0(rfunctions.dir, "get_country_date.R"))
  old_df <- fun_country(rfunctions.dir)

  testthat::expect_true(compare::compare(old_df, df_country_date())$result, TRUE)
})



testthat::test_that("JHU table the same!", {

  fun_country <- dget(paste0(rfunctions.dir, "get_country_date.R"))
  old_df <- fun_country(rfunctions.dir)

  testthat::expect_true(compare::compare(old_df, df_country_date())$result, TRUE)
})



testthat::test_that("JHU RT Table the same!", {

  fun_country <- dget(paste0(rfunctions.dir, "get_country_date.R"))
  old_df <- fun_country(rfunctions.dir)

  testthat::expect_true(compare::compare(old_df, calc_jhu_rt())$result, TRUE)
})



# Code that uses existing R functions to output CSVs for ITF Power BI Dashboard
library(compareDF)
library(readr)
# Path to all local R functions
rfunctions.dir <- "https://raw.githubusercontent.com/CDCgov/ITF_Power_BI/master/Rfunctions/"

#country data
fun_country <- dget(paste0(rfunctions.dir, "get_country.R"))
df_country1 <- fun_country(rfunctions.dir)
compareDF::compare_df(df_country, df_country1)

#index country and date
fun_country_date <- dget(paste0(rfunctions.dir, "get_country_date.R"))
df_country_date1 <- fun_country_date(rfunctions.dir, df_country1)
compareDF::compare_df(df_country_date, df_country_date1)

# get the base jhu and who dataframes
fun_jhu <- dget(paste0(rfunctions.dir, "get_jhu_data.R"))
df_jhu1 <- fun_jhu(rfunctions.dir, df_country_date)
fun_who <- dget(paste0(rfunctions.dir, "get_who_data.R"))
df_who1 <- fun_who(rfunctions.dir, df_country_date)
compareDF::compare_df(df_jhu, df_jhu1)
compareDF::compare_df(df_who, df_who1)

# get the base Rt jhu and Rt who dataframes
fun_rt_jhu <- dget(paste0(rfunctions.dir, "Rt_jhu.R"))
df_rt_jhu1 <- fun_rt_jhu(rfunctions.dir, df_jhu1)
fun_rt_who <- dget(paste0(rfunctions.dir, "Rt_who.R"))
df_rt_who1 <- fun_rt_who(rfunctions.dir, df_who1)
compareDF::compare_df(df_rt_jhu, df_rt_jhu1)
compareDF::compare_df(df_rt_who, df_rt_who1)

fun_ncov <- dget(paste0(rfunctions.dir, "get_ncov_data.R"))
df_ncov1 <- fun_ncov(rfunctions.dir, df_jhu1, df_who1)
compareDF::compare_df(df_ncov, df_ncov1)

# Getting Rt dataset
fun_Rt <- dget(paste0(rfunctions.dir, "Rt_ncov.R"))
df_rt_ncov1 <- fun_Rt(rfunctions.dir, df_rt_jhu1, df_rt_who1)

compareDF::compare_df(df_rt_ncov, df_rt_ncov1)

#getting trajectory data
fun_traj <- dget(paste0(rfunctions.dir, "trajectory_function_final_newalgo.R"))
hotspot_data1 <- fun_traj("z", rfunctions.dir, df_ncov1)
hotspot_map1 <- fun_traj("map", rfunctions.dir, df_ncov1)
compareDF::compare_df(hotspot_data, hotspot_data1)
compareDF::compare_df(hotspot_map, hotspot_map1)


#testing data
fun_tst <- dget(paste0(rfunctions.dir, "covid_testing.R"))
testing_data1 <- fun_tst("long", rfunctions.dir, df_country)
testing_cross1 <- fun_tst("cross", rfunctions.dir, df_country)
compareDF::compare_df(testing_data, testing_data1)
compareDF::compare_df(testing_cross, testing_cross1)


df_gmob_raw1 <- read.csv("https://www.gstatic.com/covid19/mobility/Global_Mobility_Report.csv", encoding="UTF-8")
compareDF::compare_df(gmob[[1]], df_gmob_raw1)

# Getting google mobility dataset
fun_gmob <- dget(paste0(rfunctions.dir, "gmob.R"))
gmob1 <- fun_gmob(rfunctions.dir, df_country1, df_gmob_raw1)
compareDF::compare_df(gmob[[2]], gmob1)


#risk matrix data
fun_risk <- dget(paste0(rfunctions.dir, "get_riskmatrix.R"))
riskmatrix1 <- fun_risk("date", rfunctions.dir, df_ncov1, df_rt_ncov1)
xriskmatrix1 <- fun_risk("cross", rfunctions.dir, df_ncov1, df_rt_ncov1)
compareDF::compare_df(riskmatrix, riskmatrix1)
compareDF::compare_df(xriskmatrix, xriskmatrix1)


#risk matrix_v2 data
fun_risk <- dget(paste0(rfunctions.dir, "get_riskmatrix_v2.R"))
riskmatrix_v21 <- fun_risk("date", rfunctions.dir, df_ncov1)
xriskmatrix_v21 <- fun_risk("cross", rfunctions.dir, df_ncov1)
compareDF::compare_df(riskmatrix_v2, riskmatrix_v21)
compareDF::compare_df(xriskmatrix_v2, xriskmatrix_v21)


#risk get_riskmatrix_v3_KM
fun_risk <- dget(paste0(rfunctions.dir, "get_riskmatrix_v3_KM.R"))
riskmatrix_v31 <- fun_risk("date", rfunctions.dir, df_ncov1)
xriskmatrix_v31 <- fun_risk("cross", rfunctions.dir, df_ncov1)
compareDF::compare_df(riskmatrix_v3, riskmatrix_v31)
compareDF::compare_df(xriskmatrix_v3, xriskmatrix_v31)


#who criteria data
fun_criteria <- dget(paste0(rfunctions.dir, "get_who_criteria.R"))
who_criteria1 <- fun_criteria(rfunctions.dir, df_ncov1, testing_data1, df_rt_ncov1)
compareDF::compare_df(who_criteria, who_criteria1)


#vaccine data
fun_vax <- dget(paste0(rfunctions.dir, "get_vax_data.R"))
vax_dict1 <- fun_vax(rfunctions.dir)
compareDF::compare_df(vaccinations_all, vax_dict1$all)
compareDF::compare_df(vaccinations_manufacturers, vax_dict1$manufacturers)
compareDF::compare_df(vaccinations_rollout, vax_dict1$rollout)
compareDF::compare_df(vaccinations_categories, vax_dict1$categories)

# overlay data
fun_overlay <- dget(paste0(rfunctions.dir, "get_country_overlays.R"))
overlay_dict1 <- fun_overlay(rfunctions.dir, df_ncov1, gmob[[1]])
compareDF::compare_df(overlay_cases_deaths, overlay_dict1$cases_deaths)
compareDF::compare_df(overlay_stringency, overlay_dict1$stringency)
compareDF::compare_df(overlay_mobility, overlay_dict1$mobility)
