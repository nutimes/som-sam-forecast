################################################################################
###                   TIME SERIES FORECASTING WORKFLOW                       ###
################################################################################

## ---- Load required libraries ------------------------------------------------
library(tidyr)
library(dplyr)
library(lubridate)
library(tsibble)
library(feasts)
library(ggplot2)
library(fable)
library(fabletools)
library(sf)
library(cyphr)
library(rsample)

## ---- Retrieve secret key ----------------------------------------------------
secret_key <- data_key(".", Sys.getenv("path_secret_key"))

## ---- Read project-specific functions ----------------------------------------
lapply(list.files(path = "R", full.names = TRUE), FUN = source)

## ---- Read data in -----------------------------------------------------------
source("scripts/read-in-data.R")

## ---- Data wrangling ---------------------------------------------------------
source("scripts/data-wrangling.R")

## ---- Exploratory Data Analysis ----------------------------------------------
source("scripts/exploratory-data-analysis.R")

## ---- Decomposition ----------------------------------------------------------
source("scripts/decomposition.R")

## ---- Split input data into training and test set ----------------------------
source("scripts/split-training-test-data.R")

## ---- Forecasts --------------------------------------------------------------
source("scripts/arima-training-test-forecast-pastoral.R")
source("scripts/arima-training-test-forecast-agropastoral.R")
source("scripts/arima-training-test-forecast-riverine.R")
