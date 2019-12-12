########################################################################
#
# POPULATION_GLOBAL.R
# ======================================================================
#
# This script provides some global reusable functions and variables
#
# Author:   Gerry Henstra
# Date  :   December 2019
#
########################################################################

# function used to identify in other scripts
population_global <- function(){ UseMethod("population_global")}




###########################################################
# Data directories
###########################################################
DATA_DIRECTORY <- "data"



###########################################################
# Variables and Control Flags
###########################################################
DOWNLOAD_FROM_SOURCE <- TRUE # TRUE : download files from origin / #FALSE : use local file referenced in the link_ variable

FIRST_YEAR <-NULL
LAST_YEAR <- NULL
YEARS <- NULL

CURRENT_POPULATION <- NULL
PREVIOUS_POPULATION <- NULL
POPULATION_INCREASE <- NULL

TOP10_COUNTRIES_POPULATION <- NULL
TOP10_COUNTRIES_PERCENTAGE <- NULL

CHINA_POPULATION_INCREASE <- NULL
INDIA_POPULATION_INCREASE <- NULL
CHINA_PERCENT_INCREASE <- NULL
INDIA_PERCENT_INCREASE <- NULL

PREVIOUS_FERTILITY_RATE <- NULL
CURRENT_FERTILITY_RATE <- NULL

# Key location codes
TOP10 <- NULL
GLOBAL <- 900
CHINA <- 156
INDIA <- 356
USA <- 840


###########################################################
# Get current major and minor version numbers of local 
# R install
###########################################################
R_Major <- as.numeric(R.version$major)
R_Minor <- as.numeric(R.version$minor)



###########################################################
# Create a function to set the seed based on currently 
# running R version
#
# setSeed(s) : s = seed to be set to
###########################################################
setSeed <- function(s){
  if (R_Major >= 3 & R_Minor > 3.5) {
    set.seed(s, sample.kind="Rounding")
  } else {
    set.seed(s)
  }
}



###########################################################
# Root Squared Standard Error Routine
###########################################################
RMSE <- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings - predicted_ratings)^2))
}




###########################################################
# Round to nearest number
###########################################################
floor_n <- function(n, base){
  base * floor(n/base) 
}

ceiling_n <- function(n, base){
  if (n%%5 == 0) {n <- n + 1}
  base * ceiling(n/base) 
}
