########################################################################
#
# POPULATION_MAIN.R
# ======================================================================
#
# This is the main script that controls the flow of the processing.
# 
# External scripts contain related code that this script imports and
# executes.
#
# Author:     Gerry Henstra
# Date:       December 2019
#
########################################################################



###########################################################
#
# Load External Scripts
#
###########################################################
if(!exists("population_packages", mode="function")) source("population_packages.R")
if(!exists("population_global", mode="function")) source("population_global.R")
if(!exists("population_data", mode="function")) source("population_data.R")
if(!exists("population_plots", mode="function")) source("population_plots.R")
if(!exists("population_prediction", mode="function")) source("population_prediction.R")




###########################################################
#
# Load Datasets
#
###########################################################

# use the global variable DOWNLOAD_FROM_SOURCE found in population_global.r to
# control whether files are downloaded from the original source from the internet
# or local copies are used

population_data.import_data()



