########################################################################
#
# POPULATION_PACKAGES.R
# ======================================================================
#
# Import and load the required libraries for the project.
#
# Author:     Gerry Henstra
# Date:       December 2019
#
########################################################################



# function used to identify in other scripts
population_packages <- function(){}



###########################################################
# Install required packages if missing
###########################################################
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(readr)) install.packages("readr")
if(!require(readxl)) install.packages("readxl")
if(!require(dplyr)) install.packages("dplyr")
if(!require(ggplot2)) install.packages("ggplot2")
if(!require(rnaturalearth)) install.packages("rnaturalearth")



###########################################################
# Load Libraries
###########################################################
library(tidyverse)
library(data.table)
library(readr)
library(readxl)
library(dplyr)
library(ggplot2)
library(stringr)
library(rnaturalearth)

