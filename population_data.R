########################################################################
#
# POPULATION_DATA.R
# ======================================================================
#
# Import, clean and prepare data for processing.
#
# Author:     Gerry Henstra
# Date:       December 2019
#
########################################################################



# function used to identify methods from other scripts
population_data <- function(){ }



########################################################################
#
# DATASETS
#
########################################################################

########################################################################
# Main Datasets
########################################################################
locations <- NULL
population <- NULL
future_population <- NULL
world <- NULL
variant <- NULL
fertility <- NULL
mortality <- NULL
population5 <- NULL

########################################################################
# working Datasets
########################################################################
population_by_country <- NULL
top10_countries <- NULL
china_india_trends <- NULL
top10_countries_trends <- NULL
all_countries_trends <- NULL
age_groups <- NULL



########################################################################
#
# FILE LINKS
#
########################################################################

########################################################################
# External Links
########################################################################
link_population <- "https://population.un.org/wpp/Download/Files/1_Indicators%20(Standard)/CSV_FILES/WPP2019_PopulationBySingleAgeSex_1950-2019.csv"
link_future_population <- "https://population.un.org/wpp/Download/Files/1_Indicators%20(Standard)/CSV_FILES/WPP2019_PopulationBySingleAgeSex_2020-2100.csv"

link_variants <- "https://population.un.org/wpp/Download/Files/1_Indicators%20(Standard)/CSV_FILES/WPP2019_Period_Indicators_Medium.csv"
link_locations <- "https://population.un.org/wpp/Download/Files/4_Metadata/WPP2019_F01_LOCATIONS.XLSX"


########################################################################
# Filenames
########################################################################
filename_population <- "WPP2019_PopulationBySingleAgeSex_1950-2019.csv"
filename_future_population <- "WPP2019_PopulationBySingleAgeSex_2020-2100.csv"
filename_locations <- "WPP2019_F01_LOCATIONS.xlsx"
filename_variant <- "WPP2019_Period_Indicators_Medium.csv"




########################################################################
#
# FILE PROCESSING
#
########################################################################

########################################################################
# Import Data 
# ----------------------------------------------------------------------
# Imports all data from appropriate sources if required
#
########################################################################
population_data.import_data <- function(){
  
  if (is.null(world)){import_world_data()}
  if (is.null(locations)){import_locations_data()}
  if (is.null(population)){import_population_data()}
  if (is.null(variant)){import_variant_data()}
  
}



########################################################################
# Import World Data
########################################################################
import_world_data <- function(){
  message("IMPORTING WORLD DATA")  
  
  world <- ne_countries(scale = "medium", returnclass = "sf")
  world <- world %>% mutate(country = name)
  
  world <- world %>%
    mutate(code = as.double(un_a3))
  
  assign("world", world, envir = .GlobalEnv)
  

}



########################################################################
# Import Location Data
########################################################################
import_locations_data <- function() {
  
  message("IMPORTING LOCATION DATA")
  
  # file is an Excel file
  
  if(DOWNLOAD_FROM_SOURCE) {
    download.file(link_locations, str_c(DATA_DIRECTORY, "/", filename_locations), mode="wb")
    xlsx <- read_excel(str_c(DATA_DIRECTORY, "/", filename_locations), sheet=1, col_names=FALSE, skip=17)
  }
  
  # use the first sheet and ignore the first 17 rows
  xlsx <- read_excel(str_c(DATA_DIRECTORY, "/", filename_locations), sheet=1, col_names=FALSE, skip=17)
  
  # there are no clean column names so the following adds usable ones
  colnames(xlsx) <- c("index",
                      "name",
                      "notes",
                      "code",
                      "alpha",
                      "typecode",
                      "typename",
                      "parentcode",
                      "worldcode",
                      "subregioncode",
                      "subregionname",
                      "sdgsubregioncode",
                      "sdgsubregionname",
                      "sdgregioncode",
                      "sdgregionname",
                      "regioncode",
                      "regionname",
                      "UN_moredeveloped_901",
                      "UN_lessdeveloped_902",
                      "UN_leastdeveloped_941",
                      "UN_lessdeveloped_x_least_934",
                      "UN_lessdeveloped_x_china_948",
                      "UN_landlockeddeveloping_1636",
                      "UN_smallislanddeveloping_1637",
                      "WB_highincome_1503",
                      "WB_middleincome_1517",
                      "WB_uppermiddleincome_1502",
                      "WB_lowermiddleincome_1501",
                      "WB_lowincome_1500",
                      "WB_nogroupavail_1518",
                      "HIV_malemax",
                      "HIV_femalemax",
                      "HIV_bothmax",
                      "HIV_yearformaxboth",
                      "mortalityagepattern",
                      "lifeexpectancyatbirth",
                      "populationlessthan90000"
                      )

  # select only the columns required for the analysis
  locations <- xlsx %>%
    select(code,
           name,
           alpha,
           typecode,
           typename,
           parentcode,
           worldcode,
           regioncode,
           regionname,
           subregioncode,
           subregionname
           )

  #assign the data set to the global variable so it is accessible by any script file
  assign("locations", locations, envir = .GlobalEnv)
}




########################################################################
# Import Population Data
########################################################################
import_population_data <- function(){
  
  message("IMPORTING POPULATION DATA")
  
  # these files are rather large, each over 240MB
  # set the DOWNLOAD_FROM_SOURCE variable to TRUE the first time to get the file from the UN site
  # once you have it set it to FALSE so it continues to use the local file
  # the DOWNLOAD_FROM_SOURCE global variable can be found in the population_global.r script
  
  if(DOWNLOAD_FROM_SOURCE) {
    temp <- tempfile()
    download.file(link_population, temp)
    population <- fread(text = gsub("::", ",", readLines(temp)))  
    
    temp <- tempfile()
    download.file(link_future_population, temp)
    future_population <- fread(text = gsub("::", ",", readLines(temp)))  
    
    rm(temp)
    
  } else {
    population <- read_csv(str_c(DATA_DIRECTORY, "/", filename_population))
    future_population <- read_csv(str_c(DATA_DIRECTORY, "/", filename_future_population))
  }
  
  # this file will be the validation set as it contains the UN population prediction from 2020 to 2100
  future_population <<- future_population %>% 
    select(code = LocID,
           name = Location,
           year = Time,
           age = AgeGrp,
           male = PopMale,
           female = PopFemale,
           total = PopTotal)  

  # this file contains the actual population levels by country, year and age from 1950 to 2019
  population <- population %>% 
    filter(Time < 2020) %>%
    select(code = LocID,
           name = Location,
            year = Time,
            age = AgeGrp,
            male = PopMale,
            female = PopFemale,
            total = PopTotal)  

  # population totals are in thousands, so we convert them to full population numbers  
  population <- population %>%
    mutate(total = (total * 1000),
           male = (male * 1000),
           female = (female * 1000))

  # create a new column for the decade of each age for some analysis  
  population <- population %>%
    mutate(
      decade = ifelse(age < 100, floor(age/10) * 10 + 9, 100),
      agegroup = ifelse(age < 100, str_c(as.character(decade - 9), "-", as.character(decade)), "100+")
    )

  # populate a few global variables that are used throught
  FIRST_YEAR <<- min(population$year)
  LAST_YEAR <<- max(population$year)
  YEARS <<- FIRST_YEAR:LAST_YEAR

  # get the most current population values - 2019
  CURRENT_POPULATION <<- population %>%
    left_join(locations, by = "code") %>%
    filter(year == LAST_YEAR & typecode == 4) %>%
    summarize(total = sum(total)) %>%
    .$total
  
  # get the population from the oldest year which is 1950
  PREVIOUS_POPULATION <<- population %>%
    left_join(locations, by = "code") %>%
    filter(year == FIRST_YEAR & typecode == 4) %>%
    summarize(total = sum(total)) %>%
    .$total
  
  # calculate the increase in population from 1950 to 2019
  POPULATION_INCREASE <<- (CURRENT_POPULATION - PREVIOUS_POPULATION) / PREVIOUS_POPULATION * 100
  

  # assign the population data set to the global variable for use throught the project
  assign("population", population, envir = .GlobalEnv)
  
  # create subsets of the data for our observation / analysis
  create_population_by_country()
  create_top10_countries()
  create_china_india_trends()
  create_top10_countries_trends()
  create_age_groups()

  
}




########################################################################
# Import Variant Data
########################################################################
import_variant_data <- function() {
  
  message("IMPORTING VARIANT DATA")
  
  # this file contains the variant data including birth rates, death rates and life expectancy in 5 year intervals
  # set the DOWNLOAD_FROM_SOURCE variable to TRUE the first time to get the file from the UN site
  # once you have it set it to FALSE so it continues to use the local file
  # the DOWNLOAD_FROM_SOURCE global variable can be found in the population_global.r script
  
  if(DOWNLOAD_FROM_SOURCE) {
    temp <- tempfile()
    download.file(link_variants, temp)
    variant <- fread(text = gsub("::", ",", readLines(temp)))  
    
    rm(temp)
  } else {
    variant <- read_csv(str_c(DATA_DIRECTORY, "/", filename_variant))
  }

  # not all fields are used, so select only the columns needed for the analysis and model  
  variant <- variant %>%
    select(code = LocID,
           name = Location,
           period = Time,
           fertility_rate = TFR,
           birth_rate = CBR,
           total_births = Births,
           life_expectancy_both = LEx,
           life_expectancy_male = LExMale,
           life_expectancy_female = LExFemale,
           mortality_infant = IMR,
           mortality_under_5 = Q5,
           death_rate = CDR,
           total_deaths = Deaths,
           deaths_male = DeathsMale,
           deaths_feamle = DeathsFemale,
           migration_rate = CNMR,
           migration_total = NetMigrations)
  

  # Convert a few values and create a year range because these numbers are for 5 year intervals, not single
  # as in the population data set
  
  variant <- variant %>% 
    mutate(year = as.numeric(substr(period, 1, 4)),
           from_year = as.numeric(substr(period, 1, 4)),
           to_year = as.numeric(substr(period, 6, 9)) - 1,
           total_births = total_births * 1000,
           mortality_rate_infant = mortality_infant,
           mortality_rate_under_5 = mortality_under_5,
           death_rate = death_rate,
           total_deaths = total_deaths,
           deaths_male = deaths_male,
           deaths_feamle = deaths_feamle,
           migration_rate = migration_rate,
           migration_total = migration_total,
           life_expectancy = life_expectancy_both)
  
  # extract the fertility rates for 1950 and 2019
  PREVIOUS_FERTILITY_RATE <<- variant %>% filter(code==GLOBAL & year==1955) %>% .$fertility_rate
  CURRENT_FERTILITY_RATE <<- variant %>% filter(code==GLOBAL & year==2020) %>% .$fertility_rate
  

  # assign the data set to the global variable
  assign("variant", variant, envir = .GlobalEnv)
  
}






########################################################################
# Create Working datasets 
# ----------------------------------------------------------------------
# Manipulates main data sets into working sets
#
# These datasets are used in the observation section of the report
########################################################################

########################################################################
# Population By Country
########################################################################
create_population_by_country <- function(){
  
  population_by_country <- population %>%
    group_by(code, name, year) %>%
    summarize(male = sum(male), female = sum(female), total = sum(total))
  
  assign("population_by_country", population_by_country, envir=.GlobalEnv)
  
}


########################################################################
# Top 10 Countries by Population
########################################################################
create_top10_countries <- function(){
  top10_countries <- population_by_country %>% 
    left_join(locations, by="code") %>%    
    filter(year == LAST_YEAR & typecode == 4) %>%
    select(Code = code, Country = name.x, Population = total)
  
  top10_countries <- top10_countries %>%
    arrange(desc(Population)) %>%
    head(10)

  # <<- is another / short form for assigning a variable to a global variable. Does the same as the assign command
  
  TOP10_COUNTRIES_POPULATION <<- top10_countries %>% group_by() %>% summarize(total = sum(Population)) %>% .$total
  TOP10_COUNTRIES_PERCENTAGE <<- TOP10_COUNTRIES_POPULATION / CURRENT_POPULATION * 100

  TOP10 <<- top10_countries %>% select(Code, Country)
  
  
  assign("top10_countries", top10_countries, envir=.GlobalEnv)
}


########################################################################
# Top 10 Countries by Population By Year
########################################################################
create_top10_countries_trends <- function() {
  
  top10_countries_trends <- population_by_country %>%
    left_join(locations, by="code") %>%    
    filter(typecode == 4 & code %in% top10_countries$Code) %>%
    select(Code = code, Country = name.x, Year = year, Population = total)
    
  assign("top10_countries_trends", top10_countries_trends, envir=.GlobalEnv)
  
}



########################################################################
# All Countries trends
########################################################################
create_all_countries_trends <- function() {
  previous <- population_by_country %>%
    filter(year == FIRST_YEAR) %>%
    select(Code = code, Country = name, Previous_Male = male, Previous_Female = female, Previous_Total = total)
  
  current <- population_by_country %>%
    filter(year == LAST_YEAR) %>%
    select(Code = code, Country = name, Current_Male = male, Current_Female = female, Current_Total = total)
  
  all_countries_trends <- current %>%
    left_join(previous, by="Code") %>%
    select(Code = Code, 
           Country = Country.x, 
           Current_Male, 
           Previous_Male, 
           Current_Female,
           Previous_Female,
           Current_Total,
           Previous_Total)
  
  assign("all_countries_trends", all_countries_trends, envir=.GlobalEnv)
}



########################################################################
# China and India yearly trends
########################################################################
create_china_india_trends <- function() {
  china_india_trends <- population_by_country %>%
    filter(code %in% c(156, 356)) %>%
    select(Code = code, Country = name, Year = year, Population = total)
  
  china_previous <- china_india_trends %>%
    filter(Code == 156 & Year == FIRST_YEAR) %>%
    .$Population
  
  china_current <- china_india_trends %>%
    filter(Code == 156 & Year == LAST_YEAR) %>%
    .$Population
  
  CHINA_POPULATION_INCREASE <<- china_current - china_previous
  CHINA_PERCENT_INCREASE <<- (china_current - china_previous) / china_previous * 100
  
  india_previous <- china_india_trends %>%
    filter(Code == 356 & Year == FIRST_YEAR) %>%
    .$Population
  
  india_current <- china_india_trends %>%
    filter(Code == 356 & Year == LAST_YEAR) %>%
    .$Population
  
  INDIA_POPULATION_INCREASE <<- india_current - india_previous
  INDIA_PERCENT_INCREASE <<- (india_current - india_previous) / india_previous * 100
  
  
  assign("china_india_trends", china_india_trends, envir=.GlobalEnv)
}



########################################################################
# Age Groups
########################################################################
create_age_groups <- function() {
  
  age_groups <- population %>%
    group_by(code, name, year, decade, agegroup) %>%
    summarize(male = sum(male), female = sum(female), total = sum(total)) %>%
    select(Code=code, Country=name, Year=year, Decade=decade, AgeGroup=agegroup, Male=male, Female=female, Total=total)
  
  assign("age_groups", age_groups, envir=.GlobalEnv)
}



  
