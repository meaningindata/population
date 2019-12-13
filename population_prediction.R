########################################################################
#
# POPULATION_PREDICTION.R
# ======================================================================
#
# Script used to develop the machine learning model to predict population
#
# Author:     Gerry Henstra
# Date:       December 2019
#
########################################################################



# function used to identify methods from other scripts
population_prediction <- function(){ }



########################################################################
# Variables
########################################################################
years_to_predict <- 31
mean_variant_diff <- 5
smoothing_factor <- .25



########################################################################
# Datasets
########################################################################

# converted variants data from 5 year intervals to yearly
yearly_variants <- data.frame(code = as.numeric(),
                              year = as.numeric(),
                              birth_rate = as.numeric(),
                              death_rate = as.numeric(),
                              migration_rate = as.numeric(),
                              life_expectancy = as.numeric()
                              )

# predicted yearly variants from 2020 to 2050
predicted_variants <- data.frame(code = as.numeric(),
                                 year = as.numeric(),
                                 birth_rate = as.numeric(),
                                 death_rate = as.numeric(),
                                 migration_rate = as.numeric(),
                                 life_expectancy = as.numeric()
                                 )

training <- NULL
test <- NULL

prediction <- NULL
prediction_accuracy <- NULL


########################################################################
# Convert 5 year variants to yearly
########################################################################
population_prediction.convert_variants_to_yearly <- function() {
  
  # variants are in 5 year intervals
  # convert them to yearly so they match the level of the population set
  
  variants <- variant %>% 
    filter(year < 2020 & code==GLOBAL)

  # for each row in the working set, execute a routine to split it into years
  for(i in 1:nrow(variants)){
    v = variants[i,]
    split_5year_variants(v)
  }

  yearly_variants <<- yearly_variants %>% mutate_all(funs(ifelse(is.na(.),0,.)))
}


split_5year_variants <- function(v){
  
  # parameter v = a 5 year variant record
  
  # columns were added to the data set that represent the from year and to year of the 5 year period
  # create a list of the years in the period
  from_to = v$from_year:v$to_year
  
  # grab the following year to identify what value we need to get to
  next_year <- variant %>% filter(code==v$code & from_year==v$from_year + 5)
  
  # calculate the difference between the passed year and the next year
  # divide this by 5 to get a yearly difference
  birth_rate_diff <- (next_year$birth_rate - v$birth_rate) / 5
  death_rate_diff <- (next_year$death_rate - v$death_rate) / 5
  migration_diff <- (next_year$migration_rate - v$migration_rate) / 5
  life_expectancy_diff <- (next_year$life_expectancy_both - v$life_expectancy_both) / 5
  
  year_index <- 0
  
  # for each year in the 5 year period add a yearly record with the singel year values
  for (x in from_to) {
    # start with the value from the variant being split
    # Take the yearly difference calculated and multiply it by the year index to determine the value for each year
    # add this to the birth rate of the passed record to begin increasing the value for each year
    
    yearly_variants <<- rbind(yearly_variants,
                                 data.frame(code = v$code,
                                            year = x,
                                            birth_rate = v$birth_rate + (birth_rate_diff * year_index),
                                            death_rate = v$death_rate + (death_rate_diff * year_index),
                                            migration_rate = v$migration_rate + (migration_diff * year_index),
                                            life_expectancy = v$life_expectancy_both + (life_expectancy_diff * year_index)
                                            )
                                 )
    
    # increment the year index
    year_index <- year_index + 1
  }
  
}



########################################################################
# Variant Predictions
########################################################################

population_prediction.predict_variants <- function() {

  # once the variants have been extrapolated to yearly, calculate the future values


  # start with the last year in the set
  lastest_year <- max(yearly_variants$year)
    
  # create a vector of the years to predict based on the last year we have and the number of years to predict
  y <- seq(lastest_year + 1,lastest_year + years_to_predict)
  
  # apply the variant prediction logic for each year
  sapply(y, calculate_variants_for_year)
  
}


calculate_variants_for_year <- function(y) {
  
  # parameters y = year to predict

  # codes represent the levels of the data (country, global etc...) 
  # depending on the codes being processed, get a list of each code
  #
  # I ended up with using just the GLOBAL records, but did at one time use country specific records
  # This code allowed me to switch and play with the various levels
  
  codes <- yearly_variants %>% group_by(code) %>% summarize()
  
  for(c in codes$code) {
    # One of the smoothing values is to use the average diffence over a number of previous years
    # prior_variants is the data set of those years
    # the number of years to go back is set in the variable mean_variant_diff
    
    prior_variants <- yearly_variants %>% filter(year>=(y - mean_variant_diff) & code==c)

    # get the values of the prior year to the one passed in
    last_variant <- yearly_variants %>% filter(year==(y-1) & code==c)
    
    # calculate the rate of change (ROC) based on the average difference of values over the prior X years
    # take the value from the last year and add the rat of change to it
    # apply an extra smoothing factor to the ROC to help reduce the decline of birth rates and death rates
    # the additional smoothing rate will not be applied to the life expectancy as it leveled the value out too much
    
    roc_birth_rate <- mean(diff(prior_variants$birth_rate))
    new_birth_rate <- last_variant$birth_rate + (roc_birth_rate * (1 - smoothing_factor))
    
    roc_death_rate <- mean(diff(prior_variants$death_rate))
    new_death_rate <- last_variant$death_rate + (roc_death_rate * (1 - smoothing_factor))
  
    roc_migration_rate <- mean(diff(prior_variants$migration_rate))
    roc_life_expectancy <- mean(diff(prior_variants$life_expectancy))
    new_life_expectancy <- last_variant$life_expectancy + roc_life_expectancy

    # create a new record of variant values for the year and add it to the yearly variants data set
    yearly_variants <<- rbind(yearly_variants,
                              data.frame(code = c,
                                         year = y,
                                         birth_rate = new_birth_rate,
                                         death_rate = new_death_rate,
                                         migration_rate = last_variant$migration_rate + roc_migration_rate,
                                         life_expectancy = new_life_expectancy
                              ))

      }
  
}


########################################################################
# Population Predictions
########################################################################

population_prediction.predict_population <- function() {
  
  # create a training set to be used for the model and population predictions
  training <<- population %>%
    left_join(locations, by="code") %>%    
    filter(year < 2020 & code==GLOBAL) %>%
    select(code, 
           year,
           name=name.x,
           age,
           total
           )
  
  # create the validation set 
  validation <<- future_population %>%
    filter(code==GLOBAL) %>%
    group_by(year, code, name) %>%
    summarize(total = sum(total * 1000))
  
  
  # get the last year in the training set so we know here to start from
  lastest_year <- max(training$year)
  
  # generate a vector of years to predict for
  years <- seq(lastest_year + 1,lastest_year + years_to_predict)
  
  # predict th epopulation for each year in the vector
  sapply(years, predict_population_for_year)


}


predict_population_for_year <- function(y) {
  
  # paramters y = year to predict

  # get the codes we are working with
  # the final version is only using GLOBAL, but we did play with each contry at one point
  # we kept that logic
  
  codes <- training %>% group_by(code) %>% summarize()
  
  # for each country code (only global in final), predict the population
  for(c in codes$code) {

    # get the population form two year ago
    prior_year <- training %>% filter(code--c & year==(y - 2))

    # get the population for last year
    last_year <- training %>% filter(code==c & year==(y - 1))

    # extract the population from last year
    total_population <- sum(last_year$total)

    # get the name of the country / level    
    name <- last_year[1,]$name

    # get the variants for that year and level from the created yearly variants data set
    variants <- yearly_variants %>% filter(year==(y - 1) & code==c)

    # if the variants are NA, convert them to 0 so they have no impact    
    birth_rate <- ifelse(is.na(variants$birth_rate),0,variants$birth_rate)
    death_rate <- ifelse(is.na(variants$death_rate),0,variants$death_rate)
    migration_rate <- ifelse(is.na(variants$migration_rate),0,variants$migration_rate)
    life_expectancy <- ifelse(is.na(variants$life_expectancy),0,variants$life_expectancy)
    

    # create a nes set of records for the prediction
    # change the year to the year being predicted
    # add one to the age value to simulate the aging of the population by one year
    prediction <- last_year %>%
      mutate(year = y, age = age + 1)

    # add a new record to the prediction data set to include the new births
    # age 0 represents new births and we moved them to age 1
    # we need to calculate what the new births are for the new year and add the age 0 record for it
    prediction <- rbind(prediction,
                      data.frame(
                        code = c,
                        year = y,
                        name = name, 
                        age = 0,
                        total = (total_population / 1000 * birth_rate)
                      ))
    
    # for each age group (including births) we need to apply a death rate to them
    prediction <- prediction %>%
      mutate(total = total - (total/1000*death_rate))
    
    # take the life expectancy and determine the percent of the population that over it
    old_age_percent <- prediction %>% filter(age > life_expectancy) %>% summarize(total = sum(total)) / prediction %>% summarize(total = sum(total)) %>% .$total

    # take the population over the life expectancy from the previous two years    
    prior_old_age <- prior_year %>% filter(age > life_expectancy) %>% summarize(total = sum(total)) %>% .$total
    last_old_age <- last_year %>% filter(age > life_expectancy) %>% summarize(total = sum(total)) %>% .$total
    
    # adjust the new old_age_percent by the variance of the prior two years
    # this results in an adjustment to be made to the population over the life expectancy
    le_adjustment <- old_age_percent$total * (last_old_age - prior_old_age) / prior_old_age

    # for the age groups over the life expectancy, adjust the population with the adjustment calculated
    prediction <- prediction %>%
      mutate(total = ifelse(age < life_expectancy, total, (total - (total * le_adjustment))))

    # add the new yearly prediction to the training set so it can be used in the next year calculation    
    training <<- rbind(training, prediction)
    
  }

}


########################################################################
# Population Prediction Accuracy Calculation
########################################################################
population_prediction.calculate_prediction_accuracy <- function() {

  # retrieve the predicted years only
  prediction <-training %>%
    filter(year>2019) %>%
    group_by(year, code, name) %>%
    summarize(total = sum(total))
  

  # create a data set by joining the predicted values to the validation set
  # the validation set contains the predictions of population from the UN
  prediction_accuracy <<- prediction %>%
    left_join(validation, by=c("year", "code", "name")) %>%
    select(Year=year, Code=code, Country=name,  Prediction=total.x, Actual=total.y) %>%
    mutate(Variance = (Prediction - Actual) / Actual * 100) %>%
    arrange(Variance)
}





