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
yearly_variants <- data.frame(code = as.numeric(),
                              year = as.numeric(),
                              birth_rate = as.numeric(),
                              death_rate = as.numeric(),
                              migration_rate = as.numeric(),
                              life_expectancy = as.numeric()
                              )

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
  
  variants <- variant %>% 
    left_join(locations, by="code") %>%    
    filter(year < 2020 & code==GLOBAL)

  for(i in 1:nrow(variants)){
    v = variants[i,]
    split_5year_variants(v)
  }

  yearly_variants <- yearly_variants %>% mutate_all(funs(ifelse(is.na(.),0,.)))
}

split_5year_variants <- function(v){
  
  from_to = v$from_year:v$to_year
  
  next_year <- variant %>% filter(code==v$code & from_year==v$from_year + 5)
  
  birth_rate_diff <- (next_year$birth_rate - v$birth_rate) / 5
  death_rate_diff <- (next_year$death_rate - v$death_rate) / 5
  migration_diff <- (next_year$migration_rate - v$migration_rate) / 5
  life_expectancy_diff <- (next_year$life_expectancy_both - v$life_expectancy_both) / 5
  
  year_index <- 0
  
  for (x in from_to) {
    yearly_variants <<- rbind(yearly_variants,
                                 data.frame(code = v$code,
                                            year = x,
                                            birth_rate = v$birth_rate + (birth_rate_diff * year_index),
                                            death_rate = v$death_rate + (death_rate_diff * year_index),
                                            migration_rate = v$migration_rate + (migration_diff * year_index),
                                            life_expectancy = v$life_expectancy_both + (life_expectancy_diff * year_index)
                                            )
                                 )
    
    year_index <- year_index + 1
  }
  
}


population_prediction.predict_variants <- function() {

  #years_to_predict <- 10
  
  lastest_year <- max(yearly_variants$year)
    
  y <- seq(lastest_year + 1,lastest_year + years_to_predict)
  
  sapply(y, calculate_variants_for_year)
  
}


calculate_variants_for_year <- function(y) {

  codes <- yearly_variants %>% group_by(code) %>% summarize()
  
  for(c in codes$code) {
    prior_variants <- yearly_variants %>% filter(year>=(y - mean_variant_diff) & code==c)
    last_variant <- yearly_variants %>% filter(year==(y-1) & code==c)
    
    roc_birth_rate <- mean(diff(prior_variants$birth_rate))
    new_birth_rate <- last_variant$birth_rate + (roc_birth_rate * (1 - smoothing_factor))
    
    roc_death_rate <- mean(diff(prior_variants$death_rate))
    new_death_rate <- last_variant$death_rate + (roc_death_rate * (1 - smoothing_factor))
    
    roc_migration_rate <- mean(diff(prior_variants$migration_rate))
    roc_life_expectancy <- mean(diff(prior_variants$life_expectancy))
    new_life_expectancy <- last_variant$life_expectancy + roc_life_expectancy

    yearly_variants <<- rbind(yearly_variants,
                              data.frame(code = c,
                                         year = y,
                                         birth_rate = new_birth_rate,
                                         death_rate = new_death_rate, #last_variant$death_rate, # + roc_death_rate,
                                         migration_rate = last_variant$migration_rate + roc_migration_rate,
                                         life_expectancy = new_life_expectancy
                              ))

      }
  
  
  
}






population_prediction.predict_population <- function() {
  
  training <<- population %>%
    left_join(locations, by="code") %>%    
    filter(year < 2020 & code==GLOBAL) %>%
    select(code, 
           year,
           name=name.x,
           age,
           total
           )
  
  test <<- population %>%
    left_join(locations, by="code") %>%    
    filter(year >= 2020 & code==GLOBAL) %>%
    select(code, 
           year,
           name=name.x,
           age,
           total
           )
  
  lastest_year <- max(training$year)
  
  years <- seq(lastest_year + 1,lastest_year + years_to_predict)
  
  sapply(years, predict_population_for_year)


}


predict_population_for_year <- function(y) {
  
  codes <- training %>% group_by(code) %>% summarize()
  
  for(c in codes$code) {

    prior_year <- training %>% filter(code--c & year==(y - 2))
    last_year <- training %>% filter(code==c & year==(y - 1))
    total_population <- sum(last_year$total)
    
    name <- last_year[1,]$name

    variants <- yearly_variants %>% filter(year==(y - 1) & code==c)
    
    birth_rate <- ifelse(is.na(variants$birth_rate),0,variants$birth_rate)
    death_rate <- ifelse(is.na(variants$death_rate),0,variants$death_rate)
    migration_rate <- ifelse(is.na(variants$migration_rate),0,variants$migration_rate)
    life_expectancy <- ifelse(is.na(variants$life_expectancy),0,variants$life_expectancy)
    
    
    prediction <- last_year %>%
      mutate(year = y, age = age + 1)
    
    prediction <- rbind(prediction,
                      data.frame(
                        code = c,
                        year = y,
                        name = name, 
                        age = 0,
                        total = (total_population / 1000 * birth_rate)
                      ))
    
    prediction <- prediction %>%
      mutate(total = total - (total/1000*death_rate))
    
    prediction <- prediction %>%
      mutate(total = total + (total/1000*migration_rate))  
    
    old_age_percent <- prediction %>% filter(age > life_expectancy) %>% summarize(total = sum(total)) / prediction %>% summarize(total = sum(total)) %>% .$total
    
    prior_old_age <- prior_year %>% filter(age > life_expectancy) %>% summarize(total = sum(total)) %>% .$total
    last_old_age <- last_year %>% filter(age > life_expectancy) %>% summarize(total = sum(total)) %>% .$total
    
    
    le_adjustment <- old_age_percent$total * (last_old_age - prior_old_age) / prior_old_age

    prediction <- prediction %>%
      mutate(total = ifelse(age < life_expectancy, total, (total - (total * le_adjustment))))
    
    training <<- rbind(training, prediction)
    
  }

}


population_prediction.calculate_prediction_accuracy <- function() {
  validation <- test %>%
    group_by(year, code, name) %>%
    summarize(total = sum(total))
  
  future <- future_population %>%
    filter(code==GLOBAL) %>%
    group_by(year, code, name) %>%
    summarize(total = sum(total * 1000))
  
  validation <- rbind(validation, future)
  
  
  prediction <-training %>%
    filter(year>2019) %>%
    group_by(year, code, name) %>%
    summarize(total = sum(total))
  
  
  
  prediction_accuracy <<- prediction %>%
    left_join(validation, by=c("year", "code", "name")) %>%
    select(Year=year, Code=code, Country=name,  Prediction=total.x, Actual=total.y) %>%
    mutate(Variance = (Prediction - Actual) / Actual * 100) %>%
    arrange(Variance)
}



population_prediction.predict <- function(){
  population_prediction.convert_variants_to_yearly()
  population_prediction.predict_variants()
  population_prediction.predict_population()
}


