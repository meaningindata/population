########################################################################
#
# POPULATION_PLOTS.R
# ======================================================================
#
# Routines for creating plots and maps
#
# Author:     Gerry Henstra
# Date:       December 2019
#
########################################################################



# function used to identify methods from other scripts
population_plots <- function(){ UseMethod("population_plots")}



########################################################################
# Show world population on map for year
########################################################################
world_population_map <- NULL

population_plots.map_world_population <- function(y){
  
  # passed parameter y = year
  
  
  if (is.null(world_population_map)) {
    data <- population_by_country %>% 
      left_join(locations, by="code") %>%
      filter(year == y & typecode==4)
    
    world_population_map <<- world %>% 
      left_join(data, by="code") %>%
      ggplot() + 
      geom_sf(aes(fill = total/10^6)) +
      ggtitle(str_c("Population by Country for ", y)) +
      scale_fill_gradient("Millions", na.value="grey", guide="colourbar", aesthetics = "fill")
    
  }
  world_population_map
  
}


########################################################################
# Report the top 10 countries
########################################################################
population_plots.list_top10_countries <- function(){

  # <<- assigns value to the global variable, same as assign command
  TOP10_COUNTRIES_POPULATION <<- top10_countries %>% group_by() %>% summarize(total = sum(Population)) %>% .$total
  TOP10_COUNTRIES_PERCENTAGE <<- TOP10_COUNTRIES_POPULATION / CURRENT_POPULATION * 100
  
  data <- top10_countries %>% mutate(Population = format(Population, big.mark=","))
  
  knitr::kable(data)
  
}



########################################################################
# Plot China and India population trends
########################################################################
population_plots.plot_china_india_trends <- function() {

  china_india_trends %>%
    ggplot(aes(Year, Population / 10^6, colour=Country)) +
    geom_line() +
    theme(axis.text.x = element_text(angle=90, hjust=1)) +
    scale_x_continuous(name="Year", breaks=YEARS) +
    ggtitle("China/India Population by Year") +
    ylab("Millions of People")
}




########################################################################
# Plot China Population Growth
########################################################################
population_plots.plot_china_population_growth <- function() {
  
  yearintervals <- seq(1950,2020,5)
  
  china <- population_by_country %>%
    filter(code == 156 & year > 1950) %>%
    mutate(diff = total - lag(total), growth = diff / lag(total) * 100)
  
  china %>% 
    ggplot(aes(year, growth)) +
    geom_line() +
    theme(axis.text.x = element_text(angle=90, hjust=1)) +
    scale_x_continuous(name="Year", breaks=yearintervals) +
    ggtitle("China Population Growth by Year") +
    ylab("% Growth") +
    ylim(0,3)

}





########################################################################
# Plot Top 10 country population trends
########################################################################
population_plots.plot_top10_countries_trends <- function() {
  
  yearintervals <- seq(1950,2020,5)
  
  top10_countries_trends %>%
    ggplot(aes(Year, Population / 10^6, colour=Country)) +
    geom_line() +
    theme(axis.text.x = element_text(angle=90, hjust=1)) +
    scale_x_continuous(name="Year", breaks=yearintervals) +
    ggtitle("Top 10 Countries Population by Year") +
    ylab("Millions of People") + 
    theme(legend.position = "bottom")
}






########################################################################
# Plot gender by year
########################################################################
population_plots.plot_gender_trends <- function(c) {

  # passed parameter c = country code
  
  gender_trends_male <- population_by_country %>%
    filter(code == c) %>%
    group_by(year) %>%
    summarize(total = sum(male)) %>%
    mutate(gender = "Male") %>%
    select(year,
           gender,
           total)
  
  gender_trends_female <- population_by_country %>%
    filter(code == c) %>%
    group_by(year) %>%
    summarize(total = sum(female)) %>%
    mutate(gender = "Female") %>%
    select(year,
           gender,
           total)
  
  gender_trends <- rbind(gender_trends_male, gender_trends_female)
  
  gender_trends %>%
    ggplot(aes(year, total/10^6, group=gender, fill=gender)) +
    geom_bar(stat="identity", position="stack") +
    theme(axis.text.x = element_text(angle=90, hjust=1)) +
    scale_x_continuous(name="Year", breaks=YEARS) +
    ggtitle("Gender Population by Year") +
    ylab("Millions of People")
  
}




########################################################################
# Plot fertility rates
########################################################################
population_plots.plot_fertility_rates <- function(c){
  
  # passed parameter c = country code
  
  yearintervals <- seq(1950,2020,5)
  
  variant %>%
    filter(code==c & year <= 2020) %>%
    ggplot(aes(year, fertility_rate)) +
    geom_line() +
    theme(axis.text.x = element_text(angle=90, hjust=1)) +
    scale_x_continuous(name="Year", breaks=yearintervals) +
    ggtitle("Fertility Rate by Year") +
    ylab("Average Births per Woman") +
    geom_hline(yintercept = 2.1, linetype="dashed", color="black") +
    geom_text(aes(1960,1.9, label="Replacement Fertility Rate", vjust=-1), color="black")
  
}




########################################################################
# Plot fertility rates of top 10 countries
########################################################################
population_plots.plot_fertility_rates_top10 <- function() {
  
  yearintervals <- seq(1950,2020,5)
  
  variant %>%
    filter(code %in% TOP10$Code & year <= 2020) %>%
    ggplot(aes(year, fertility_rate, colour=name)) +
    geom_line() +
    scale_x_continuous(name="Year", breaks=yearintervals) +
    ggtitle("Fertility Rate for Top 10 Countries by Year") +
    ylab("Average Births per Woman") +
    geom_hline(yintercept = 2.1, linetype="dashed", color="black") +
    geom_text(aes(1960,1.9, label="Replacement Fertility Rate", vjust=-1), color="black") +
    theme(legend.position = "bottom", axis.text.x = element_text(angle=90, hjust=1))

}




########################################################################
# Plot Age groups for year
########################################################################
population_plots.plot_age_groups_for_year <- function(c, y) {
  
  #passed parameters : c = country code, y = year
  
  age_groups %>%
    filter(Code == c, Year == y) %>%
    arrange(Decade) %>%
    ggplot(aes(reorder(AgeGroup, Decade),Total/10^6)) +
    geom_bar(stat="identity") +
    theme(axis.text.x = element_text(angle=90, hjust=1)) +
    xlab("Age Groups") +
    ggtitle(str_c("Global Age Population in ", y)) +
    ylab("Millions of People")    

}





########################################################################
# Plot Percent of Age Groups between 1950 and 2019
########################################################################
population_plots.plot_compare_agegroups <- function(){
  first <- age_groups %>%
    filter(Code == GLOBAL, Year == FIRST_YEAR)
  
  first_average_age <- population %>%
    filter(code==900 & year==FIRST_YEAR) %>%
    summarize(avg = sum(age * total) / sum(total)) %>%
    .$avg
    
  last <- age_groups %>%
    filter(Code == GLOBAL, Year == LAST_YEAR)
  
  last_average_age <- population %>%
    filter(code==900 & year==LAST_YEAR) %>%
    summarize(avg = sum(age * total) / sum(total)) %>%
    .$avg
  
  ds <- rbind(first, last)
  
  ds %>%
    ggplot(aes(fill=reorder(AgeGroup, Decade), x=Year, y=Total)) +
    geom_bar(position="fill", stat="identity") + 
    scale_x_continuous(breaks=c(FIRST_YEAR, LAST_YEAR)) +
    scale_y_continuous(labels = scales::percent) +
    geom_text(aes(1950,.5, label=str_c("Avg: ", as.character(round(first_average_age))), vjust=-1), color="black") +
    geom_text(aes(2019,.5, label=str_c("Avg: ", as.character(round(last_average_age))), vjust=-1), color="black") +
    labs(title="Age Group Comparisons between 1950 and 2019", x="Year", y="Percent of Population", fill="Age Group")

    
}



########################################################################
# Plot Birth Rates
########################################################################
population_plots.plot_birth_rates <- function(c){

  # passed parameter c = country code
  
  yearintervals <- seq(1950,2020,5)
  
  variant %>%
    filter(code==c & year <= 2020) %>%
    ggplot(aes(year, birth_rate)) +
    geom_line() +
    theme(axis.text.x = element_text(angle=90, hjust=1)) +
    scale_x_continuous(name="Year", breaks=yearintervals) +
    ggtitle("Birth Rate by Year") +
    ylab("Average Births per 1,000")
  
}



########################################################################
# Plot Death Rates
########################################################################
population_plots.plot_death_rates <- function(c){
  
  # passed parameter c = country code
  
  
  yearintervals <- seq(1950,2020,5)
  
  variant %>%
    filter(code==c & year <= 2020) %>%
    ggplot(aes(year, death_rate)) +
    geom_line() +
    theme(axis.text.x = element_text(angle=90, hjust=1)) +
    scale_x_continuous(name="Year", breaks=yearintervals) +
    ggtitle("Death Rate by Year") +
    ylab("Average Deaths per 1,000")

}



########################################################################
# Plot Life Expectancy
########################################################################
population_plots.plot_life_expectancy <- function(c){
  
  # passed parameter c = country code
  
  yearintervals <- seq(1950,2020,5)
  
  variant %>%
    filter(code==c & year <= 2020) %>%
    ggplot(aes(year, life_expectancy)) +
    geom_line() +
    theme(axis.text.x = element_text(angle=90, hjust=1)) +
    scale_x_continuous(name="Year", breaks=yearintervals) +
    ggtitle("Life Expectancy by Year") +
    ylab("Life Expectancy")
  
}



########################################################################
# Plot Life Expectancy for Top 10 countries
########################################################################
population_plots.plot_life_expectancy_top10 <- function(){
  
  yearintervals <- seq(1950,2020,5)
  
  
  variant %>%
    filter(code %in% TOP10$Code & year <= 2020) %>%
    ggplot(aes(year, life_expectancy, colour=name)) +
    geom_line() +
    theme(axis.text.x = element_text(angle=90, hjust=1)) +
    scale_x_continuous(name="Year", breaks=yearintervals) +
    ggtitle("Life Expectancy for Top 10 Populated Countries") +
    ylab("Life Expectancy")


}





########################################################################
# Plot Life Expectancy Smoothing
########################################################################
population_plots.plot_life_expectancy_smoothing <- function() {
  
  df <- yearly_variants %>%
    filter(code==GLOBAL & year<2020) %>%
    mutate(from_year = floor_n(year, 5))
  
  df <- df %>%
    left_join(variant, by=c("code", "from_year")) %>%
    select(year=year.x, predicted=life_expectancy.x, wpp=life_expectancy_both)
  
  ggplot(df, aes(year)) +
    geom_line(aes(y=wpp), colour="black") +
    geom_line(aes(y=predicted), colour="red") +
    ylab("Life Expectancy (years)") +
    ggtitle("Life Expectancy Smoothing Results")
  
}




########################################################################
# Plot Variant Predictions
########################################################################
prediction_plots.plot_birth_rate_prediction <- function() {
  yearly_variants %>%
    ggplot(aes(year, birth_rate)) +
    geom_line() +
    ggtitle("Birth Rate Prediction : 2050") +
    xlab("Year") +
    ylab("Birth's per 1,000")
}

prediction_plots.plot_life_expectancy_prediction <- function() {
  yearly_variants %>%
    ggplot(aes(year, life_expectancy)) +
    geom_line() +
    ggtitle("Life Expectancy Prediction : 2050") +
    xlab("Year") +
    ylab("Life Expectancy (Years)")
}

prediction_plots.plot_death_rate_prediction <- function() {
  yearly_variants %>%
    ggplot(aes(year, death_rate)) +
    geom_line() +
    ggtitle("Death Rate Prediction : 2050") +
    xlab("Year") +
    ylab("Deaths per 1,000")
}






########################################################################
# Plot Population Predictions
########################################################################
prediction_plots.plot_prediction <- function() {
  
  prediction_accuracy %>%
    group_by(Year) %>%
    summarize(Prediction = sum(Prediction), Actual = sum(Actual)) %>%
    ggplot(aes(Year, Actual)) +
    geom_line() + 
    geom_line(aes(Year, Prediction), color="Red")
  
}

prediction_plots.display_prediction_accuracy <- function() {
  pa <- prediction_accuracy %>% group_by(Year) %>% select(Year, Prediction, Actual, Variance) %>% arrange(Year)
  
  pa <- pa %>% 
    mutate(Prediction = format(Prediction, big.mark=","), `UN Prediction` = format(Actual, big.mark=","), `% Variance` = round(Variance,2)) %>%
    select(Year, Prediction, `UN Prediction`, '% Variance')
  
  knitr::kable(pa)
}


