library(tidyverse)
library(dplyr)
library(ggplot2)
library(plotly)

# The functions might be useful for A4
source("../source/a4-helpers.R")

## Test queries ----
#----------------------------------------------------------------------------#
# Simple queries for basic testing
#----------------------------------------------------------------------------#
# Return a simple string
test_query1 <- function() {
  return ("Hello world")
}

# Return a vector of numbers
test_query2 <- function(num=6) {
  v <- seq(1:num)
  return(v)
}

# Introduction: Prisoner Inequity Project
# The US has 5% population of the world but a 25% prisoner population of the world. 
# Prisoners are not treated equally in US prisons. 
# We use a dataset from Vera Inst that contains a lot of detailed data related to US prison.   
# The direct stakeholders can be officials in US Justice Department, family members of prisoners, lawyers, and state/local governments. 
# The indirect stakeholders can be any US citizen or law enforcement people in other countires. 
# The questions that we answer in this report:
# - Growth of prisoner population in the country and by states.
# - Prisoner population by gender, race, and geographic area.
# - Is there a bias (unfairness) to minorities, specially Black/Africa American people.


## Section 2  ---- 
#----------------------------------------------------------------------------#
# Your functions and variables might go here ... <todo: update comment>
#----------------------------------------------------------------------------#

# We will focus on the Total Population, Total Jail Population, and Black Jail Population.
# These values will likely be calculated using your DPLYR skills. You might answer such questions as: 
  
# Complete: At least three values are included 
# Complete: Values clarify chosen variables related to patterns of inequality 

df <- read.csv("https://raw.githubusercontent.com/vera-institute/incarceration-trends/master/incarceration_trends.csv")
# View(df)

# filter data in most recent year
most_recent_year <- max(df$year)
print(paste("The most recent year is: ", most_recent_year))
df_recent <- filter(df, df$year == most_recent_year)
# View(df_recent)

# Calculate total population in US
total_population <- sum(df_recent$total_pop, na.rm = TRUE)
print(paste("Total Population in US is ", total_population))

# Calculate total jail population in US
total_jail_population <- sum(df_recent$total_jail_pop, na.rm = TRUE)
print(paste("Total prisoner in US jail is ", total_jail_population))

# Calculate total black jail population in US
total_black_jail_population <- sum(df_recent$black_jail_pop, na.rm = TRUE)
print(paste("Total black prisoner in US jail is ", total_black_jail_population))

# What is the average value of Jail Pop across all the counties (in most recent year)? 
average_jail_pop <- mean(df_recent$total_jail_pop, na.rm = TRUE)
print(paste("Average prison population in counties: ", average_jail_pop))

# Where is Jail Pop the highest or lowest?  
highest_number <- max(df_recent$total_jail_pop, na.rm = TRUE)
print(highest_number)
highest_county <- filter(df_recent, total_jail_pop == highest_number)
print(paste("The county with highest prison pop: ", highest_county$county_name))

lowest_number <- min(df_recent$total_jail_pop, na.rm = TRUE)
print(lowest_number)
lowest_county <- filter(df_recent, total_jail_pop == lowest_number)
print(paste("The counties with lowest prison pop: ", lowest_county$county_name))

## Section 3  ---- 
#----------------------------------------------------------------------------#
# Growth of the U.S. Prison Population
# Your functions might go here ... <todo:  update comment>
#----------------------------------------------------------------------------#
# This function ... <todo:  update comment>
get_year_jail_pop <- function() {
  # TODO: Implement this function 
  year_jail_pop <- df %>% group_by(year) %>% 
    summarise(US_jail_pop = sum(total_jail_pop, na.rm =  TRUE))
  return(year_jail_pop)   
}

# This function ... <todo:  update comment>
plot_jail_pop_for_us <- function(dataset)  {
  # TODO: Implement this function 
    # require(ggplot2)
    g <- ggplot(dataset, aes(x = year, y = US_jail_pop)) + geom_smooth(method = "lm") + geom_point()
    return (g)
}

dataset <- get_year_jail_pop()
p <- plot_jail_pop_for_us(dataset)
plot(p)

## Section 4  ----  
#----------------------------------------------------------------------------#
# Growth of Prison Population by State 
# Your functions might go here ... <todo:  update comment>
# See Canvas
#----------------------------------------------------------------------------#
# get state names
# print(unique(df_recent$state))

# the State is WA, it can be replaced by any state
get_year_jail_pop <- function() {
  # TODO: Implement this function 
  year_jail_pop <- filter(df, state == "WA") %>% group_by(year) %>% 
    summarise(US_jail_pop = sum(total_jail_pop, na.rm =  TRUE))
  return(year_jail_pop)   
}

# This function ... <todo:  update comment>
plot_jail_pop_for_us <- function(dataset)  {
  # TODO: Implement this function 
  # require(ggplot2)
  g <- ggplot(dataset, aes(x = year, y = US_jail_pop)) + geom_smooth(method = "lm") + geom_point()
  return (g)
}

dataset <- get_year_jail_pop()
p <- plot_jail_pop_for_us(dataset)
plot(p)

## Section 5  ---- 
#----------------------------------------------------------------------------#
# <variable comparison that reveals potential patterns of inequality>
# Your functions might go here ... <todo:  update comment>
# See Canvas
#----------------------------------------------------------------------------#
# compare the percentage of black in jail
black_ratio <- 100 * total_black_jail_population / total_jail_population
print(paste("The US black prison ratio % is: ", black_ratio))
# The black ratio in US population is 13.6% from the US Census. 
# The black jail ratio is 33.58% so a pattern of inequality is true.

## Section 6  ---- 
#----------------------------------------------------------------------------#
# <a map shows potential patterns of inequality that vary geographically>
# Your functions might go here ... <todo:  update comment>
# See Canvas
#----------------------------------------------------------------------------#
# install.packages("usmap")
library("usmap")

# black jail pop by county
p <- plot_usmap(data = df_recent, values = "black_jail_pop", color = "black") + 
  scale_fill_continuous(low = "white", high = "red", name = "County Black Jail Population (2018)", label = scales::comma) + 
  theme(legend.position = "right")
plot(p)

# black jail pop by state
state_black_jail_pop <- df_recent %>% group_by(state) %>% 
  summarise(black_pop = sum(black_jail_pop, na.rm =  TRUE))
p <- plot_usmap(data = state_black_jail_pop, values = "black_pop", color = "black") + 
  scale_fill_continuous(low = "white", high = "red",name = "State Black Jail Population (2018)", label = scales::comma) + 
  theme(legend.position = "right")
plot(p)

## Load data frame ---- 
