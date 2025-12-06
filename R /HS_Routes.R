#' @description determines the bus routes of most interest, as they appear to be used as transportation to and from school by the most students.
#' Also, determines the on time performance as a proportion of busses late by more than 5 minutes during school hours and outside of school hours. 
#' @param ridership data frame with the route, type of rider (High.School), Time 
#' @param school_hours a vector of hours that students would be riding the bus based on school start time and release time
#' @return school_routes, a vector of the most likely school routes

#Load in data
## otp <- read.csv("/Users/lelamiller/Downloads/otp_simulated.csv" )
## ridership <- read.csv("/Users/lelamiller/Downloads/ridership_simulated.csv")

# Nikhil Data
otp <- read.csv("/Users/nikhilsonthalia/Downloads/PHP-1560/Data/otp_simulated.csv" )
ridership <- read.csv("/Users/nikhilsonthalia/Downloads/PHP-1560/Data/ridership_simulated.csv")

#load libraries
library(tidyverse)

#vector for school hours? Can be changed when running function for different start times
#We know high schools start at 7:45 roughly, and they end around 2:30 typically
#therefore HS student ridership within the hours 6-7 and 2-3 can be assumed to be transportation to and from school
school_hours <- c(6, 7, 14, 15)

HS_Routes <- function(ridership, otp, school_hours){
  only_hs <- ridership %>%
    filter(High.School == "Providence Public School Department") %>%
    filter(Day.of.Week != "Sat", 
           Day.of.Week != "Sun")
  
  #look at the distribution for routes in which kids are riding regularly, find the average number of students riding each route within each hour of the day
  hs_routes <- only_hs %>%
    mutate(
      Time = as.POSIXct(Time, format = "%m/%d/%y %H:%M"),
      hour = as.numeric(format(Time, "%H")),
      date = as.Date(Time)
    ) %>%
    group_by(Route, hour) %>%
    summarize(x_hat = n()/n_distinct(date))
  
  hs_routes$Route <- as.factor(hs_routes$Route)
  
  #comparing school hour riders to non school hours riders
  school_vs_nonschool <- hs_routes %>%
    mutate(commute = ifelse(hour %in% school_hours, "school_commute", "other")) %>%
    group_by(Route, commute) %>%
    summarize(avg_x = mean(x_hat), .groups = "drop") %>%
    pivot_wider(
      names_from = commute,
      values_from = avg_x,
      values_fill = 0
    )
  
  #Compute a ratio metric to classify routes
  #here, we are computing the the number of students commuting during school hours divided by the number 
  #of students commuting during non-school transportation likely hours. This shows how many more students
  #are riding the bus for drop off, indicating whether or not these routes are likely school bus routes. 
  school_vs_nonschool <- school_vs_nonschool %>%
    mutate(
      commute_ratio = school_commute / other,
      likely_school_route = commute_ratio > 1.3   # change threshold? how many routes do we want
    )
  
  #Keep only likely school routes
  school_routes <- school_vs_nonschool %>%
    filter(likely_school_route == TRUE) %>%
    select(Route)
  
  school_routes <- school_routes$Route
  print(school_routes)
  
  return(school_routes)
}



