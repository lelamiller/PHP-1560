#' @description determines the bus routes of most interest, as they appear to be used as transportation to and from school by the most students.
#' Also, determines the on time performance as a proportion of busses late by more than 5 minutes during school hours and outside of school hours. 
#' @param HS_ridership_result data frame with the route, hour of day, and x_hat, the average number of student passengers on a week day
#' @param otp a dataframe of bus routes, their arrival times and scheduled arrival times. Indicates the on time performance of busses. 
#' @param school_hours a vector of hours that students would be riding the bus based on school start time and release time
#' @return otp_summary, a comparison of the proportion of late arrivals on school routes during school hours and non school hours


#First metric to determine whether a bus is a school route or not:
  #We know high schools start at 7:45 roughly, and they end around 2:30 typically
  #therefore HS student ridership within the hours 6-7 and 2-3 can be assumed to be transportation to and from school
  #1: We can compare ridership across routes during this hour 
  #2: On each route, we can compare average ridership during these hours to the average ridership during non-school hours

HS_ridership_result <- read.csv("/Users/lelamiller/Documents/GitHub/PHP-1560/Data/HS_ridership_result.csv")
otp <- read.csv("/Users/lelamiller/Downloads/otp_simulated.csv" )

library(tidyverse)

school_hours <- c(6, 7, 14, 15)

HS_Routes <- function(HS_ridership_result, otp, school_hours){
  
#comparing school hour riders to non school hours riders
  school_vs_nonschool <- HS_ridership_result %>%
    mutate(commute = ifelse(hour %in% school_hours, "school_commute", "other")) %>%
    group_by(Route, commute) %>%
    summarize(avg_x = mean(x_hat), .groups = "drop") %>%
    pivot_wider(
      names_from = commute,
      values_from = avg_x,
      values_fill = 0
    )
  
#Compute a ratio metric to classify routes
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
  
#Now, filter the on time performance data to be those routes
  otp_summary <- otp %>%
    filter(Route %in% school_routes) %>%
    #mutate to add columns for hours, indicator for school hours, and indicator for late
    mutate(
      Scheduled.Time = as.POSIXct(Scheduled.Time, format = "%Y-%m-%d %H:%M:%S"),
      hour = as.numeric(format(Scheduled.Time, "%H")),
      Late = ifelse(Delay.Sec > 300, "Late", "On Time/Early"),
      school_period = ifelse(hour %in% school_hours, "school_hours", "nonschool_hours")
    ) %>%
    #find the number and proportion late and on time by whether or not its during school hours
    group_by(school_period, Late) %>%
    summarize(n = n(), .groups = "drop") %>%
    group_by(school_period) %>%
    mutate(prop = n / sum(n)) %>%
    pivot_wider(
      names_from = Late,
      values_from = prop,
      names_prefix = "prop_"
    )

  return(otp_summary)
}

    

