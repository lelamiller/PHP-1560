#' @description  determines the on time performance as a proportion of busses late by more than 5 minutes during school hours and outside of school hours. 
#' @param ridership data frame with the route, type of rider (High.School), 
#' @param otp a dataframe of bus routes, their arrival times and scheduled arrival times. Indicates the on time performance of busses. 
#' @param school_hours a vector of hours that students would be riding the bus based on school start time and release time
#' @return otp_summary, a comparison of the proportion of late arrivals on school routes during school hours and non school hours

#Load in data
#otp <- read.csv("/Users/lelamiller/Downloads/otp_simulated.csv" )
#ridership <- read.csv("/Users/lelamiller/Downloads/ridership_simulated.csv")

# Nikhil Data
#otp <- read.csv("/Users/nikhilsonthalia/Downloads/PHP-1560/Data/otp_simulated.csv" )
#ridership <- read.csv("/Users/nikhilsonthalia/Downloads/PHP-1560/Data/ridership_simulated.csv")

#load libraries
#library(tidyverse)

#Set old school hours (current start time)
#old_school_hours <- c(6, 7, 14, 15)

#set new school hours, can be the same or can be different as the old start time
#school_hours <- c(6, 7, 14, 15)

#Start the function:
otp_school_routes <- function(otp, ridership, old_school_hours, school_hours){

#compute school routes by calling our first function
school_routes <- HS_Routes(ridership, otp, old_school_hours)

#filter the on time performance data to be the likely school routes 
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

#PLOT OTP SUMMARY FOR SLIDE 5?
}