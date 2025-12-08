#' @description Route_allocation, considers usual popularity of routes, 
#' optimizes on time performance for an input of school hours based
#' on stops that are most popular among students during current school times. 
#' recommends a new route or to keep a route for the students commute to school 
#' based on the route with the best otp for a stop during that hour. For the 50 most popular bus stops
#' @param ridership data frame with the route, type of rider (High.School), 
#' @param otp a data frame of bus routes, their arrival times and scheduled arrival times. Indicates the on time performance of busses. 
#' @param old_school_morning a vector of hours that students would be riding the bus in the morning based on initial school start times
#' @param old_school_afternoon a vector of hours that students would be riding the bus in the afternoon based on initial school end times
#' @param new_school_morning a vector of hours that students would be riding the bus in the morning based on proposed school start times
#' @param new_school_afternoon a vector of hours that students would be riding the bus in the afternoon based on proposed school start times
#' @param stops a data frame of bus stop id and 4 letter place code, for merging stop_id into our otp dataframe
#' @param cutoff a integer for the number of students per bus stop per day to limit our list of bus stops to recommend routes for
#' @param new_school_hours <- c(new_school_morning, new_school_afternoon)
#' @return recommended_routes_morning, a data frame with the needed morning bus stops, and the recommended route for the best on time performance based on 5 min lateness
#' @return recommended_routes_afternoon, a data frame with the needed afternoon bus stops, and the recommended route for the best on time performance based on 5 min lateness
#' @return morning_reliability_based, dataframe with needed morning bus stops, and the recommended route for the best on time performance based on reliability score 
#' @return afternoon_reliability_based, dataframe with needed afternoon bus stops, and the recommended route for the best on time performance based on reliability score 




#load libraries
library(tidyverse)


Route_allocation <- function(otp, ridership, new_school_morning, 
                             new_school_afternoon, old_school_morning, 
                             old_school_afternoon, stops, cutoff, new_school_hours) {

#look at the popular stops on the hs routes, how do these overlap between routes, how can we change routes based on otp
morning_stops <- ridership %>%
  filter(High.School == "Providence Public School Department") %>%
  filter(Day.of.Week != "Sat", 
         Day.of.Week != "Sun") %>%
  mutate(
    Time = as.POSIXct(Time, format = "%m/%d/%y %H:%M"),
    hour = as.numeric(format(Time, "%H")),
    date = as.Date(Time)
  ) %>%
  filter(hour %in% old_school_morning) %>%
  group_by(Stop.Number, Route) %>%
  summarize(nstudents = n()/n_distinct(date)) %>%
  arrange(desc(nstudents)) %>%
  group_by(Stop.Number) %>%
  filter(nstudents > cutoff)
#PLOT IDEA HERE AND FOR AFTERNOON_STOPS?

morning_unique_stops <- (unique(c(morning_stops$Stop.Number)))

#Which stops to students ride in the afternoon?
afternoon_stops <- ridership %>%
  filter(High.School == "Providence Public School Department") %>%
  filter(Day.of.Week != "Sat", 
         Day.of.Week != "Sun") %>%
  mutate(
    Time = as.POSIXct(Time, format = "%m/%d/%y %H:%M"),
    hour = as.numeric(format(Time, "%H")),
    date = as.Date(Time)
  ) %>%
  filter(hour %in% old_school_afternoon) %>%
  group_by(Stop.Number, Route) %>%
  summarize(nstudents = n()/n_distinct(date)) %>%
  arrange(desc(nstudents)) %>%
  group_by(Stop.Number) %>%
  filter(nstudents > cutoff)

afternoon_unique_stops <- (unique(c(afternoon_stops$Stop.Number)))

#use the map of stops to add stop_id to the otp dataframe, this will allow us to compare the routes by the stops the students ride
stop_join <- stops %>%
  select(stop_id, stop_associated_place) %>%
  rename(Stop = stop_associated_place)

#this shows the proportion late overall for each route and stop and hour
stop_otp <- left_join(otp, stop_join) %>%
  mutate(
    Scheduled.Time = as.POSIXct(Scheduled.Time, format = "%Y-%m-%d %H:%M:%S"),
    hour = hour(Scheduled.Time),
    Late = ifelse(Delay.Sec > 300, 1, 0)    # flag lateness > 5 min
  ) %>%
  group_by(stop_id, Route, hour) %>%
  summarise(proplate = sum(Late)/n())

### ADDING THE RELIABILITY SCORE INTO OUR ROUTE ALLOCATION PROCESS

# Identify all unique routes serving HS stops
routes_for_scoring <- unique(stop_otp$Route)

# Compute reliability scores for these routes using the existing function
reliability_table_morning <- reliability_score(otp, routes_for_scoring, new_school_morning)
reliability_table_afternoon <- reliability_score(otp, routes_for_scoring, new_school_afternoon)

# Join the scores into stop_otp so we can compare routes by stop
stop_otp <- stop_otp %>%
  left_join(reliability_table_morning %>% select(Route, reliability_score), by = "Route",
            relationship = "many-to-one") %>%
  rename(reliability_morning = reliability_score) %>%
  left_join(reliability_table_afternoon %>% select(Route, reliability_score), by = "Route",
            relationship = "many-to-one") %>%
  rename(reliability_afternoon = reliability_score)



stop_filter <- unique(stop_otp$stop_id)

#ensure no values in the school stop data arent in the otp data
morning_unique_stops <- morning_unique_stops[morning_unique_stops %in% stop_filter]
afternoon_unique_stops <- afternoon_unique_stops[afternoon_unique_stops %in% stop_filter]

#morning allocation loop 
morning_chosen_route <- c()

for(i in 1:length(morning_unique_stops)){
  
  #current stop for each run of the loop
  current_stop <- morning_unique_stops[i]
  
  find_best_route <- stop_otp %>%
    filter(stop_id == current_stop) %>%
    filter(hour %in% new_school_morning) 
  
  morning_chosen_route[i] <- find_best_route$Route[which.min(find_best_route$proplate)]
  
}

#afternoon allocation loop
afternoon_chosen_route <- c()

for(i in 1:length(afternoon_unique_stops)){
  
  #current stop for each run of the loop
  current_stop <- afternoon_unique_stops[i]
  
  find_best_route <- stop_otp %>%
    filter(stop_id == current_stop) %>%
    filter(hour %in% new_school_afternoon) 
  
  afternoon_chosen_route[i] <- find_best_route$Route[which.min(find_best_route$proplate)]
  
}


### ALLOCATION BASED ON RELIABILITY SCORE 

# Morning reliability allocation
morning_reliable <- c()

for(i in seq_along(morning_unique_stops)) {
  current_stop <- morning_unique_stops[i]
  
  find_best_route <- stop_otp %>%
    filter(stop_id == current_stop,
           hour %in% new_school_morning)
  
  morning_reliable[i] <- find_best_route$Route[
    which.max(find_best_route$reliability_morning)
  ]
}

# Afternoon reliability allocation
afternoon_reliable <- c()

for(i in seq_along(afternoon_unique_stops)) {
  current_stop <- afternoon_unique_stops[i]
  
  find_best_route <- stop_otp %>%
    filter(stop_id == current_stop,
           hour %in% new_school_afternoon)
  
  afternoon_reliable[i] <- find_best_route$Route[
    which.max(find_best_route$reliability_afternoon)
  ]
}


recommended_routes_morning <- data.frame(
  stop = morning_unique_stops,
  recommended_route = morning_chosen_route
)

recommended_routes_afternoon <- data.frame(
  stop = afternoon_unique_stops,
  recommended_route = afternoon_chosen_route
)

morning_reliability_based = data.frame(
  stop = morning_unique_stops,
  recommended_route = morning_reliable
)

afternoon_reliability_based = data.frame(
  stop = afternoon_unique_stops,
  recommended_route = afternoon_reliable
)

return(list(recommended_routes_morning, recommended_routes_afternoon, morning_reliability_based, afternoon_reliability_based))
}


#add quantifying lateness in different ways, doing the distribution 


#read in the data
#otp <- read.csv("/Users/lelamiller/Downloads/otp_simulated.csv" )
#ridership <- read.csv("/Users/lelamiller/Downloads/ridership_simulated.csv")
#MAP FOR STOPS:
#stops <- read.delim("/Users/lelamiller/Documents/GitHub/PHP-1560/stops.txt", sep = ",")

# Nikhil Data
#otp <- read.csv("/Users/nikhilsonthalia/Downloads/PHP-1560/Data/otp_simulated.csv" )
#ridership <- read.csv("/Users/nikhilsonthalia/Downloads/PHP-1560/Data/ridership_simulated.csv")
#stops <- read.delim("/Users//nikhilsonthalia/Downloads/PHP-1560/Data/stops.txt", sep = ",")

#new_school_morning <- c(6, 7)
#new_school_afternoon <- c(14, 15)
#include hour before start time, hour during start time
#include hour of end time and hour after end time
#old_school_morning <- c(6, 7)
#old_school_afternoon <- c(14, 15)

#cutoff <- 10

#test run of function:
#Route_allocation(otp, ridership, new_school_morning, 
                             #new_school_afternoon, old_school_morning, 
                             #old_school_afternoon, stops, cutoff)

