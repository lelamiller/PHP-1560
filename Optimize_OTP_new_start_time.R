#' @description Route_allocation, considers usual popularity of routes, 
#' optimizes on time performance for an input of school hours based
#' on stops that are most popular among students during current school times. 
#' recommends a new route or to keep a route for the students commute to school 
#' based on the route with the best otp for a stop during that hour. For the 50 most popular bus stops
#' @param ridership data frame with the route, type of rider (High.School), 
#' @param otp a data frame of bus routes, their arrival times and scheduled arrival times. Indicates the on time performance of busses. 
#' @param school_hours a vector of hours that students would be riding the bus based on school start time and release time
#' @return recommended_routes, a data frame with the original popular routes, and then new recommended route if that 
#' route is not the best on time performance during the school hours 

#read in the data
otp <- read.csv("/Users/lelamiller/Downloads/otp_simulated.csv" )
ridership <- read.csv("/Users/lelamiller/Downloads/ridership_simulated.csv")
#MAP FOR STOPS:
stops <- read.delim("/Users/lelamiller/Documents/GitHub/PHP-1560/Data/stops.txt", sep = ",")

new_school_morning <- c(6, 7)
new_school_afternoon <- c(14, 15)
  #include hour before start time, hour during start time
  #include hour of end time and hour after end time
old_school_morning <- c(6, 7)
old_school_afternoon <- c(14, 15)
  
Route_allocation <- function(otp, ridership, school_hours) {
  
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
  summarize(nstudents = n()) %>%
  arrange(desc(nstudents)) %>%
  group_by(Stop.Number)

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
  summarize(nstudents = n()) %>%
  arrange(desc(nstudents)) %>%
  group_by(Stop.Number)



#use the map of stops to add stop_id to the otp dataframe, this will allow us to compare the routes by the stops the students ride
stops <- stops %>%
  select(stop_id, stop_associated_place) %>%
  rename(Stop = stop_associated_place)

otp <- left_join(otp, stops)

otp <- otp %>%
  mutate(
    Scheduled.Time = as.POSIXct(Scheduled.Time, format = "%Y-%m-%d %H:%M:%S"),
    hour = hour(Scheduled.Time),
    Late = ifelse(Delay.Sec > 300, 1, 0)    # flag lateness > 5 min
  )




#maybe a loop through most popular stops, to find the route with the best on time performance for that stop during the hour?
  #at this point, I would want to look at the most popular stops otp, but they are coded differently, as 4 letters, is there a way for me to somehow figure out which numebr\
#numeric stop goes with the character stop, can I create a map of sorts between the two and merge the data together



}


