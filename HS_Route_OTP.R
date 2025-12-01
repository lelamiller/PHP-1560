#' @description Route_allocation, considers usual popularity of routes, 
#' optimizes on time performance for an input of school hours based
#' on stops that are most popular among students during current school times. 
#' reccomends a new route or to keep a route for the students commute to school 
#' based on the route with the best otp for a stop during that hour. 
#' @param otp 
#' @return 
#' 
#read in the data
otp <- read.csv("/Users/lelamiller/Downloads/otp_simulated.csv" )
ridership <- read.csv("/Users/lelamiller/Downloads/ridership_simulated.csv")

school_hours <- c(6, 7, 14, 15)

Route_allocation <- function(otp, ridership, school_hours) {
  
#look at the popular stops on the hs routes, how do these overlap between routes, how can we change routes based on otp
routes_stops <- ridership %>%
  filter(High.School == "Providence Public School Department") %>%
  filter(Day.of.Week != "Sat", 
         Day.of.Week != "Sun") %>%
  mutate(
    Time = as.POSIXct(Time, format = "%m/%d/%y %H:%M"),
    hour = as.numeric(format(Time, "%H")),
    date = as.Date(Time)
  ) %>%
  filter(hour %in% school_hours) %>%
  group_by(Stop.Number, Route) %>%
  summarize(nstudents = n()) %>%
  arrange(desc(nstudents))

print(head(routes_stops))

#maybe a loop through most popular stops, to find the route with the best on time performance for that stop during the hour?
  #at this point, I would want to look at the most popular stops otp, but they are coded differently, as 4 letters, is there a way for me to somehow figure out which numebr\
#numeric stop goes with the character stop, can I create a map of sorts between the two and merge the data together

#professor paul is looking into whether there is a mapping for the stop number to the stop name.


}


