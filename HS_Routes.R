otp <- read.csv("/Users/lelamiller/Downloads/otp_simulated.csv" )
ridership <- read.csv("/Users/lelamiller/Downloads/ridership_simulated.csv")

#load necessary libraries
library(tidyverse)

HS_Routes <- function(ridership){
  only_hs <- ridership %>%
    filter(High.School != "None") 
  return(only_hs)
}

only_hs <- HS_Routes(ridership)

#look at the distribution for routes in which kids are riding regularly
hs_routes <- only_hs %>%
  mutate(
    Time = as.POSIXct(Time, format = "%m/%d/%y %H:%M"),
    hour = as.numeric(format(Time, "%H")),
    date = as.Date(Time)
  ) %>%
  group_by(Route, date) %>%
  summarise(nstudents = n())

#now I have a data frame of the number of students that ride each route in each day. Lets look at an example day, 


