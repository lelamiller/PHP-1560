#' @description calculates the average rate of PPSD student ridership on each bus route by hour of the day
#' @param ridership dataframe with columns must contain Route, High.School, Time, Day.of.Week
#' @return data frame with the route, hour of day, and x_hat, the average number of student passengers on a week day

#read in the data
otp <- read.csv("/Users/lelamiller/Downloads/otp_simulated.csv" )
ridership <- read.csv("/Users/lelamiller/Downloads/ridership_simulated.csv")

#load necessary libraries
library(tidyverse)
library(ggplot2)

#filter to the data for just high school students
HS_ridership <- function(ridership){
  
#filter to just high school students on school days
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

return(hs_routes)
}


#Test Run
HS_ridership_result <- HS_ridership(ridership)
write.csv(HS_ridership_result, "/Users/lelamiller/Documents/GitHub/PHP-1560/Data/HS_ridership_result.csv", row.names = FALSE)

#Plots to look at the distribution of student ridership
allroutesx_hat <- ggplot(HS_ridership_result) + geom_line(aes(x = hour, y = x_hat, group = Route, color = Route)) + labs(x= "Hour of the Day", y = "Avg Number of Students")
ggsave("/Users/lelamiller/Documents/GitHub/PHP-1560/Plots/allroutesx_hat.png", plot = allroutesx_hat, width = 6, height = 6, units = "in", dpi = 300)

#from this plot we can see that there are clear spikes in ridership during morning hours, and mid afternoon. Now, we want to identify the routes with the most students. 

