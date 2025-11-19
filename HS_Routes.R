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
