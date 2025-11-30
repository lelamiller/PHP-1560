#' @description determines the bus routes of most interest, as they appear to be used as transportation to and from school by the most students
#' @param HS_ridership_result data frame with the route, hour of day, and x_hat, the average number of student passengers on a week day
#' @return appended HS_ridership_result, excluding routes that do not appear to be school bus routes


#First metric to determine whether a bus is a school route or not:
  #We know high schools start at 7:45 roughly, and they end around 2:30 typically
  #therefore HS student ridership within the hours 6-7 and 2-3 can be assumed to be transportation to and from school
  #1: We can compare ridership across routes during this hour 
  #2: On each route, we can compare average ridership during these hours to the average ridership during non-school hours

HS_ridership_result <- read.csv("/Users/lelamiller/Documents/GitHub/PHP-1560/Data/HS_ridership_result.csv")

HS_Routes <- function(HS_ridership_result){
  
  school_hours <- c(6, 7, 14, 15)

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
    select(Route, school_commute, other, commute_ratio)
  
  return(school_routes)
}

    

