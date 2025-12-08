#' @description Creates all visualizations for Bus Route Analysis
#' This script generates plots for ridership patterns, OTP comparisons, 
#' popular stops, and lateness metrics

# Load necessary libraries
library(tidyverse)
library(ggplot2)
library(scales)


#' Plot 1: Overall ridership by hour across all routes
plot_ridership_by_hour <- function(ridership) {
  
  # Filter to just high school students on school days
  only_hs <- ridership %>%
    filter(High.School == "Providence Public School Department") %>%
    filter(Day.of.Week != "Sat", Day.of.Week != "Sun")
  
  # Calculate average students per route per hour
  hs_routes <- only_hs %>%
    mutate(
      Time = as.POSIXct(Time, format = "%m/%d/%y %H:%M"),
      hour = as.numeric(format(Time, "%H")),
      date = as.Date(Time)
    ) %>%
    group_by(Route, hour) %>%
    summarize(x_hat = n()/n_distinct(date), .groups = "drop")
  
  hs_routes$Route <- as.factor(hs_routes$Route)
  
  # Create plot
  p <- ggplot(hs_routes, aes(x = hour, y = x_hat, group = Route, color = Route)) + 
    geom_line(alpha = 0.7) + 
    labs(title = "Average Daily Student Ridership by Hour and Route",
         subtitle = "Providence Public School District High School Students",
         x = "Hour of the Day", 
         y = "Average Number of Students") +
    theme_minimal() +
    theme(legend.position = "none")  # Too many routes to show legend
  
  return(p)
}

#' Plot 2: Popular morning stops
plot_morning_stops <- function(ridership, old_school_morning, cutoff = 10) {
  
  morning_stops <- ridership %>%
    filter(High.School == "Providence Public School Department") %>%
    filter(Day.of.Week != "Sat", Day.of.Week != "Sun") %>%
    mutate(
      Time = as.POSIXct(Time, format = "%m/%d/%y %H:%M"),
      hour = as.numeric(format(Time, "%H")),
      date = as.Date(Time)
    ) %>%
    filter(hour %in% old_school_morning) %>%
    group_by(Stop.Number, Route) %>%
    summarize(nstudents = n()/n_distinct(date), .groups = "drop") %>%
    arrange(desc(nstudents)) %>%
    group_by(Stop.Number) %>%
    filter(nstudents > cutoff) %>%
    slice_head(n = 1)  # Keep only the most popular route per stop
  
  # Get top 20 stops
  top_stops <- morning_stops %>%
    arrange(desc(nstudents)) %>%
    slice_head(n = 20)
  
  p <- ggplot(top_stops, aes(x = reorder(Stop.Number, nstudents), y = nstudents, fill = Route)) + 
    geom_col() + 
    coord_flip() +
    labs(title = "Top 20 Morning Bus Stops by Student Ridership",
         subtitle = paste("Stops with >", cutoff, "average daily riders during morning hours"),
         x = "Bus Stop", 
         y = "Average Daily Students",
         fill = "Most Used Route") +
    theme_minimal()
  
  return(p)
}

#' Plot 3: Popular afternoon stops
plot_afternoon_stops <- function(ridership, old_school_afternoon, cutoff = 10) {
  
  afternoon_stops <- ridership %>%
    filter(High.School == "Providence Public School Department") %>%
    filter(Day.of.Week != "Sat", Day.of.Week != "Sun") %>%
    mutate(
      Time = as.POSIXct(Time, format = "%m/%d/%y %H:%M"),
      hour = as.numeric(format(Time, "%H")),
      date = as.Date(Time)
    ) %>%
    filter(hour %in% old_school_afternoon) %>%
    group_by(Stop.Number, Route) %>%
    summarize(nstudents = n()/n_distinct(date), .groups = "drop") %>%
    arrange(desc(nstudents)) %>%
    group_by(Stop.Number) %>%
    filter(nstudents > cutoff) %>%
    slice_head(n = 1)
  
  # Get top 20 stops
  top_stops <- afternoon_stops %>%
    arrange(desc(nstudents)) %>%
    slice_head(n = 20)
  
  p <- ggplot(top_stops, aes(x = reorder(Stop.Number, nstudents), y = nstudents, fill = Route)) + 
    geom_col() + 
    coord_flip() +
    labs(title = "Top 20 Afternoon Bus Stops by Student Ridership",
         subtitle = paste("Stops with >", cutoff, "average daily riders during afternoon hours"),
         x = "Bus Stop", 
         y = "Average Daily Students",
         fill = "Most Used Route") +
    theme_minimal()
  
  return(p)
}

# ========== OTP COMPARISON PLOTS ==========

#' Plot 4: OTP comparison school vs non-school hours
plot_otp_comparison <- function(otp_summary) {
  
  plot_data <- otp_summary %>%
    select(school_period, starts_with("prop_")) %>%
    pivot_longer(cols = starts_with("prop_"), 
                 names_to = "status", 
                 values_to = "proportion") %>%
    mutate(status = gsub("prop_", "", status),
           status = gsub("_", " ", status))
  
  p <- ggplot(plot_data, aes(x = school_period, y = proportion, fill = status)) +
    geom_col(position = "stack") +
    geom_text(aes(label = paste0(round(proportion * 100, 1), "%")),
              position = position_stack(vjust = 0.5),
              color = "white", fontface = "bold", size = 5) +
    scale_y_continuous(labels = percent_format()) +
    scale_fill_manual(values = c("Late" = "#d62728", "On Time/Early" = "#2ca02c")) +
    labs(title = "On-Time Performance: School Hours vs Non-School Hours",
         subtitle = "Routes serving PPSD High School students (>5 min late threshold)",
         x = "Time Period", 
         y = "Percentage of Trips", 
         fill = "Status") +
    theme_minimal(base_size = 14) +
    theme(legend.position = "bottom")
  
  return(p)
}

# ========== LATENESS METRIC PLOTS ==========

#' Plot 5: Multiple lateness thresholds
plot_lateness_thresholds <- function(lateness_metrics, top_n = 10) {
  
  plot_data <- lateness_metrics %>%
    arrange(desc(prop_late_5min)) %>%
    slice_head(n = top_n) %>%
    select(Route, prop_late_5min, prop_late_10min, prop_late_15min) %>%
    pivot_longer(cols = starts_with("prop_late"), 
                 names_to = "threshold", 
                 values_to = "proportion") %>%
    mutate(threshold = case_when(
      threshold == "prop_late_5min" ~ ">5 min late",
      threshold == "prop_late_10min" ~ ">10 min late",
      threshold == "prop_late_15min" ~ ">15 min late"
    ))
  
  p <- ggplot(plot_data, aes(x = reorder(Route, proportion), 
                             y = proportion, fill = threshold)) +
    geom_col(position = "dodge") +
    coord_flip() +
    scale_y_continuous(labels = percent_format()) +
    scale_fill_manual(values = c(">5 min late" = "#ff7f0e", 
                                 ">10 min late" = "#d62728",
                                 ">15 min late" = "#8b0000")) +
    labs(title = "Proportion Late by Different Thresholds",
         subtitle = paste("Top", top_n, "routes with worst on-time performance"),
         x = "Route",
         y = "Proportion of Trips",
         fill = "Lateness Definition") +
    theme_minimal() +
    theme(legend.position = "bottom")
  
  return(p)
}

#' Plot 6: Reliability scores
plot_reliability_scores <- function(reliability_scores, top_n = 10, bottom_n = 10) {
  
  best_routes <- reliability_scores %>%
    arrange(desc(reliability_score)) %>%
    slice_head(n = top_n) %>%
    mutate(category = "Most Reliable")
  
  worst_routes <- reliability_scores %>%
    arrange(reliability_score) %>%
    slice_head(n = bottom_n) %>%
    mutate(category = "Least Reliable")
  
  plot_data <- bind_rows(best_routes, worst_routes)
  
  p <- ggplot(plot_data, aes(x = reorder(Route, reliability_score), 
                             y = reliability_score, fill = category)) +
    geom_col() +
    geom_hline(yintercept = 80, linetype = "dashed", color = "darkgreen", linewidth = 1) +
    annotate("text", x = 1, y = 82, label = "80 = Highly Reliable", 
             hjust = 0, color = "darkgreen", size = 3) +
    geom_hline(yintercept = 60, linetype = "dashed", color = "orange", linewidth = 1) +
    annotate("text", x = 1, y = 62, label = "60 = Acceptable", 
             hjust = 0, color = "orange", size = 3) +
    coord_flip() +
    scale_fill_manual(values = c("Most Reliable" = "#2ca02c", 
                                 "Least Reliable" = "#d62728")) +
    labs(title = "Route Reliability Scores",
         subtitle = "Penalty-based scoring system: 100 = perfect, 0 = worst",
         x = "Route",
         y = "Reliability Score (0-100)",
         fill = "") +
    theme_minimal() +
    theme(legend.position = "bottom")
  
  return(p)
}

#' Plot 7: Morning vs Afternoon comparison
plot_morning_afternoon_comparison <- function(time_comparison) {
  
  plot_data <- time_comparison %>%
    select(Route, prop_late_Morning, prop_late_Afternoon) %>%
    pivot_longer(cols = c(prop_late_Morning, prop_late_Afternoon),
                 names_to = "period", values_to = "prop_late") %>%
    mutate(period = ifelse(period == "prop_late_Morning", "Morning", "Afternoon"))
  
  # Get top 15 routes by total lateness
  top_routes <- plot_data %>%
    group_by(Route) %>%
    summarize(total_late = sum(prop_late), .groups = "drop") %>%
    arrange(desc(total_late)) %>%
    slice_head(n = 15) %>%
    pull(Route)
  
  plot_data <- plot_data %>%
    filter(Route %in% top_routes)
  
  p <- ggplot(plot_data, aes(x = reorder(Route, prop_late), 
                             y = prop_late, fill = period)) +
    geom_col(position = "dodge") +
    coord_flip() +
    scale_y_continuous(labels = percent_format()) +
    scale_fill_manual(values = c("Morning" = "#ff7f0e", "Afternoon" = "#9467bd")) +
    labs(title = "Proportion Late: Morning vs Afternoon",
         subtitle = "Top 15 routes by overall lateness",
         x = "Route", 
         y = "Proportion of Trips Late (>5 min)",
         fill = "Time Period") +
    theme_minimal() +
    theme(legend.position = "bottom")
  
  return(p)
}

#' Plot 8: Time difference (which routes are worse at different times)
plot_time_difference <- function(time_comparison, top_n = 10) {
  
  # Routes where afternoon is much worse
  worse_afternoon <- time_comparison %>%
    arrange(desc(diff_mean_delay)) %>%
    slice_head(n = top_n) %>%
    mutate(category = "Worse in Afternoon")
  
  # Routes where morning is much worse  
  worse_morning <- time_comparison %>%
    arrange(diff_mean_delay) %>%
    slice_head(n = top_n) %>%
    mutate(category = "Worse in Morning")
  
  plot_data <- bind_rows(worse_afternoon, worse_morning)
  
  p <- ggplot(plot_data, aes(x = reorder(Route, diff_mean_delay), 
                             y = diff_mean_delay, fill = category)) +
    geom_col() +
    geom_hline(yintercept = 0, linetype = "solid", color = "black") +
    coord_flip() +
    scale_fill_manual(values = c("Worse in Afternoon" = "#d62728", 
                                 "Worse in Morning" = "#1f77b4")) +
    labs(title = "Routes with Biggest Performance Differences by Time of Day",
         subtitle = "Positive values = worse in afternoon | Negative values = worse in morning",
         x = "Route",
         y = "Difference in Mean Delay (Afternoon - Morning, minutes)",
         fill = "") +
    theme_minimal() +
    theme(legend.position = "bottom")
  
  return(p)
}

# ========== MAIN PLOTTING FUNCTION ==========

#' Generate all plots and save to PDF
generate_all_plots <- function(ridership, otp, school_routes, 
                               old_school_morning, old_school_afternoon,
                               otp_summary, lateness_metrics, 
                               reliability_scores, time_comparison,
                               output_file = "/Users/nikhilsonthalia/Downloads/PHP-1560/Results/PPSD_Bus_Analysis_Plots.pdf") {
  
  cat("Generating all plots...\n")
  
  pdf(output_file, width = 12, height = 8)
  
  # Ridership plots
  cat("1. Creating ridership by hour plot...\n")
  print(plot_ridership_by_hour(ridership))
  
  cat("2. Creating morning stops plot...\n")
  print(plot_morning_stops(ridership, old_school_morning, cutoff = 10))
  
  cat("3. Creating afternoon stops plot...\n")
  print(plot_afternoon_stops(ridership, old_school_afternoon, cutoff = 10))
  
  # OTP comparison
  cat("4. Creating OTP comparison plot...\n")
  print(plot_otp_comparison(otp_summary))
  
  # Lateness metrics
  cat("5. Creating lateness thresholds plot...\n")
  print(plot_lateness_thresholds(lateness_metrics, top_n = 10))
  
  cat("6. Creating reliability scores plot...\n")
  print(plot_reliability_scores(reliability_scores, top_n = 10, bottom_n = 10))
  
  cat("7. Creating morning vs afternoon comparison...\n")
  print(plot_morning_afternoon_comparison(time_comparison))
  
  cat("8. Creating time difference plot...\n")
  print(plot_time_difference(time_comparison, top_n = 10))
  
  dev.off()
  
  cat("\nAll plots saved to", output_file, "\n")
}


#PLOTS/TABLES OF FUNCTION OUTPUT

library(tidyverse)
library(ggplot2)

#read in data 
routes_morning <- read.csv( "/Users/lelamiller/Documents/GitHub/PHP-1560/Results/result_morning.csv")
routes_afternoon <- read.csv("/Users/lelamiller/Documents/GitHub/PHP-1560/Results/result_afternoon.csv")
routes_morning_reliable <- read.csv("/Users/lelamiller/Documents/GitHub/PHP-1560/Results/routes_morning_reliable.csv")
routes_afternoon_reliable <- read.csv("/Users/lelamiller/Documents/GitHub/PHP-1560/Results/routes_afternoon_reliable.csv")

# Rename route column consistently
routes_morning <- routes_morning %>% rename(route = recommended_route)
routes_afternoon <- routes_afternoon %>% rename(route = recommended_route)
routes_morning_reliable <- routes_morning_reliable %>% rename(route = recommended_route)
routes_afternoon_reliable <- routes_afternoon_reliable %>% rename(route = recommended_route)

# Add allocation labels
routes_morning$allocation <- "Morning - % Late (5 min)"
routes_afternoon$allocation <- "Afternoon - % Late (5 min)"
routes_morning_reliable$allocation <- "Morning - Reliability Score"
routes_afternoon_reliable$allocation <- "Afternoon - Reliability Score"

# Combine into one dataframe
routes_all <- bind_rows(routes_morning, 
                        routes_afternoon, 
                        routes_morning_reliable, 
                        routes_afternoon_reliable)

library(gt)

gt(routes_morning)
gt(routes_afternoon)
gt(routes_morning_reliable)
gt(routes_afternoon_reliable)


