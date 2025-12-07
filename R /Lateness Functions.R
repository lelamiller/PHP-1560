#' @description Calculates multiple lateness metrics for bus routes during specified hours
#' This provides alternative ways to quantify and understand bus delays beyond simple binary late/on-time
#' @param otp dataframe with columns: Route, Scheduled.Time, Delay.Sec
#' @param routes vector of route names to analyze (typically school routes)
#' @param school_hours vector of hours (0-23) to analyze (e.g., c(6,7,14,15))
#' @return list containing multiple dataframes with different lateness metrics

library(tidyverse)

#First lateness score that has different definitions of lateness

analyze_lateness_metrics <- function(otp, routes, school_hours) {
  
  lateness_metrics <- otp %>%
    filter(Route %in% routes) %>%
    mutate(
      Scheduled.Time = as.POSIXct(Scheduled.Time, format = "%Y-%m-%d %H:%M:%S"),
      hour = as.numeric(format(Scheduled.Time, "%H")),
      Minutes.Late = Delay.Sec / 60,
      
      # Different lateness categories
      Late_5min = ifelse(Delay.Sec > 300, 1, 0),      # >5 min late
      Late_10min = ifelse(Delay.Sec > 600, 1, 0),     # >10 min late
      Late_15min = ifelse(Delay.Sec > 900, 1, 0),     # >15 min late
      Early = ifelse(Delay.Sec < -60, 1, 0)           # >1 min early
    ) %>%
    filter(hour %in% school_hours) %>%
    group_by(Route) %>%
    summarize(
      # Proportion-based metrics
      prop_late_5min = mean(Late_5min),
      prop_late_10min = mean(Late_10min),
      prop_late_15min = mean(Late_15min),
      prop_early = mean(Early),
      prop_on_time = 1 - mean(Late_5min) - mean(Early),
      
      # Central tendency metrics
      mean_delay_min = mean(Minutes.Late),
      median_delay_min = median(Minutes.Late),
      
      # Variability metrics
      sd_delay_min = sd(Minutes.Late),
      iqr_delay_min = IQR(Minutes.Late),
      range_delay_min = max(Minutes.Late) - min(Minutes.Late),
      
      # Extreme values
      max_delay_min = max(Minutes.Late),
      min_delay_min = min(Minutes.Late),
      percentile_90_delay = quantile(Minutes.Late, 0.90),
      percentile_75_delay = quantile(Minutes.Late, 0.75),
      percentile_25_delay = quantile(Minutes.Late, 0.25),
      
      # Sample size
      n_trips = n(),
      
      .groups = "drop"
    ) %>%
    arrange(desc(prop_late_5min))
  
  return(lateness_metrics)
}

## Second lateness function that uses a penalty score 

reliability_score <- function(otp, routes, school_hours) {
  
  scores <- otp %>%
    filter(Route %in% routes) %>%
    mutate(
      Scheduled.Time = as.POSIXct(Scheduled.Time, format = "%Y-%m-%d %H:%M:%S"),
      hour = as.numeric(format(Scheduled.Time, "%H")),
      Minutes.Late = Delay.Sec / 60,
      
      # Penalty scoring system - heavier penalties for longer delays
      penalty = case_when(
        Minutes.Late <= 5 ~ 0,              # No penalty if on time (±5 min)
        Minutes.Late > 5 & Minutes.Late <= 10 ~ 1,    # Light penalty
        Minutes.Late > 10 & Minutes.Late <= 15 ~ 3,   # Moderate penalty
        Minutes.Late > 15 & Minutes.Late <= 20 ~ 5,   # Heavy penalty
        Minutes.Late > 20 ~ 8,              # Severe penalty
        TRUE ~ 0
      )
    ) %>%
    filter(hour %in% school_hours) %>%
    group_by(Route) %>%
    summarize(
      avg_penalty = mean(penalty),
      max_penalty = max(penalty),
      reliability_score = pmax(0, 100 - (avg_penalty * 10)),  # Score out of 100
      n_trips = n(),
      .groups = "drop"
    ) %>%
    arrange(desc(reliability_score))
  
  return(scores)
}

### Third lateness function that focuses on morning vs afternoon hours 

time_period_comparison <- function(otp, routes, morning_hours, afternoon_hours) {
  
  comparison <- otp %>%
    filter(Route %in% routes) %>%
    mutate(
      Scheduled.Time = as.POSIXct(Scheduled.Time, format = "%Y-%m-%d %H:%M:%S"),
      hour = as.numeric(format(Scheduled.Time, "%H")),
      Minutes.Late = Delay.Sec / 60,
      
      time_period = case_when(
        hour %in% morning_hours ~ "Morning",
        hour %in% afternoon_hours ~ "Afternoon",
        TRUE ~ "Other"
      ),
      
      Late_5min = ifelse(Delay.Sec > 300, 1, 0)
    ) %>%
    filter(time_period != "Other") %>%
    group_by(Route, time_period) %>%
    summarize(
      prop_late = mean(Late_5min),
      mean_delay = mean(Minutes.Late),
      median_delay = median(Minutes.Late),
      sd_delay = sd(Minutes.Late),
      n_trips = n(),
      .groups = "drop"
    ) %>%
    pivot_wider(
      names_from = time_period,
      values_from = c(prop_late, mean_delay, median_delay, sd_delay, n_trips)
    ) %>%
    mutate(
      diff_prop_late = prop_late_Afternoon - prop_late_Morning,
      diff_mean_delay = mean_delay_Afternoon - mean_delay_Morning
    )
  
  return(comparison)
}

