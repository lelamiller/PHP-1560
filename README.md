# RIPTA Bus Route Analysis

## Overview

Analyzes RIPTA bus routes to answer : Which Routes are Providence Students taking to school now and are these routes more or less on time during school hours? If start times changed, which routes should Providence students take to school based on their past on-time performance?

**Data:** `otp_simulated.csv` (on-time performance), `ridership_simulated.csv` (student ridership), `stops.txt` (bus stop info)

**Scripts:** `HS_Routes.R` (identifies school routes), `otp_hs_routes.R` (OTP comparison), `Lateness Functions.R` (three lateness metrics), `Plots.R` (visualizations), `New_Start_Time.R` (route recommendations), `Main Script.R` (runs everything)

## How to Run

install.packages(c("tidyverse", "ggplot2", "scales", "lubridate", "gt"))
source("Main Script.R") - Results save to `Results/` folder. 

## Methods

**School Route Identification:** Compares ridership during school hours (6-7am, 2-3pm) vs other times. Routes with ratio >1.3 = school routes.

**OTP Analysis:** Proportion of trips >5 min late during school vs non-school hours.

**Lateness Metrics:** (1) Multiple thresholds (>5, >10, >15 min), percentiles, mean/median delays (2) Reliability score (0-100, penalty-based: 0-5min=0pts, 5-10min=1pt, 10-15min=3pts, 15-20min=5pts, >20min=8pts) (3) Morning vs afternoon comparison.

**Route Recommendations:** Two approaches for popular stops (>10 students/day): minimize proportion late OR maximize reliability score. Provides morning/afternoon recommendations for new school schedules.

## Results

**11 CSV files:** routes morning reliable, routes afternoon reliable, result morning, result afternoon, school routes, otp summary, morning vs afternoon, reliability scores, lateness metrics, HS ridership result

**1 PDF:** 7 plots showing ridership, popular stops, OTP comparisons, lateness analysis, metric comparison

## Key Feature

Dual optimization compares two recommendation strategies: minimizing late frequency vs minimizing delay severity. Shows whether metric choice affects recommendations.

**Adjust parameters in Main Script.R:** `old_school_morning`, `old_school_afternoon`, `new_school_morning`, `new_school_afternoon`, `cutoff`
