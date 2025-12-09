# RIPTA Bus Route Analysis

## Overview

Analyzes RIPTA bus routes to answer : Which Routes are Providence Students taking to school now and are these routes more or less on time during school hours? If start times changed, which routes should Providence students take to school based on their past on-time performance?
To answer these questions, we have created a series of functions. Our first determines which routes students are riding during school hours. 
Our next compares the on time performance of school routes during school hours to non-school hours. Our next few measure ontime performance using various lateness metrics. Our final takes in old hours and new hours to reallocate students to new bus routes based on their on time performance.

**Data:** The first dataset is called `otp_simulated.csv` and the variables of interest in this dataset are Route, stop, Scheduled.Time, Actual.Arrival.Time, and Delay.Sec. It shows the on time performance for each trip a bus makes. 
The second dataset is called `ridership_simulated.csv` and the variables of interest in this dataset are Time, Stop.Number, Route, High.School (an indicator for whether a rider is a high school student). This dataset shows us which routes and stops high school students are using. 
The last dataset is called `stops.txt`and it allows us to merge the stop id numbers into the otp dataset that originally only has stops as four letter codes. 

**Scripts:** `HS_Routes.R` (identifies school routes), `otp_hs_routes.R` (OTP comparison), `Lateness Functions.R` (three lateness metrics), `Plots.R` (visualizations), `New_Start_Time.R` (route recommendations), `Main Script.R` (runs everything)


##Functions
### HS_Routes Function
Housed in the file: HS_Routes.R

Function: HS_Routes()

Input: ridership (data on high school students ridership), otp (on time performance of routes), school_hours (hours when students are transported to and from school)

This function determines the most popular routes among high school students by filtering the data to PPSD students, calculating the average number of students riding per day and computing a commute ratio comparing student average ridership during school hours vs nonschool hours. Then it filters for the routes where the ratio is above a threshold. 

Output: school_routes a vector of likely routes used for high school students commutes. 

### otp_school_routes Function
Housed in the file: otp_hs_routes.R

Function: otp_school_routes

1. Identifies Likely School Routes: Calls the HS_Routes() function to determine which bus routes are considered “school routes” based on historical student ridership during the originally scheduled school hours (old_school_hours).
2. Filters OTP Data to Only Those Routes: restricts the otp dataset to trips belonging to routes identified as school routes.
3. Creates New Variables: hour: the hour of each scheduled bus arrival, Late: categorizes each trip as "Late" if it arrived more than 300 seconds (5 min) after the scheduled time, otherwise "On Time/Early", school_period: labels each trip as occurring during school_hours or nonschool_hours
4. Summarizes Lateness by Time Period: Groups the data by whether the trip occurred during school or non-school hours and computes: the number of late vs on-time trips (n), the proportion of trips in each category within each time period
5. Reshapes the Output: Converts the lateness indicators into separate columns (prop_Late and prop_On Time/Early) for easier comparison.
6. Returns a Summary Table: Provides a tidy summary showing the proportion of late and on-time trips for school routes during: school commute hours, non-school hours

Output: otp_summary

### analyze_lateness_metrics Function

Housed in: Lateness Functions.R
Inputs: otp, routes, school_hours

This function computes a wide set of lateness statistics for each route during specified school commuting hours. It filters OTP data to the selected routes and time window, converts delays into minutes, and classifies lateness into multiple thresholds (>5, >10, >15 minutes late, and early arrivals). It then summarizes lateness patterns for each route, including proportions late, early, and on time, measures of central tendency (mean and median delay), variability (standard deviation and range), and extreme delay percentiles. The function returns a single dataframe ranking routes by their proportion of >5-minute late trips and displaying all lateness-related metrics.

Output: Dataframe summarizing lateness characteristics for each route.


###reliability_score Function

Housed in: Lateness Functions.R
Inputs: otp, routes, school_hours

This function generates a reliability score for each bus route, incorporating both the frequency and severity of delays. It assigns penalty values based on how late each trip is, with longer delays receiving higher penalties. For each route, it calculates the average and maximum penalties, converts the average penalty into a 0–100 reliability score (higher score indicates more reliable performance), and reports the number of trips evaluated. Routes are ranked from highest to lowest reliability.

Output: Dataframe containing average penalty, maximum penalty, reliability score, and trip count for each route.

###time_period_comparison Function

Housed in: Lateness Functions.R
Inputs: otp, routes, morning_hours, afternoon_hours

This function compares route performance during morning and afternoon school commute hours. It categorizes each trip into either the morning or afternoon period based on arrival time, flags whether the trip was more than 5 minutes late, and calculates lateness proportions and delay metrics for each period. It then reshapes these results side-by-side and computes the difference between morning and afternoon performance, enabling users to see whether a route is more reliable before or after school.

Output: Dataframe comparing lateness rates and delay characteristics for each route across morning and afternoon periods, including performance differences.


### Route_allocation Function
Housed in the file: New_Start_Time

Function: Route_allocation

Inputs: otp, ridership, new_school_morning, new_school_afternoon, old_school_morning, old_school_afternoon, stops, cutoff

1. Identifies popular stops used by Providence high school students during current school commute hours (morning and afternoon), filtering out rarely used stops based on the cutoff.
2. Merges stop information with OTP data to calculate, for each stop–route–hour combination, the proportion of trips arriving more than 5 minutes late.
3. Computes reliability scores for all routes during the proposed new morning and afternoon school hours. CALLING THE reliability_score() function
4. Selects recommended routes for each popular stop using two criteria: The route with the lowest >5 min lateness proportion (best punctuality), The route with the highest reliability score 
5. Returns four data frames: Morning and afternoon route recommendations based on proportion over 5 minutes late, Morning and afternoon route recommendations based on reliability

Outputs: list(recommended_routes_morning, recommended_routes_afternoon, morning_reliability_based, afternoon_reliability_based)


## How to Run

install.packages(c("tidyverse", "ggplot2", "scales", "lubridate", "gt"))
source("Main Script.R") - Results save to `Results/` folder. 

## Methods

**School Route Identification:** Compares ridership during school hours (6-7am, 2-3pm) vs other times. Routes with ratio >1.3 = school routes.

**OTP Analysis:** Proportion of trips >5 min late during school vs non-school hours.

**Lateness Metrics:** (1) Multiple thresholds (>5, >10, >15 min), percentiles, mean/median delays (2) Reliability score (0-100, penalty-based: 0-5min=0pts, 5-10min=1pt, 10-15min=3pts, 15-20min=5pts, >20min=8pts) (3) Morning vs afternoon comparison.

**Route Recommendations:** Two approaches for popular stops (>10 students/day): minimize proportion late OR maximize reliability score. Provides morning/afternoon recommendations for new school schedules.

## Results
To view results, click the results folder 

**11 CSV files:** 
Recommended routes based on reliability scores (as determined by our function for reliability): routes morning reliable, routes afternoon reliable, result morning, result afternoon, school routes, otp summary, morning vs afternoon, reliability scores, lateness metrics, HS ridership result

To view the plots, click the plots folder

**1 PDF:** 7 plots showing ridership, popular stops, OTP comparisons, lateness analysis, metric comparison

## Key Feature

Dual optimization compares two recommendation strategies: minimizing late frequency vs minimizing delay severity. Shows whether metric choice affects recommendations.

**Adjust parameters in Main Script.R:** `old_school_morning`, `old_school_afternoon`, `new_school_morning`, `new_school_afternoon`, `cutoff`
