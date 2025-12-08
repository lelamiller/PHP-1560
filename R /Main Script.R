#Main Script
library(tidyverse)
library(ggplot2)
library(scales)
library(lubridate)

results_path <- "/Users/nikhilsonthalia/Downloads/PHP-1560/Results"
if (!dir.exists(results_path)) {
  cat("Creating Results folder...\n")
  dir.create(results_path, recursive = TRUE)
}

list.files("/Users/nikhilsonthalia/Downloads/PHP-1560")
source("/Users/nikhilsonthalia/Downloads/PHP-1560/R /HS_Routes.R")
source("/Users/nikhilsonthalia/Downloads/PHP-1560/R /otp_hs_routes.R")
source("/Users/nikhilsonthalia/Downloads/PHP-1560/R /Lateness Functions.R")
source("/Users/nikhilsonthalia/Downloads/PHP-1560/R /Plots.R")
source("/Users/nikhilsonthalia/Downloads/PHP-1560/R /New_Start_Time.R")
cat("All scripts loaded!\n\n")

# Load data
otp <- read.csv("/Users/nikhilsonthalia/Downloads/PHP-1560/Data/otp_simulated.csv")
ridership <- read.csv("/Users/nikhilsonthalia/Downloads/PHP-1560/Data/ridership_simulated.csv")
stops <- read.delim("/Users/nikhilsonthalia/Downloads/PHP-1560/Data/stops.txt", sep = ",")

# Set parameters
old_school_morning <- c(6, 7)
old_school_afternoon <- c(14, 15)
old_school_hours <- c(old_school_morning, old_school_afternoon)

new_school_morning <- c(7, 8)
new_school_afternoon <- c(15, 16)
new_school_hours <- c(new_school_morning, new_school_afternoon)

cutoff <- 10

# Run analyses
school_routes <- HS_Routes(ridership, otp, old_school_hours)
cat("Found", length(school_routes), "school routes\n\n")
otp_summary <- otp_school_routes(otp, ridership, old_school_hours, old_school_hours)
lateness_metrics <- analyze_lateness_metrics(otp, school_routes, old_school_hours)
cat("Lateness metrics calculated:", nrow(lateness_metrics), "routes\n")

reliability_scores <- reliability_score(otp, school_routes, old_school_hours)
cat("Reliability scores calculated:", nrow(reliability_scores), "routes\n")

time_comparison <- time_period_comparison(otp, school_routes, 
                                          old_school_morning, old_school_afternoon)
cat("Time comparison calculated:", nrow(time_comparison), "routes\n\n")
result <- Route_allocation(otp, ridership, 
                            new_school_morning, new_school_afternoon,
                            old_school_morning, old_school_afternoon,
                            stops, cutoff)
routes_morning <- result[[1]]
routes_afternoon <- result[[2]]
routes_morning_reliable <- result[[3]]
routes_afternoon_reliable <- result[[4]]

#Saving Results
write.csv(lateness_metrics, "/Users/nikhilsonthalia/Downloads/PHP-1560/Results/lateness_metrics.csv", row.names = FALSE)
write.csv(reliability_scores, "/Users/nikhilsonthalia/Downloads/PHP-1560/Results/reliability_scores.csv", row.names = FALSE)
write.csv(time_comparison, "/Users/nikhilsonthalia/Downloads/PHP-1560/Results/morning_vs_afternoon.csv", row.names = FALSE)
write.csv(otp_summary, "/Users/nikhilsonthalia/Downloads/PHP-1560/Results/otp_summary.csv", row.names = FALSE)
school_routes_df <- data.frame(Route = school_routes)
write.csv(school_routes_df, "/Users/nikhilsonthalia/Downloads/PHP-1560/Results/school_routes.csv", row.names = FALSE)
write.csv(routes_morning, "/Users/lelamiller/Documents/GitHub/PHP-1560/Results/result_morning.csv", row.names = FALSE)
write.csv(routes_afternoon, "/Users/lelamiller/Documents/GitHub/PHP-1560/Results/result_afternoon.csv", row.names = FALSE)
write.csv(routes_morning_reliable, "/Users/lelamiller/Documents/GitHub/PHP-1560/Results/routes_morning_reliable.csv", row.names = FALSE)
write.csv(routes_afternoon_reliable, "/Users/lelamiller/Documents/GitHub/PHP-1560/Results/routes_afternoon_reliable.csv", row.names = FALSE)


#Maing Plots
tryCatch({
  generate_all_plots(
    ridership = ridership,
    otp = otp,
    school_routes = school_routes,
    old_school_morning = old_school_morning,
    old_school_afternoon = old_school_afternoon,
    otp_summary = otp_summary,
    lateness_metrics = lateness_metrics,
    reliability_scores = reliability_scores,
    time_comparison = time_comparison,
    output_file = file.path(results_path, "PPSD_Bus_Analysis_Plots.pdf")
  )
  cat("✓ Plots saved successfully!\n")
}, error = function(e) {
  cat("✗ ERROR generating plots:", e$message, "\n")
})


# List what was saved
cat("\nFiles in Results folder:\n")
saved_files <- list.files(results_path)
if (length(saved_files) > 0) {
  for (f in saved_files) {
    cat("  -", f, "\n")
  }
} else {
  cat("  (folder is empty - something went wrong)\n")
}