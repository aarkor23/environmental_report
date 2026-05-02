# ---- INSTRUCTIONS ----
# **Project:** Automate a monthly report for your current job or a public dataset (e.g., sales, expenses, or survey data).
# 
# **Steps:**
#   
#   1. **Data Prep:** Write an R script to clean and aggregate data.
# 2. **Visualization:** Create visualizations with `ggplot2`.
# 3. **Reporting:** Generate a PDF/HTML report using R Markdown.
# 4. **Automation:** Schedule the report to run and email results (using `cron` or `taskscheduleR`).**Deliverables:**
#   - GitHub repo with:
#   - R script (`report_generator.R`)
# - R Markdown report (`report.Rmd`)
# - Sample output (PDF/HTML)
# - README with instructions

# --- LIBRARIES ----
library(tidyverse)
library(readr)
library(lubridate)

# ---- CREATING SAMPLE DATASET ----
set.seed(123)
n <- 100

# Station IDs (character, no issue)
station_id <- sample(c("AQ001", "AQ002", "AQ003", "AQ004", "AQ005", "AQ006"), n, replace = TRUE)

# Dates: intentionally mixed Date and character strings
date <- sample(c(
  seq(as.Date("2026-01-01"), as.Date("2026-03-31"), by = "day"),
  paste0(sample(1:31, 20, replace = TRUE), "/", sample(1:12, 20, replace = TRUE), "/2026")
), n, replace = TRUE)

# PM2.5: numeric, with NAs and negative outliers
pm25 <- round(runif(n, 0, 20), 1)
pm25[sample(1:n, 5)] <- NA
pm25[sample(1:n, 3)] <- runif(3, -5, 0) # negative outliers

# PM10: numeric, with NAs
pm10 <- round(runif(n, 10, 40), 1)
pm10[sample(1:n, 10)] <- NA

# NO2: intentionally mixed numeric, NA, and character
no2 <- as.character(sample(c(sample(5:25, n-12, replace = TRUE), rep(NA, 10), "12", "high"), n, replace = TRUE))

# Temperature: numeric, with NAs and Fahrenheit outliers
temp_c <- round(runif(n, 15, 30), 1)
temp_c[sample(1:n, 5)] <- round(runif(5, 50, 90), 1) # Fahrenheit values
temp_c[sample(1:n, 5)] <- NA

# Humidity: numeric, with NAs and >100 outliers
humidity <- sample(40:90, n, replace = TRUE)
humidity[sample(1:n, 5)] <- sample(100:120, 5) # >100 outliers
humidity[sample(1:n, 5)] <- NA

# Wind speed: intentionally mixed numeric, NA, and character
wind_speed <- as.character(sample(c(sample(5:20, n-12, replace = TRUE), rep(NA, 5), "high", "low", "medium"), n, replace = TRUE))

# Location: character, with typos
location <- sample(c("Sydney", "Melbourne", "Brisbane", "Perth", "Adelaide", "Darwin"), n, replace = TRUE)
location[sample(1:n, 5)] <- c("Melbourn", "Brisban", "Perthh", "Adelaidee", "Sydneey")

# Combine into a data frame
env_data <- data.frame(
  station_id,
  date,
  pm25,
  pm10,
  no2,
  temp_c,
  humidity,
  wind_speed,
  location,
  stringsAsFactors = FALSE
)

# View the first 10 rows
head(env_data, 10)