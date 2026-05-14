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
library(fuzzyjoin)
library(stringdist)
library(ggplot2)

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
# Check for NAs in columns
colSums(is.na(env_data))

# ---- CLEANING OF DATA ----
## Replace values in humidity above 100 with median
## Correct location names
## Replace values in temp_c

current_date <- format(Sys.Date(), format = "%Y-%m-%d")

cleaning_data <- function(data){
  data <- data %>%
    ## Remove entries with no date
    filter(!is.na(date)) %>%
    ## Change no2/wind_speed to num
    mutate(across(where(is.character), ~ {
      num_col <- suppressWarnings(as.numeric(.x))
      numeric_vals <- num_col[!is.na(num_col)]
      if (length(numeric_vals) > 0){
        mean_val <- mean(numeric_vals, na.rm = T)
        ifelse(is.na(num_col), mean_val, num_col)
      } else{
        .x
      }
    })) %>%
    mutate(
      median_humidity = median(data$humidity[data$humidity <= 100], na.rm = T),
      humidity = case_when(
        humidity > 100 ~ median_humidity,
        is.na(humidity) ~ median_humidity,
        TRUE ~ humidity
        )
      ) %>%
    select(-median_humidity) %>%
    mutate(
      median_temp_c = median(data$temp_c[data$temp_c <= 45], na.rm = T),
      temp_c = case_when(
        is.na(temp_c) ~ median_temp_c,
        temp_c > 45 ~ median_temp_c,
        TRUE ~ temp_c
      )
    ) %>%
    select(-median_temp_c) %>%
    mutate(pm10 = ifelse(is.na(pm10), median(pm10, na.rm = T), pm10),
           pm25 = ifelse(is.na(pm25), median(pm25, na.rm = T), pm25)
           ) 

}

correction_location_spelling <- function(data, location_col = "location"){
  valid_locations <- tibble(location = c("Sydney", "Melbourne", "Brisbane", "Perth", "Adelaide", "Darwin"))
  fuzzy_match <- stringdist_join(
    data, valid_locations,
    by = location_col,
    method = "jw",
    max_dist = 0.2,
    distance_col = "dist"
  ) 
  fuzzy_match %>%
    mutate(location = coalesce(location.y, location.x)) %>%
    select(-location.y,
           -location.x,
           -dist)
}
# Master function
clean_all_data <- function(data) {
  # List of functions and their arguments (excluding the data argument)
  cleaning_steps <- list(
    list(func = cleaning_data, args = list()),
    list(func = correction_location_spelling, args = list(location_col = "location"))
  )
  
  # Apply each function in sequence
  for (step in cleaning_steps) {
    data <- do.call(step$func, c(list(data), step$args))
  }
  
  return(data)
}

clean_data <- clean_all_data(env_data)

dir.create("data_processed", showWarnings = FALSE)  
saveRDS(clean_data, file.path("data_processed", paste0("clean_data", current_date, ".rds")))

# ---- VISUALIZATION ----
## ---- Aggregating data ----
agg_location <- clean_data %>%
  group_by(location, date) %>%
  summarise(mean_temp = mean(temp_c),
            mean_pm25 = mean(pm25),
            mean_pm10 = mean(pm10),
            mean_no2 = mean(no2),
            mean_humidity = mean(humidity))

agg_location_total <- clean_data %>%
  group_by(location) %>%
  summarise(mean_temp = mean(temp_c),
            mean_pm25 = mean(pm25),
            mean_pm10 = mean(pm10),
            mean_no2 = mean(no2),
            mean_humidity = mean(humidity))

agg_station <- clean_data %>%
  group_by(station_id, date) %>%
  summarise(mean_temp = mean(temp_c),
            mean_pm25 = mean(pm25),
            mean_pm10 = mean(pm10),
            mean_no2 = mean(no2),
            mean_humidity = mean(humidity))

agg_date <- clean_data %>%
  group_by(date) %>%
  summarise(mean_temp = mean(temp_c),
            mean_pm25 = mean(pm25),
            mean_pm10 = mean(pm10),
            mean_no2 = mean(no2),
            mean_humidity = mean(humidity))

## ---- Creating long format for facting
agg_location_wide <- agg_location %>%
  pivot_longer(cols = -c(location, date),
               names_to = "variable",
               values_to =  "value")

agg_station_wide <- agg_station %>%
  pivot_longer(cols = -c(station_id, date),
               names_to = "pollutant",
               values_to =  "value")

agg_date_wide <- agg_date %>%
  pivot_longer(cols = -date,
               names_to = "pollutant",
               values_to =  "value")

## ---- Line graph by location
plot_all_pollutants_temp_by_loc <- ggplot(agg_date_wide, aes(x = date, y = value, group = pollutant, color = pollutant)) +
  geom_line() +
  facet_wrap(~ pollutant, scales = "free_y") +
  labs(title = "Pollutants and Temperature Over Time by Location",
       x = "Date",
       y = "Value",
       color = "Pollutants") +
  theme_minimal()

plot_temp <- ggplot(agg_location, aes(x = date, y = mean_temp, group = location, color = location)) +
  geom_smooth() +
  labs(title = "Temperature Over Time by Location",
       x = "Date",
       y = "Temperature in C°",
       color = "Location") +
  theme_minimal()
plot_temp 


plot_all_pollutants_temp_by_loc