# Project: Air Quality Data Cleaning and Analysis

**Description:**
This R project cleans raw air quality data and generates a summary report with aggregated statistics and visualizations.

**Features:**
- Generating sample dataset of air quality parameters for different cities
- Data cleaning (handling missing values, outliers, and standardizing units)
- Aggregation by region, time, or pollutant type
- Summary statistics and visualizations

**Dependencies:**
- Packages: `ggplot2`, `readr`, `tidyverse`, `lubridate`, `fuzzyjoin`, `stringdist`

**How to Run:**
1. Clone the repository.
2. Generate sample dataset in `report_generator.R.
3. Run `report_generator.R` to clean the data and create output file for report
5. Knit `air_analysis_report.Rmd` to produce the report.

**Output:**
- Cleaned data: `data_processed/clean_air_quality.csv`
- Report: `air_analysis_report.docx`
