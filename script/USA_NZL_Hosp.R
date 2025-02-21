#######################################################
# This script imports CDC and NZL data, cleans, merges, and exports the data
# Author: Craig Schultz
# Date updated: 2025-02-21
##########################################################################

# Load necessary libraries
pacman::p_load(
  here,         
  rio,          
  openxlsx,     
  remotes,      
  tidyverse,    
  skimr,        
  epikit,       
  lubridate,    
  stringr,      
  base,
  janitor,     
  corrr,
  caret,
  broom,
  httr,
  curl,
  jsonlite)    

#############################################################################################
### Import file of countries who have API GET json or Github

# Import NZ data and clean
new_zealand_file_1 <- read.csv("https://raw.githubusercontent.com/minhealthnz/nz-covid-data/main/cases/weekly-hospitalisations-for-covid.csv")

new_zealand_data <- new_zealand_file_1 %>%
  clean_names() %>%
  filter(variable_label == "Sex") %>%
  select(admissions_for_covid_19_in_the_week_ending, hospitalisations, icu) %>%
  rename(date = admissions_for_covid_19_in_the_week_ending,
         new_hospitalization = hospitalisations,
         new_icu = icu) %>%
  mutate(country = "NEW ZEALAND",
         WHO_REGION = "WPRO",
         ISO_3_CODE = "NZL",
         date = as.Date(date)) %>%
  group_by(WHO_REGION, ISO_3_CODE, country, date) %>%
  summarise(new_hospitalization = sum(new_hospitalization, na.rm = TRUE),
            new_icu = sum(new_icu, na.rm = TRUE)) %>%
  mutate(
    epiweek = floor_date(date, unit = "week", week_start = 1),
    iso_year = year(epiweek),
    iso_week_number = isoweek(epiweek)
  ) %>%
  select(WHO_REGION, ISO_3_CODE, country, epiweek, new_hospitalization, new_icu, iso_year, iso_week_number)

# CDC Data Integration
cdc_url <- "https://data.cdc.gov/resource/bigw-pgk2.json?$limit=50000"
cdc_request <- GET(url = cdc_url)
cdc_api <- base::rawToChar(cdc_request$content)
cdc_data <- jsonlite::fromJSON(cdc_api, flatten = TRUE)

if (is.list(cdc_data)) {
  cdc_data <- as.data.frame(cdc_data)
}

# CDC Data Processing
cdc_filtered_data <- cdc_data %>%
  clean_names() %>%
  mutate(
    season = as.integer(season),
    time_period = str_trim(str_to_lower(time_period)),
    estimate = as.integer(estimate)
  ) %>%
  filter(time_period == "week", season %in% c(2324, 2425), strata == "Overall") %>%
  mutate(time_parsed = lubridate::mdy(time)) %>%
  filter(!is.na(time_parsed), time_parsed >= as.Date("2024-05-04")) %>%
  mutate(
    epiweek = floor_date(time_parsed, unit = "week", week_start = 1),
    WHO_REGION = "AMRO",
    ISO_3_CODE = "USA",
    country = "UNITED STATES OF AMERICA",
    new_hospitalization = estimate,
    new_icu = NA,
    iso_year = year(epiweek),
    iso_week_number = isoweek(epiweek)
  ) %>%
  select(WHO_REGION, ISO_3_CODE, country, epiweek, new_hospitalization, new_icu, iso_year, iso_week_number)

# Merge CDC and NZL Data
CDC_NZL_merged_data <- bind_rows(new_zealand_data, cdc_filtered_data)

# Remove duplication and finalize
CDC_NZL_merged_data <- CDC_NZL_merged_data %>%
  group_by(epiweek, country) %>%
  slice_max(new_hospitalization, n = 1, with_ties = FALSE) %>%
  ungroup()

# Export final dataset
export(CDC_NZL_merged_data, here("data", "clean", "historical_clean_data.csv"))

# Completion message
print("CDC and NZL data imported, cleaned, and merged successfully into historical_clean_data.csv")
