#######################################################
# This script import data file from USA through API connection and do data clean
# Author: Joao Muianga - muiangaj@who.int
# Date updated: 17 NOV 2023
##########################################################################

# Downloads and installs packages (if they are not already installed)
#####################################################################
pacman::p_load(

  here,         # file paths relative to R project root folder
  rio,          # import/export of many types of data
  openxlsx,     # special functions for handling Excel workbooks
  remotes,      # install from github
  tidyverse,    # includes many packages for tidy data wrangling and presentation
  skimr,        # data exploration
  epikit,       # useful epi functions
  lubridate,    # dates and times
  stringr,      # handling characters
  base,
  janitor,     # tables and data cleaning
  corrr,
  caret,
  broom,
  httr,
  curl,
  jsonlite)        


#############################################################################################
### Import file of countries who have API GET json or Github

########## United States of America
path <- "https://healthdata.gov/resource/g62h-syeh.json?$limit=50000"  # prepare request
request <- GET(url = path)
request$status_code  # check for any server error ("200" is good!)

# submit the request, parse the response, and convert to a data frame
USA_api <- base::rawToChar(request$content)
USA_file <- jsonlite::fromJSON(USA_api, flatten = TRUE)

#### next rows for USA
path_1 <- "https://healthdata.gov/resource/g62h-syeh.json?$limit=50000&$offset=50000"  # prepare request
request_1 <- GET(url = path_1)
request_1$status_code  # check for any server error ("200" is good!)

# submit the request, parse the response, and convert to a data frame
USA_api_1 <- base::rawToChar(request_1$content)
USA_file_1 <- jsonlite::fromJSON(USA_api_1, flatten = TRUE)


########## USA have 2 API with max of 50000000 row per API, to complete join both for one file
################ USA file 1
usa_data <- USA_file%>%
  # remove states
  filter(state != "PR" & state != "VI"& state != "MP" & state != "PW" & state != "FM" & state != "MH" & state != "GU" & state !="AS" )%>%
  replace(is.na(.), 0) %>%
  dplyr::select(state, date, previous_day_admission_adult_covid_confirmed, previous_day_admission_pediatric_covid_confirmed)%>%
  # calculate new hospitalization column
  mutate(previous_day_admission_adult_covid_confirmed     = as.numeric(previous_day_admission_adult_covid_confirmed),
         previous_day_admission_pediatric_covid_confirmed = as.numeric(previous_day_admission_pediatric_covid_confirmed),
         new_hospitalization = previous_day_admission_adult_covid_confirmed + previous_day_admission_pediatric_covid_confirmed,
         date = as.Date(date))%>%
  select(state, date,new_hospitalization)


################ USA file 2
usa_data_2_rows <- USA_file_1%>%
  # remove states
  filter(state != "PR" & state != "VI"& state != "MP" & state != "PW" & state != "FM" & state != "MH" & state != "GU" & state !="AS")%>%
  select(state, date, previous_day_admission_adult_covid_confirmed, previous_day_admission_pediatric_covid_confirmed)%>%
  replace(is.na(.), 0) %>%
  # calculate new hospitalization column
  mutate(previous_day_admission_adult_covid_confirmed     = as.numeric(previous_day_admission_adult_covid_confirmed),
         previous_day_admission_pediatric_covid_confirmed = as.numeric(previous_day_admission_pediatric_covid_confirmed),
         new_hospitalization = previous_day_admission_adult_covid_confirmed + previous_day_admission_pediatric_covid_confirmed,
         date = as.Date(date))%>%
  select(state, date,new_hospitalization)

#### Join tables
usa_data_join <- full_join(usa_data, usa_data_2_rows, by=c("date", "new_hospitalization", "state"))
## summarize by date
usa_data_full<-usa_data_join%>%filter(new_hospitalization != 0)%>%
  mutate( country  = "UNITED STATES OF AMERICA",
          WHO_REGION = "AMRO",
          ISO_3_CODE = "USA",
          date = as.Date(date))%>%
  group_by(WHO_REGION,ISO_3_CODE, country, date)%>%
  summarise(new_hospitalization = sum(new_hospitalization, na.rm = T))

# Import NZ data and clean
new_zealand_file_1<-read.csv("https://raw.githubusercontent.com/minhealthnz/nz-covid-data/main/cases/weekly-hospitalisations-for-covid.csv")

# Clean and align data 
new_zealand_data <- new_zealand_file_1%>%
  clean_names()%>%
  filter(variable_label == "Sex")%>%
  select(admissions_for_covid_19_in_the_week_ending, hospitalisations, icu)%>%
  rename(date            =  admissions_for_covid_19_in_the_week_ending,             # change column name
         new_hospitalization    =  hospitalisations,
         new_icu                =  icu )%>%
  mutate(country = "NEW ZEALAND",
         WHO_REGION = "WPRO",
         ISO_3_CODE = "NZL",
         date  = as.Date(date))%>%
  group_by(WHO_REGION, ISO_3_CODE, country, date)%>%
  summarise(new_hospitalization = sum(new_hospitalization, na.rm = T), new_icu = sum(new_icu, na.rm = T))


# join USA and NZL data
USA_NZL_data <- full_join(new_zealand_data, usa_data_full, by=c("country", "WHO_REGION", "ISO_3_CODE", "date","new_hospitalization"))

## remove  duplication
USA_NZL_data <- USA_NZL_data%>%
  group_by(date, country) %>%       # group the rows by 'name'
  slice_max(new_hospitalization,        # keep row per group with maximum date value 
            n = 1,         # keep only the single highest row 
            with_ties = F) # if there's a tie (of date), take the first row

# Create week number column and summarise
USA_NZL_data <- USA_NZL_data%>%
  mutate(
    epiweek = floor_date(
      date,
      unit = "week",
      week_start = 1))%>%
  # sum column base on value number
  group_by(country, epiweek)%>%
  summarise(new_hospitalization = if(all(is.na(new_hospitalization))) NA else sum(new_hospitalization, na.rm = T),
            new_icu             = if(all(is.na(new_icu))) NA else sum(new_icu, na.rm = T))

########################################################
USA_NZL_data<- USA_NZL_data%>%
  mutate(iso_year    = year(epiweek),         #create year column
         iso_week_number = week(epiweek))     #create week number columns

# Export dataset
export(USA_NZL_data, here("USA_NZL_Hosp.csv"))

