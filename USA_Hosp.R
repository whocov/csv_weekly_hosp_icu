#######################################################
# This script import data file from USA through API connection and do data clean
# Author: Joao Muianga - muiangaj@who.int
# Date updated: 20 OCT 2023
##########################################################################

# Downloads and installs packages (if they are not already installed)
#####################################################################
pacman::p_load(
  here,         # file paths relative to R project root folder
  rio,          # import/export of many types of data
  tidyverse,    # includes many packages for tidy data wrangling and presentation
  skimr,        # data exploration
  epikit,       # useful epi functions
  lubridate,    # dates and times
  stringr,      # handling characters
  janitor       # tables and data cleaning
             )  


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
         date = as.Date(date))


################ USA file 2
usa_data_2_rows <- USA_file_1%>%
  # remove states
  filter(state != "PR" & state != "VI"& state != "MP" & state != "PW" & state != "FM" & state != "MH" & state != "GU" & state !="AS")%>%
  select(date, previous_day_admission_adult_covid_confirmed, previous_day_admission_pediatric_covid_confirmed)%>%
  replace(is.na(.), 0) %>%
  # calculate new hospitalization column
  mutate(previous_day_admission_adult_covid_confirmed     = as.numeric(previous_day_admission_adult_covid_confirmed),
         previous_day_admission_pediatric_covid_confirmed = as.numeric(previous_day_admission_pediatric_covid_confirmed),
         new_hospitalization = previous_day_admission_adult_covid_confirmed + previous_day_admission_pediatric_covid_confirmed,
         date = as.Date(date))

#### Join tables
usa_data_join <- full_join(usa_data, usa_data_2_rows, by=c("date", "new_hospitalization"))

## summarize by date
usa_data_full<-usa_data_join%>%filter(new_hospitalization != 0)%>%
  select( date, new_hospitalization)%>%
  mutate( country_name        = "United States of America")%>%
  group_by(country_name, date)%>%
  summarise(new_hospitalization = sum(new_hospitalization, na.rm = T))%>%rename(report_date = date)


###################################################################
## clean historical data
################################################################
## remove  duplication
historical_data <- usa_data_full%>%
  group_by(report_date, country_name) %>%       # group the rows by 'name'
  slice_max(new_hospitalization,         # keep row per group with maximum date value 
            n = 1,         # keep only the single highest row 
            with_ties = F) # if there's a tie (of date), take the first row

# Create week number column and summarise
historical_data <- historical_data%>%
  mutate(
    epiweek = floor_date(
      report_date,
      unit = "week",
      week_start = 1
    ),
    iso_year = year(report_date))%>%
  # sum column base on value number
  group_by(country_name, epiweek)%>%
  summarise(new_hospitalization = if(all(is.na(new_hospitalization))) NA else sum(new_hospitalization, na.rm = T),
            new_icu             = if(all(is.na(new_icu))) NA else sum(new_icu, na.rm = T))


######################### count time 
historical_data <- historical_data%>%
  group_by(country_name, epiweek)%>%
  mutate(freq_time = n())

########################################################
historical_data<- historical_data%>%
  mutate(
    freq = if_else(
      condition = freq_time == 1, 
      true      = "weekly", 
      false     = "daily"
    ),
    iso_year    = year(epiweek),
    iso_week_number = week(epiweek))

historical_data <- historical_data%>%
  select(country_name, iso_year, iso_week_number, epiweek, freq, freq_time, new_hospitalization, new_icu)%>%
  rename(country = country_name)


###################Change country name values
historical_data <- historical_data%>%
  mutate(country = recode(
    country,
    "United States of America" = "UNITED STATES OF AMERICA"))

############################################
## Import ref_countr data
ref_places <- import(here("data", "ref_country", "data_export_NCOV_REF_PLACES.csv"))
ref_places <- ref_places%>%distinct()%>%rename(country = ADM0_NAME)

###########################################
### Join ref country and historical data
historical_dataset <- left_join(historical_data_full, ref_places, by= ("country"))

last_week = max(historical_dataset$epiweek)

# remove last week
historical_dataset<- historical_dataset%>%filter(epiweek < last_week)

###########################################################################################
#### Export data to Excel

export(historical_dataset, here("data", "clean","historical_clean_data.csv"))

############################################################################################





