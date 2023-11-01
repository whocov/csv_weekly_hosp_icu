#######################################################
# This script import data file from USA through API connection and do data clean
# Author: Joao Muianga - muiangaj@who.int
# Date updated: 20 OCT 2023
##########################################################################

# Downloads and installs packages (if they are not already installed)
#####################################################################
pacman::p_load(

  learnr,   # interactive tutorials
  here,     # file paths relative to R project root folder
  rio,      # import/export of many types of data
  openxlsx, # special functions for handling Excel workbooks
  remotes,  # install from github
  tidyverse,    # includes many packages for tidy data wrangling and presentation
  skimr,        # data exploration
  epikit,       # useful epi functions
  lubridate,    # dates and times
  stringr,      # handling characters
  base,
  janitor,      # tables and data cleaning
  gtsummary,    # making descriptive and statistical tables
  corrr,
  caret,
  broom,
  httr,
  jsonlite,
  cowplot,          # combining plots  
  RColorBrewer,     # color scales
  gghighlight,      # highlight a subset
  ggrepel,          # smart labels
  ggExtra,          # fancy plots  
  tsibble,          # epiweeks
  viridis,          # colorblind-friendly scales
  scales,           # helper functions
  apyramid,         # age and sex pyramids
  rmarkdown,        # produce PDFs, Word Documents, Powerpoints, and HTML files
  knitr,            # R Markdown report generation and html tables
  flextable)        # HTML tables


# Install phi function to get all XMART data 
devtools::install_github("whocov/phifunc", auth_token = "d9b68eb06316f7e322c660103b187cbbcf024f72", subdir = "phifunc", dep = TRUE, force = TRUE)
# pull all data

require(phifunc)
require(tidyverse)

# get daily cases/deaths

phi_data <- pull_phi_data()

# get all datasets
# imports cases/death data and testing data by default - this includes phi data 

all_data <- read_all_data(
  use_ext_phi_data = FALSE, # using cases and deaths data from external dashboard
  stringency = TRUE, # import stringency data
  vaccine = TRUE, # import vaccine data (OWID ONLY)
  mobility = TRUE, # import mobility data (apple and google)
  age_sex = TRUE,  # import age/sex data
  phsm = TRUE, # import public health and social measures index
  epinow = TRUE  # import epinow data (effective R number)
)

# reference table with population data
pop_data <- pull_pop_data()

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


##### Import NZ data
new_zealand_file_1<-read.csv("https://raw.githubusercontent.com/minhealthnz/nz-covid-data/main/cases/weekly-hospitalisations-for-covid.csv")

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

####################### Clean data NZ
nz_data <- new_zealand_file_1%>%
  filter(Variable.Label == "Sex")%>%
  select(Admissions.for.COVID.19.in.the.week.ending, Hospitalisations, ICU)%>%
  rename(report_date            =  Admissions.for.COVID.19.in.the.week.ending,             # change column name
         new_hospitalization    =  Hospitalisations,
         new_icu                =  ICU )%>%
  mutate(country_name = "New Zealand",
         report_date = as.Date(report_date))%>%
  group_by(country_name, report_date)%>%
  summarise(new_hospitalization = sum(new_hospitalization, na.rm = T), new_icu = sum(new_icu, na.rm = T))

###################################################################
## Join USA and NZ dataset in one file
historical_data <- full_join(usa_data_full, nz_data, by=c("report_date", "country_name", "new_hospitalization"))

#######################################
## clean historical data
################################################################
## remove  duplication
historical_data <- historical_data%>%
  group_by(report_date, country_name) %>%       # group the rows by 'name'
  slice_max(new_hospitalization,        # keep row per group with maximum date value 
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
            new_icu = if(all(is.na(new_icu))) NA else sum(new_icu, na.rm = T))

########################################################
historical_data<- historical_data%>%
  mutate(
    iso_year    = year(epiweek),         #create year column
    iso_week_number = week(epiweek))     #create week number columns

historical_data <- historical_data%>%
  select(country_name, iso_year, iso_week_number, epiweek, new_hospitalization, new_icu)%>%
  rename(country = country_name)%>%
  mutate(country = recode( country,
    "United States of America" = "UNITED STATES OF AMERICA",
    "New Zealand"              = "NEW ZEALAND"))


###########################################
### Join ref country and historical data
historical_data <- left_join(historical_data, ref_places, by= ("country"))

##### calculate last week and incompleted 
last_week = max(historical_data$epiweek)

# remove last week
historical_data<- historical_data%>%filter(epiweek < last_week)
#### Export data to Excel

export(historical_data, here("USA_NZ_Hosp.csv"))



