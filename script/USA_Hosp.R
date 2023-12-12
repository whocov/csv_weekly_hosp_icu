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
  filter(Variable.Label == "Sex")%>%
  select(Admissions.for.COVID.19.in.the.week.ending, Hospitalisations, ICU)%>%
  rename(date            =  Admissions.for.COVID.19.in.the.week.ending,             # change column name
         new_hospitalization    =  Hospitalisations,
         new_icu                =  ICU )%>%
  mutate(country = "NEW ZEALAND",
         WHO_REGION = "WPRO",
         ISO_3_CODE = "NZL",
         date  = as.Date(date, format = "%d/%m/%Y"))%>%
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
export(USA_NZL_data, here("USA_Hosp.csv"))



msc <-import(here("data", "raw", "data_export_NCOV_A_COMBINED_WEEK_SEX_AGEGROUP_MIXEDSOURCES 3.csv")) 

msc<- msc%>%select(WHO_REGION, COUNTRY_NAME, COUNTRY_CODE, ISO_START_DATE, DETAILED_CASES, DETAILED_CASES_DEATHS, SEX, AGEGROUP, ISO_WEEK, ISO_YEAR)

linelist <- msc%>%mutate(epiweek = as.Date(ISO_START_DATE))

# filter last 28 days
day28 <- max(linelist$epiweek)-28
linelist_28 <- linelist%>%filter(epiweek > day28)%>%
  select(WHO_REGION, COUNTRY_NAME, DETAILED_CASES)

### Cases
linelist_cases<- linelist_28%>%filter(!is.na(DETAILED_CASES))
linelist_cases<-linelist_cases%>%distinct(WHO_REGION, COUNTRY_NAME)
total_cases <- linelist_cases%>%count(WHO_REGION)%>%rename(country_cases= n)

### deaths

linelist_deaths <- linelist%>%filter(epiweek > day28)%>%
  select(WHO_REGION, COUNTRY_NAME,DETAILED_CASES_DEATHS )

linelist_deaths<- linelist_deaths%>%filter(!is.na(DETAILED_CASES_DEATHS))
linelist_deaths<-linelist_deaths%>%distinct(WHO_REGION, COUNTRY_NAME)
total_deaths <- linelist_deaths%>%count(WHO_REGION)%>%rename(country_deaths= n)

ref_country <- import(here("data", "raw", "data_export_NCOV_REF_COUNTRY_POP.csv"))
total<-ref_country%>%filter(WHO_REGION != "WB" & WHO_REGION != "GLOBAL")%>%count(WHO_REGION)%>%rename(countries= n)

####### 

join_deaths_cases <- full_join(total_cases, total_deaths, by="WHO_REGION")
join_deaths_cases <- join_deaths_cases%>%mutate(WHO_REGION = recode(WHO_REGION, 
                                                                    "AFR" = "AFRO",
                                                                    "AMR" = "AMRO",
                                                                    "SEAR" = "SEARO",
                                                                    "WPR" = "WPRO",
                                                                    "EUR" = "EURO"
                                                                    ))

cases_deahts <- full_join(join_deaths_cases, total, by="WHO_REGION")


cases_deahts <- cases_deahts%>%
  group_by(WHO_REGION)%>%
  mutate(percent_cases  = scales::percent(country_cases / countries),
         percent_deaths = scales::percent(country_deaths / countries))


cases_deahts <- cases_deahts%>%select(WHO_REGION, countries, country_cases, percent_cases, country_deaths, percent_deaths)

library(officer)
library(flextable)

cases_deahts <- flextable::flextable(cases_deahts)

cases_deahts <- cases_deahts %>% 
  add_header_row(
    top = TRUE,                # New header goes on top of existing header row
    values = c("WHO Region",     # Header values for each column below
               "Total of countries", 
               "Cases",    # This will be the top-level header for this and two next columns
               "",
               "Deaths",
               "")) %>% 
  
  set_header_labels(         # Rename the columns in original header row
    WHO_REGION = "", 
    countries = "",                  
    country_cases = "countries reported last 28 days",
    percent_cases = "% of countries",
    country_deaths = "countries reported last 28 days",
    percent_deaths = "% of countries")  %>% 
  
  merge_at(i = 1, j = 3:4, part = "header") %>% # Horizontally merge columns 3 to 5 in new header row
  merge_at(i = 1, j = 5:6, part = "header")     # Horizontally merge columns 6 to 8 in new header row

cases_deahts  # print


# define style for border line
border_style = officer::fp_border(color="black", width=1)

# add border lines to table
cases_deahts <- cases_deahts %>% 
  
  # Remove all existing borders
  border_remove() %>%  
  
  # add horizontal lines via a pre-determined theme setting
  theme_booktabs() %>% 
  
  # add vertical lines to separate Recovered and Died sections
  vline(part = "all", j = 2, border = border_style) %>%   # at column 2 
  vline(part = "all", j = 4, border = border_style)%>%    # at column 5
  flextable::align(align = "center", j = c(2:6), part = "all")%>%
  fontsize(i = 1, size = 12, part = "header") %>%   # adjust font size of header
  bold(i = 1, bold = TRUE, part = "header") %>%     # adjust bold face of header
  bold(i = 6, bold = TRUE, part = "body")

cases_deahts














