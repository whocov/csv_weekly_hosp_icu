################################################################################
# Historical data: data clean by country
# Author: Joao Muianga
# Date: 22-Nov-2022
################################################################################
message("\n...running 03_dataclean.R")
source(here("script", "02_import_files.R"))

####################################################################################
########## Import files through API
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

max(usa_data_full$report_date)

############### Switzerland data clean
switzerland_1<- switzerland_file%>%
  filter(geoRegion == "CH" & altersklasse_covid19 == "all" & primary_hosp_reason == "covid")%>%
  select(geoRegion, date, entries)%>%
  rename(country_name              = geoRegion,                                                # rename columns
         new_hospitalization    = entries )%>%
  mutate(country_name = recode(
    country_name,"CH" = "Switzerland"))%>%                                                   # change CH to Switzerland
  transform(iso_year = substr(date, 1,4), iso_week = substr(date, 5, 6))                      # separe year with week

switzerland_2 <-switzerland_1%>%filter(iso_week != 53)%>%                                     # remove week 53
  mutate(date_num= as.character(paste0(date, "1")),
         report_date = as.Date(date_num , format = "%Y%W%u"))

switzerland_data <- switzerland_2%>%select(country_name, report_date, new_hospitalization)%>%
  mutate(country_name        = as.character(country_name),
         report_date         = as.Date(report_date),
         new_hospitalization = as.numeric(new_hospitalization))%>%
  group_by(country_name, report_date)%>%
  summarise(new_hospitalization = sum(new_hospitalization, na.rm = T))
  



####################### New Zealand
new_zealand_data <- new_zealand_file_1%>%
  filter(Variable.Label == "Sex")%>%
  select(Admissions.for.COVID.19.in.the.week.ending, Hospitalisations, ICU)%>%
  rename(report_date            =  Admissions.for.COVID.19.in.the.week.ending,             # change column name
         new_hospitalization    =  Hospitalisations,
         new_icu                =  ICU )%>%
  mutate(country_name = "New Zealand",
         report_date  = as.Date(report_date))%>%
  group_by(country_name, report_date)%>%
  summarise(new_hospitalization = sum(new_hospitalization, na.rm = T), new_icu = sum(new_icu, na.rm = T))


# Bulgaria data source
#colnames(bulgaria_file) <- as.character(unlist(bulgaria_file[1,]))                         # assign headers based on existing row in dataframe in R
#bulgaria_file = bulgaria_file[-1, ]

#bulgaria_data<- as.data.frame(bulgaria_file)%>%                                            # convert to dataframe
#          select(Дата, Новохоспитализирани )%>%
#          rename( report_date         = Дата,
#                  new_hospitalization = Новохоспитализирани)%>%
#          mutate(country_name         = "Bulgaria",
#                 report_date          = as.Date(report_date),
#                 new_hospitalization  = as.numeric(new_hospitalization))%>%
#  group_by(country_name, report_date)%>%
#  summarise(new_hospitalization = sum(new_hospitalization, na.rm = T))


############## United Kingdom
unitedKingdom_data <- full_join(united_kingdom_hosp, united_kingdom_icu , by = c("areaName", "date"))
unitedKingdom_data <- unitedKingdom_data%>%select(date, areaName, newAdmissions,covidOccupiedMVBeds)%>%
  rename(report_date         = date,
         country_name        = areaName,
         new_hospitalization = newAdmissions,
         new_icu             = covidOccupiedMVBeds)%>%
  mutate(report_date = as.Date(report_date))%>%
  group_by(country_name, report_date)%>%
  summarise(new_hospitalization = sum(new_hospitalization, na.rm = T), new_icu = sum(new_icu, na.rm = T))



############ Ireland hospitalization
ireland_hosp_data <- as.data.frame(ireland_hosp)
ireland_hosp_data <- ireland_hosp_data%>%
  select(features.properties.Date, features.properties.SUM_no_new_admissions_covid19_p)%>%
  rename( date         = features.properties.Date,
          new_hospitalization = features.properties.SUM_no_new_admissions_covid19_p)%>%
  mutate(country_name = "Ireland",
         date = as.numeric(date))
 ireland_hosp_data_1 <- ireland_hosp_data%>%
   mutate(report_date = as.POSIXct(date /1000 ,origin = "1970-01-01", tz = "timezone"))
 ireland_hosp_dat1 <- ireland_hosp_data_1%>%
   select(country_name, report_date, new_hospitalization)%>%mutate(report_date = as.Date(report_date))

 
 ############ Ireland ICU
 ireland_icu_data <- as.data.frame(ireland_icu)
 ireland_icu_data <- ireland_icu_data%>%
   select(features.properties.extract, features.properties.adcconf)%>%
   rename( date         = features.properties.extract,
           new_icu = features.properties.adcconf)%>%
   mutate(country_name = "Ireland",
          date = as.numeric(date))
 ireland_icu_data_1 <- ireland_icu_data%>%
   mutate(report_date = as.POSIXct(date /1000 ,origin = "1970-01-01", tz = "timezone"))
 ireland_icu_dat1 <- ireland_icu_data_1%>%
   select(country_name, report_date, new_icu)%>%mutate(report_date = as.Date(report_date))

########### Ireland join table

ireland_data <- full_join(ireland_hosp_dat1, ireland_icu_dat1, by=c("country_name", "report_date"))%>%                            # Join icu and hospitalization in Ireland
  group_by(country_name, report_date)%>%
  summarise(new_hospitalization = sum(new_hospitalization, na.rm = T), new_icu = sum(new_icu, na.rm = T))

############## Denmark 
denmark_data<- denmark_file%>%select(Dato, Total)%>%
  rename( report_date         = Dato,
          new_hospitalization = Total)%>%
  mutate( report_date = as.Date(report_date),
          country_name = "Denmark")%>%
  group_by(country_name, report_date)%>%
  summarise(new_hospitalization = sum(new_hospitalization, na.rm = T))

############ Norway hospitalization
norway_hosp_data<- norway_hosp%>%select(date, new_hospit)%>%
  rename( report_date         = date,
          new_hospitalization = new_hospit)%>%
  mutate( country_name = "Norway",
          report_date  = as.Date(report_date, format = "%d/%m/%Y"))

########## Norway ICU
norway_icu_data<- norway_icu%>%select(date, new_icu)%>%
  rename( report_date   = date)%>%
  mutate( country_name = "Norway",
          report_date  = as.Date(report_date))
####### Join norway 
norway_data <- full_join(norway_hosp_data, norway_icu_data , by = c("country_name", "report_date")) %>%
  group_by(country_name, report_date)%>%
  summarise(new_hospitalization = sum(new_hospitalization, na.rm = T), new_icu = sum(new_icu, na.rm = T))

######### Canada
canada_data <- canada_file%>%select(Date, COVID_NEWICU, COVID_NEWOTHER)%>%
  mutate(report_date         = as.Date(Date),
         new_icu = as.numeric(COVID_NEWICU),
         new_hospitalization   = as.numeric(COVID_NEWOTHER),
         country_name = "Canada")

canada_data_full <- canada_data%>%select(report_date, country_name, new_icu, new_hospitalization)%>%
  group_by(country_name, report_date)%>%
  summarise(new_hospitalization = sum(new_hospitalization, na.rm = T), new_icu = sum(new_icu, na.rm = T))


############## Puerto Rico and Virgin Island (US)
########## Puerto Rico and Virgin Island (US) have 2 API with max of 50000000 row per API, to complete join both for one file
################ Puerto Rico and Virgin Island (US) file 1
PR_VI_data_1 <- USA_file%>%
  # filter interested state
  filter(state == "PR" | state == "VI")%>%
  select(date, state, previous_day_admission_adult_covid_confirmed, previous_day_admission_pediatric_covid_confirmed)%>%
  replace(is.na(.), 0) %>%
  # calculate new hospitalization column
  mutate(previous_day_admission_adult_covid_confirmed     = as.numeric(previous_day_admission_adult_covid_confirmed),
         previous_day_admission_pediatric_covid_confirmed = as.numeric(previous_day_admission_pediatric_covid_confirmed),
         new_hospitalization = previous_day_admission_adult_covid_confirmed + previous_day_admission_pediatric_covid_confirmed,
         date = as.Date(date),
         state= recode(state,
                       "PR" = "Puerto Rico",
                       "VI" = "United States Virgin Islands"))%>% rename( report_date = date, country_name = state)


################ Puerto Rico and Virgin Island (US) file 2
PR_VI_data_2 <- USA_file_1%>%
  # filter interested state
  filter(state == "PR" | state == "VI")%>%
  select(date, state, previous_day_admission_adult_covid_confirmed, previous_day_admission_pediatric_covid_confirmed)%>%
  replace(is.na(.), 0) %>%
  # calculate new hospitalization column
  mutate(previous_day_admission_adult_covid_confirmed     = as.numeric(previous_day_admission_adult_covid_confirmed),
         previous_day_admission_pediatric_covid_confirmed = as.numeric(previous_day_admission_pediatric_covid_confirmed),
         new_hospitalization = previous_day_admission_adult_covid_confirmed + previous_day_admission_pediatric_covid_confirmed,
         report_date = as.Date(date),
         state= recode(state,
                       "PR" = "Puerto Rico",
                       "VI" = "United States Virgin Islands"))%>% rename( country_name = state)

# join the two API in one
PR_VI_dataset <- full_join(PR_VI_data_1, PR_VI_data_2, by=c("country_name", "report_date", "new_hospitalization"))
PR_VI_dataset_1 <- PR_VI_dataset%>%select(country_name, report_date, new_hospitalization)

PR_VI_dataset_full<-PR_VI_dataset_1%>%filter(new_hospitalization != 0)%>%
  group_by(country_name, report_date)%>%
  summarise(new_hospitalization = sum(new_hospitalization, na.rm = T))


#####################################################################################
# Join all countries by report date
#################################################################
data_1 <- switzerland_data
data_2 <- full_join(new_zealand_data,unitedKingdom_data, by=c("report_date", "country_name", "new_hospitalization", "new_icu"))
data_3 <- full_join(ireland_data,norway_data, by=c("report_date", "country_name", "new_hospitalization", "new_icu"))
data_4 <- full_join(usa_data_full, denmark_data, by=c("report_date", "country_name", "new_hospitalization"))

######### Join datasets
data_1_4 <- full_join(data_1,data_4, by = c("report_date", "country_name", "new_hospitalization"))
data_2_3 <- full_join(data_2, data_3, by = c("report_date", "country_name", "new_hospitalization", "new_icu"))
data_2_3_canada <- full_join(data_2_3, canada_data_full, by = c("report_date", "country_name", "new_hospitalization", "new_icu"))
data_5_1_4 <- full_join(PR_VI_dataset_full, data_1_4, by = c("report_date", "country_name", "new_hospitalization"))

historical_data <- full_join(data_2_3_canada, data_5_1_4, by = c("report_date", "country_name", "new_hospitalization"))

###################################################################
## clean historical data
################################################################
## remove  duplication
historical_data_1 <- historical_data%>%
  group_by(report_date, country_name) %>%       # group the rows by 'name'
  slice_max(new_hospitalization,         # keep row per group with maximum date value 
            n = 1,         # keep only the single highest row 
            with_ties = F) # if there's a tie (of date), take the first row
                                   
# Create week number column and summarise
historical_data_2 <- historical_data_1%>%
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
historical_data_3 <- historical_data_2%>%
  group_by(country_name, epiweek)%>%
  mutate(freq_time = n())

########################################################
historical_data_4<- historical_data_3%>%
  mutate(
    freq = if_else(
      condition = freq_time == 1, 
      true      = "weekly", 
      false     = "daily"
    ),
    iso_year    = year(epiweek),
    iso_week_number = week(epiweek))

historical_data_full <- historical_data_4%>%
  select(country_name, iso_year, iso_week_number, epiweek, freq, freq_time, new_hospitalization, new_icu)%>%
  rename(country = country_name)


###################Change country name values
historical_data_full <- historical_data_full%>%
  mutate(country = recode(
    country,
     "United States of America" = "UNITED STATES OF AMERICA",
     "Norway"                   = "NORWAY",
     "United Kingdom"           = "THE UNITED KINGDOM",
     "Switzerland"              = "SWITZERLAND",
     "Bulgaria"                 = "BULGARIA",
     "Ireland"                  = "IRELAND",
     "New Zealand"              = "NEW ZEALAND",
     "Denmark"                  = "DENMARK",
     "Canada"                   = "CANADA",
     "Puerto Rico"              = "PUERTO RICO",
    "United States Virgin Islands"  =  "UNITED STATES VIRGIN ISLANDS"
 
  ))

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



