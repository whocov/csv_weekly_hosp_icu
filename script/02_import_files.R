
###########3 Hospitalization and New critical #############################
# This script load library and import data file from countries through API connection
#     Author: Joao Muianga (joao.muianga1@gmail.com / muiangaj@who.int)
#     Date updated: 22 Nov 2022
##########################################################################
message("\n...running 02_connections_file.R")

# Downloads and installs packages (if they are not already installed)
#####################################################################
pacman::p_load(
  
  # learning R
  ############
  learnr,   # interactive tutorials
  
  # project and file management
  #############################
  here,     # file paths relative to R project root folder
  rio,      # import/export of many types of data
  openxlsx, # special functions for handling Excel workbooks
  
  # package install and management
  ################################
  remotes,  # install from github
  
  # general data management
  #########################
  tidyverse,    # includes many packages for tidy data wrangling and presentation
  skimr,        # data exploration
  epikit,       # useful epi functions
  lubridate,    # dates and times
  stringr,      # handling characters
  base,
  
  # tables and statistics  
  #######################
  janitor,      # tables and data cleaning
  gtsummary,    # making descriptive and statistical tables
  corrr,
  caret,
  broom,
  httr,
  jsonlite,
  
  # plots - general
  #################
  #ggplot2,         # included in tidyverse
  cowplot,          # combining plots  
  RColorBrewer,     # color scales
  gghighlight,      # highlight a subset
  ggrepel,          # smart labels
  ggExtra,          # fancy plots  
  tsibble,          # epiweeks
  viridis,          # colorblind-friendly scales
  scales,           # helper functions
  apyramid,         # age and sex pyramids
  
  # routine reports
  #################
  rmarkdown,        # produce PDFs, Word Documents, Powerpoints, and HTML files
  
  # tables for presentation
  #########################
  knitr,            # R Markdown report generation and html tables
  flextable)        # HTML tables


#############################################################################################
### Import file of countries who have API GET json or Github

########## New zealand - file coming from Github
# Manual import
new_zealand_file <- read.csv("https://raw.githubusercontent.com/minhealthnz/nz-covid-data/19a3dc6c82240915a45227d5b4b102730a7c43ed/cases/weekly-hospitalisations-for-covid.csv")
new_zealand_file_1<-read.csv("https://raw.githubusercontent.com/minhealthnz/nz-covid-data/main/cases/weekly-hospitalisations-for-covid.csv")
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

########### France
################ country is using csv file
france_data <- read.csv("https://www.data.gouv.fr/fr/datasets/r/6fadff46-9efd-4c53-942a-54aca783c30c")

######### Bulgaria
# prepare request
path_bulg <- "https://data.egov.bg/resource/download/e59f95dd-afde-43af-83c8-ea2916badd19/json"
request_bulg <- GET(url = path_bulg)
request_bulg$status_code  # check for any server error ("200" is good!)


# submit the request, parse the response, and convert to a data frame
bulgaria_api <- base::rawToChar(request_bulg$content)
bulgaria_file <- jsonlite::fromJSON(bulgaria_api, flatten = TRUE)


########## Switzerland
# prepare request
path_swit <- "https://www.covid19.admin.ch/api/data/20230117-i4oav3jt/sources/COVID19Hosp_reason_AKL10_w.json"
request_swit <- GET(url = path_swit)
request_swit$status_code   # check for any server error ("200" is good!)

# submit the request, parse the response, and convert to a data frame
swit_api <- base::rawToChar(request_swit$content)
switzerland_file <- jsonlite::fromJSON(swit_api, flatten = TRUE)

##############Ireland
######### Ireland hospitalization API
ireland_hosp_file <- "https://services-eu1.arcgis.com/z6bHNio59iTqqSUY/arcgis/rest/services/Covid19AcuteHospitalHistoricSummaryOpenData/FeatureServer/0/query?outFields=*&where=1%3D1&f=geojson"
request_ireland_hosp <- GET(url = ireland_hosp_file)
request_ireland_hosp$status_code   # check for any server error ("200" is good!)

# submit the request, parse the response, and convert to a data frame
ireland_hosp_1 <- base::rawToChar(request_ireland_hosp$content)
ireland_hosp <- jsonlite::fromJSON(ireland_hosp_1, flatten = TRUE)

######### Ireland ICU API
ireland_icu_file <- "https://services-eu1.arcgis.com/z6bHNio59iTqqSUY/arcgis/rest/services/ICUBISHistoricTimelinePublicView/FeatureServer/0/query?outFields=*&where=1%3D1&f=geojson"
request_ireland_icu <- GET(url = ireland_icu_file)
request_ireland_icu$status_code   # check for any server error ("200" is good!)

# submit the request, parse the response, and convert to a data frame
ireland_icu_1 <- base::rawToChar(request_ireland_icu$content)
ireland_icu <- jsonlite::fromJSON(ireland_icu_1, flatten = TRUE)


###############################################################################
# import manually data from countries who don't have API connection available
##############################################

############ Norway
norway_hosp <- import(here("data", "raw", "Norway_hosp.xlsx"))
norway_icu <- import(here("data", "raw","Norway_icu.xlsx"))

############ Denmark
# official website: https://covid19.ssi.dk/overvagningsdata/download-fil-med-overvaagningdata
denmark_file <- import(here("data", "raw", "Newly_admitted_over_time.csv"))

######### United kingdom 
# https://coronavirus.data.gov.uk/details/healthcare?areaType=overview&areaName=United%20Kingdom
united_kingdom_hosp <- import(here("data", "raw", "united_k_hosp_data_2022-Nov-17.csv"))
united_kingdom_icu <- import(here("data", "raw","united_k_icu_data_2022-Nov-17.csv"))

######### Canada
canada_file <- import(here('data', "raw","covid19-epiSummary-hospVentICU (1).csv"))


