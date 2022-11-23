# csv_weekly_hosp_icu
The repository contain R script that generates the combined historical data of hospitalization and new icu data from official website source

- Purpose: Run R script and export the excel file to XMART 
- Author: Joao Muianga /  muiangaj@who.int
- Date: 22 Nov 2022

The folder official website data contain automated R script that get country data of hospitalization and new ICU through API available and manually file and do the data clean.
There are 2 folders
1- Script folder 
> 01_load_packages.R - load packages needs
> 02_import_files.R  - import API automaticaly from API json file available on the countries website and manually
> 03_dataclean.R     - combine all countries, clean duplication, check names


2- Data folder 
> raw   - all files downloads manually of countries who don't have API
> clean - combined and clean historical data in excel format (historical_clean_data.xlsx)

when you download the folder use official_website_data.Rproj

3- Countries with API 
- New Zealand
- United States of America
- Bulgaria
- Switzerland
- Ireland

4- Countries with file
- Canada
- United Kingdom
- Norway
- Denmark

5- Countries reporting New hospitalization and ICU
- New Zealand
- Ireland
- United Kingdom
- Norway
- Canada

6- Countries reporting New hospitalization
- United States of America
- Bulgaria
- Switzerland
- Denmark


