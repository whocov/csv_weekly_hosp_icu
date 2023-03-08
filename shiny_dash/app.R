#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

install.packages("shiny")
pacman::p_load(learnr,here, rio,openxlsx, remotes, tidyverse,skimr, epikit,       
               lubridate,stringr,base,janitor, gtsummary, corrr, caret,broom,httr,jsonlite,cowplot,          
               RColorBrewer, gghighlight,ggrepel,ggExtra,tsibble, viridis,scales, apyramid,
               rmarkdown,knitr,flextable)

 # Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Weekly new hospitalization"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
          selectInput(
            inputId = "select_year",
            label   = "Select year",
            choices = c(
              "All",
              "2022",
              "2023"
            ),
            selected = "All",
            multiple = TRUE
          ),
          selectInput(
            inputId = "select_region",
            label   = "Select region",
            choices = c(
              "All",
              "AFR",
              "AMR",
              "EMR",
              "EUR",
              "SEAR",
              "WPR",
              "OTHER"
            ),
            selected = "All",
            multiple = TRUE
          )

        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("new_hospitalization_epicurve ")
        )
    )
)

# read data
hosp_data <- rio::import(here::here("data","raw","data_export_NCOV_A_COMBINED_WEEK_SEX_AGEGROUP_MIXEDSOURCES(1).csv")) %>% 
  as_tibble()
historical_data <- rio::import(here::here("data","clean","historical_clean_data.csv")) %>% 
  as_tibble()


hosp_data <- hosp_data%>%filter(SEX == "All" & AGEGROUP == "All" & ISO_START_DATE >"2022-10-31" & ISO_START_DATE<"2023-1-30")%>%
  group_by(WHO_REGION, COUNTRY_NAME, ISO_YEAR,ISO_WEEK, ISO_START_DATE)%>%
  summarise(weekly_cases  = sum(DAILY_CASES, na.rm = T),
            weekly_deaths = sum(DAILY_CASES_DEATHS, na.rm = T),
            weekly_hosp   = sum(DETAILED_CASES_HOSPITALISED, na.rm = T))


####### Filter EUR region
Region_hosp<- hosp_data%>%filter(WHO_REGION == "EUR")%>%
  group_by(COUNTRY_NAME) %>% filter(!all(weekly_hosp == 0))%>%
  mutate(ratio_death_hosp = round(weekly_deaths/weekly_hosp, digits = 2))%>%view()


### Plot graphic
ggplot( data = Region_hosp, aes(x = ISO_START_DATE, y = weekly_hosp))+
  geom_line(aes(color = "New Hospitalization"),size =1)+
  geom_line(data = Region_hosp, aes(x = ISO_START_DATE, y = weekly_deaths, color = "Deaths"),size =1)+
  scale_color_discrete(name = '', labels = c( 'Deaths','New hospitalization'))+
  scale_x_date(
    date_breaks = "4 weeks",
    date_labels = "%Y -W%W")+
  scale_y_continuous(
    position = "right")+
  theme_cowplot()+
  facet_wrap(~COUNTRY_NAME, scales = "free")+
  theme(axis.text.x = element_text(size = 8))+
  labs(
    x = "Weekly report",
    y = "cases in number",
    title = "SEAR: Time series of countries reporting new hospitalization and deaths, period: from W44:2022 to W4_2023"
  )

####### Filter EUR region
global_hosp<- hosp_data%>%
  group_by(COUNTRY_NAME) %>% filter(!all(weekly_hosp == 0))%>%
  mutate(ratio_death_hosp = round(weekly_deaths/weekly_hosp, digits = 2))

global_hosp_1 <- global_hosp%>%group_by(WHO_REGION, ISO_START_DATE, ISO_YEAR)%>%
  summarise(death = sum(weekly_deaths, na.rm = T),
            hosp  = sum(weekly_hosp, na.rm = T))%>%view()


### Plot global Graphic
ggplot( data = global_hosp_1, aes(x = ISO_START_DATE, y = hosp))+
  geom_line(aes(color = "New Hospitalization"),size =1)+
  geom_line(data = global_hosp_1, aes(x = ISO_START_DATE, y = death, color = "Deaths"),size =1)+
  scale_color_discrete(name = '', labels = c( 'Deaths','New hospitalization'))+
  scale_x_date(
    date_breaks = "3 weeks",
    date_labels = "%Y -W%W")+
  scale_y_continuous(
    expand = c(0,0))+ 
  theme_cowplot()+
  facet_wrap(~WHO_REGION, scales = "free")+
  theme(axis.text.x = element_text(size = 8))+
  labs(
    x = "Weekly week",
    y = "cases in number",
    title = "GLOBAL: Time series of WHO Regions reporting new hospitalization and deaths, period: from W44:2022 to W4_2023"
  )


################## plot by Region
hosp_data_2<- hosp_data%>%
  group_by(WHO_REGION, COUNTRY_NAME)%>%
  filter(!all(weekly_hosp == 0))%>%
  mutate(ISO_START_DATE  = as.Date(ISO_START_DATE,format='%d-%B-%Y'))


hosp_data_2%>%group_by(WHO_REGION, COUNTRY_NAME)%>%
  filter(WHO_REGION == "EUR")%>%
  summarise(total_hosp      = sum(weekly_hosp, na.rm = T ),
            death           = sum(weekly_deaths, na.rm = T),
            total_hosp_7day = sum(weekly_hosp[ISO_START_DATE>max(ISO_START_DATE)-14], na.rm = T),
            death_7day      = sum(weekly_deaths[ISO_START_DATE>max(ISO_START_DATE)-14], na.rm = T))%>%
  mutate(ratio           = round(death / total_hosp, digits = 2),
         ratio_7d        = round(death_7day/total_hosp_7day, digits = 2))%>%
  dplyr::select(WHO_REGION, COUNTRY_NAME, total_hosp, death, ratio,total_hosp_7day, death_7day,ratio_7d)%>%
  mutate(ratio_7d = recode(ratio_7d, "Inf" = 0, NULL =0))%>%
  flextable()%>%autofit()%>%
  width(j = c(1,2,3,4,5,6,7,8), width = 2.0)%>%
  add_header_row(
    top = TRUE,
    values = c("Region",
               "Country",
               "Cumulative from W44-2022 to W4-2023",
               "",
               "",
               "last 14 days",
               "",
               ""
    ))%>%
  set_header_labels(
    WHO_REGION      = "",
    COUNTRY_NAME    = "",
    total_hosp      = "Hospitalization",
    death           = "Deaths",
    ratio           = "Deaths Ratio (%)",
    total_hosp_7day = "Hospitalization in last 14 days",
    death_7day      = "Deaths in last 14 days",
    ratio_7d        = "Deaths Ratio (%) in last 14 days"
  )%>%
  border_remove()%>%
  theme_booktabs()%>%
  fontsize(i = 1, size = 12, part = "header") %>%   # adjust font size of header
  bold(i = 1, bold = TRUE, part = "header") %>%     # adjust bold face of header
  # add vertical lines to separate Recovered and Died sections
  vline(part = "all", j = 1) %>%   # at column 2 
  vline(part = "all", j = 2)%>%      # at column 5
  vline(part = "all", j = 5)%>%        # at column 5
  merge_at(i = 1, j = 4:5, part = "header")%>%  # Horizontally merge columns 4 to 6 in new header row
  merge_at(i = 1:2, j = 1, part = "header") %>% 
  merge_at(i = 1:2, j = 2, part = "header")%>%
  bg(j = 5, i = ~ ratio >= 1.1, part = "body", bg = "red") %>%
  bg(j = 8, i = ~ ratio >= 1.1, part = "body", bg = "red")%>%
 # bg(j = 8, i = 10, part = "body", bg = "red")%>%
  #bg(j = 8, i = 14 , part = "body", bg = "red")%>%
  #bg(j = 8, i = 15 , part = "body", bg = "red")%>%
  #bg(j = 8, i = 21 , part = "body", bg = "red")%>%
  #bg(j = 8, i = 28 , part = "body", bg = "red")%>%
  bg(., i= ~ COUNTRY_NAME == "Canada", part = "body", bg = "#91c293")%>%
  bg(., i= ~ COUNTRY_NAME == "Honduras", part = "body", bg = "#91c293")%>%
  flextable::align(align = "center", j = 1, part = "all")
  
####################################################
######### historical data clean
hosp_data_01 <- hosp_data%>%
  filter(COUNTRY_NAME == "Bulgaria" | COUNTRY_NAME == "Canada"| COUNTRY_NAME == "Denmark"| COUNTRY_NAME == "Ireland"| COUNTRY_NAME == "New Zealand"|
         COUNTRY_NAME == "Norway" | COUNTRY_NAME == "Puerto Rico" | COUNTRY_NAME == "Switzerland" |COUNTRY_NAME == "The United Kingdom" |
           COUNTRY_NAME == "United States of America" | COUNTRY_NAME == "United States Virgin Islands")

# rename column name 
historical_data <- historical_data%>%
  filter(epiweek >"2022-10-31")%>%
  mutate(country = str_to_lower(country))%>%
  mutate(country = str_to_title(country))%>%
  mutate(country =  recode(country, "United States Of America" = "United States of America"))%>%
  dplyr::select(country, epiweek, new_hospitalization)%>%
  rename(historical = new_hospitalization)


# rename new_hosp column name to 
hosp_data_01<- hosp_data_01%>%rename(xmart = weekly_hosp,  country = COUNTRY_NAME, epiweek = ISO_START_DATE)


##### Join 2 tables 
hist_xmart_tables <- full_join(historical_data, hosp_data_01, by = c("country" ,"epiweek"))%>%
  dplyr::select(-c(ISO_WEEK, ISO_YEAR))

hist_xmart_tables<- hist_xmart_tables%>%
  mutate(WHO_REGION = replace(WHO_REGION, country == "Bulgaria", "EUR"))%>%
  mutate(WHO_REGION = replace(WHO_REGION, country == "Ireland" , "EUR"))%>%
  mutate(WHO_REGION = replace(WHO_REGION, country == "United States of America" , "AMR"))%>%
  mutate(WHO_REGION = replace(WHO_REGION, country == "Puerto Rico", "AMR"))%>%
  mutate(WHO_REGION = replace(WHO_REGION, country == "United States Virgin Islands", "AMR"))

hist_xmart_tables_1 <- hist_xmart_tables%>%filter(country == "Bulgaria" | country == "Puerto Rico" | country == "United States of America" |
                                                    country == "Switzerland" )%>%
 dplyr::select(WHO_REGION, country, epiweek, weekly_cases, weekly_deaths, xmart, historical)

hist_xmart_tables_2<- hist_xmart_tables_1%>%filter(epiweek<"2023-01-30")%>%
  group_by(WHO_REGION, country, epiweek)%>%
  mutate(xmart_ratio = round(weekly_deaths/xmart, digits = 2),
         historical_ratio  = round(weekly_deaths/historical, digits = 2))%>%
  mutate(xmart_ratio = recode(xmart_ratio, "Inf" = "0"))

# Plot a table that compare both source
hist_xmart_tables_2%>%flextable()%>%autofit()
  dplyr::select(-c(weekly_cases, xmart_ratio))%>%
  width(j = c(1,2,3,4,5,6,7), width = 2.0)%>%
  add_header_row(
    top = TRUE,
    values = c("Region",
               "Country",
               "Epi week",
               "XMART source",
               "",
               "Historical source",
               ""
    ))%>%
  set_header_labels(
    WHO_REGION              = "",
    country                 = "",
    epiweek                 = "",
    weekly_deaths           = "Deaths",
    xmart                   = "New Hosp_XMART",
    historical              = "New Hosp_XMART",
    historical_ratio        = "Death Ratio"
  )%>%
  border_remove()%>%
  theme_booktabs()%>%
  fontsize(i = 1, size = 12, part = "header") %>%   # adjust font size of header
  bold(i = 1, bold = TRUE, part = "header") %>%     # adjust bold face of header
  # add vertical lines to separate Recovered and Died sections
  vline(part = "all", j = 1) %>%   # at column 2 
  vline(part = "all", j = 2)%>%      # at column 5
  vline(part = "all", j = 5)%>%        # at column 5
  merge_at(i = 1, j = 4:5, part = "header")%>%  # Horizontally merge columns 4 to 6 in new header row
  merge_at(i = 1:2, j = 1, part = "header") %>% 
  merge_at(i = 1:2, j = 2, part = "header")%>%
  bg(j = 5, i = ~ ratio >= 1.1, part = "body", bg = "red") %>%
  bg(j = 8, i = ~ ratio >= 1.1, part = "body", bg = "red")%>%
  # bg(j = 8, i = 10, part = "body", bg = "red")%>%
  #bg(j = 8, i = 14 , part = "body", bg = "red")%>%
  #bg(j = 8, i = 15 , part = "body", bg = "red")%>%
  #bg(j = 8, i = 21 , part = "body", bg = "red")%>%
  #bg(j = 8, i = 28 , part = "body", bg = "red")%>%
  bg(., i= ~ COUNTRY_NAME == "Canada", part = "body", bg = "#91c293")%>%
  bg(., i= ~ COUNTRY_NAME == "Honduras", part = "body", bg = "#91c293")%>%
  flextable::align(align = "center", j = 1, part = "all")








plot_epicurve <- function(data, region = "All",  year = "All"){
  
  if (!("All" %in% region)) {
    data <- data %>%
      filter(Region %in% region)
  }
  
  plot_title_region <- stringr::str_glue("{paste0(region, collapse = ', ')} region")
  
} else {
  
  plot_title_region <- "all region"
  
}

# if no remaining data, return NULL
if (nrow(data) == 0) {
  
  return(NULL)
}

data <- data %>%
  filter(year == year)


# if no remaining data, return NULL
if (nrow(data) == 0) {
  
  return(NULL)
}

if (year == "all_year") {
  year_title <- "All year"
} else {
  year_title <- stringr::str_glue("{str_remove(year, 'year')} years")
}


ggplot(data, aes(x = data_date, y = cases_reported)) +
  geom_col(width = 1, fill = "darkred") +
  theme_minimal() +
  labs(
    x = "date",
    y = "number of cases",
    title = stringr::str_glue("Malaria cases - {plot_title_district}"),
    subtitle = agegroup_title
  )

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white',
             xlab = 'Waiting time to next eruption (in mins)',
             main = 'Histogram of waiting times')
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
