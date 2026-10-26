###############################################################################
# Import, clean, and export USA weekly COVID-19 hospital admissions
# Author: Craig Schultz
# Source: CDC NHSN — Weekly Hospital Respiratory Data by Jurisdiction
###############################################################################

# Load required libraries
pacman::p_load(
  here,
  rio,
  readr,
  dplyr,
  janitor,
  lubridate,
  httr
)

###############################################################################
# Settings
###############################################################################

cdc_hrd_url <- paste0(
  "https://data.cdc.gov/api/views/ua7e-t2fy/",
  "rows.csv?accessType=DOWNLOAD"
)

# Filter using the original CDC week-ending date
cdc_start_date <- as.Date("2020-01-01")

output_file <- here::here(
  "data", "clean", "historical_clean_data.csv"
)

# Exact column names and order required in the final export
export_columns <- c(
  "WHO_REGION",
  "ISO_3_CODE",
  "country",
  "epiweek",
  "new_hospitalization",
  "new_icu",
  "iso_year",
  "iso_week_number"
)

###############################################################################
# Import the new CDC CSV
###############################################################################

cdc_hrd_response <- httr::RETRY(
  verb = "GET",
  url = cdc_hrd_url,
  httr::timeout(180),
  times = 3,
  pause_min = 2
)

httr::stop_for_status(cdc_hrd_response)

cdc_hrd_data <- readr::read_csv(
  file = I(
    httr::content(
      cdc_hrd_response,
      as = "text",
      encoding = "UTF-8"
    )
  ),
  col_types = readr::cols(.default = readr::col_character()),
  na = c("", "NA"),
  show_col_types = FALSE
) %>%
  janitor::clean_names()

# Validate the source columns
required_columns <- c(
  "week_ending_date",
  "geographic_aggregation",
  "total_covid_19_admissions"
)

missing_columns <- setdiff(required_columns, names(cdc_hrd_data))

if (length(missing_columns) > 0L) {
  stop(
    "Missing required CDC columns: ",
    paste(missing_columns, collapse = ", ")
  )
}

###############################################################################
# Select USA national admissions
###############################################################################

cdc_hrd_usa <- cdc_hrd_data %>%
  filter(trimws(geographic_aggregation) == "USA") %>%
  transmute(
    week_ending_date = lubridate::ymd(week_ending_date),
    total_covid_19_admissions = readr::parse_double(
      total_covid_19_admissions
    )
  )

if (nrow(cdc_hrd_usa) == 0L) {
  stop("No USA national records were found.")
}

if (anyNA(cdc_hrd_usa$week_ending_date)) {
  stop("Some USA week-ending dates are missing or invalid.")
}

cdc_hrd_usa <- cdc_hrd_usa %>%
  filter(week_ending_date >= cdc_start_date)

if (nrow(cdc_hrd_usa) == 0L) {
  stop("No USA records were found for the selected date range.")
}

###############################################################################
# Construct the exact original export structure
###############################################################################

# transmute() creates only the eight required export columns.
# No separate 'date' or 'week_ending_date' column is retained.
CDC_USA_clean_data <- cdc_hrd_usa %>%
  transmute(
    WHO_REGION = "AMRO",
    ISO_3_CODE = "USA",
    country = "UNITED STATES OF AMERICA",
    
    # Preserve the original Monday epiweek labelling convention.
    # Counts still refer to CDC's Sunday–Saturday reporting period.
    epiweek = as.Date(
      lubridate::floor_date(
        week_ending_date,
        unit = "week",
        week_start = 1
      )
    ),
    
    new_hospitalization = total_covid_19_admissions,
    
    # New ICU admissions are unavailable; preserve missing values.
    new_icu = NA_real_,
    
    # Use the ISO year corresponding to the ISO week.
    iso_year = lubridate::isoyear(epiweek),
    iso_week_number = lubridate::isoweek(epiweek)
  ) %>%
  arrange(epiweek) %>%
  select(all_of(export_columns))

###############################################################################
# Validate before exporting
###############################################################################

# One national record per week is expected.
# Stop on duplicates instead of silently choosing the largest count.
if (anyDuplicated(CDC_USA_clean_data$epiweek) > 0L) {
  stop("Duplicate USA weeks found. Review the source before exporting.")
}

if (!identical(names(CDC_USA_clean_data), export_columns)) {
  stop("Final column names or order do not match the required structure.")
}

if (anyNA(CDC_USA_clean_data$new_hospitalization)) {
  warning("Some admission counts are missing; exported as empty fields.")
}

###############################################################################
# Export using rio, as in the original script
###############################################################################

dir.create(
  dirname(output_file),
  recursive = TRUE,
  showWarnings = FALSE
)

rio::export(
  as.data.frame(CDC_USA_clean_data),
  file = output_file,
  na = ""
)

###############################################################################
# Verify the saved CSV structure
###############################################################################

cdc_export_check <- readr::read_csv(
  output_file,
  col_types = readr::cols(.default = readr::col_character()),
  show_col_types = FALSE
)

if (
  !identical(names(cdc_export_check), export_columns) ||
  nrow(cdc_export_check) != nrow(CDC_USA_clean_data)
) {
  stop("The saved CSV does not match the expected structure or row count.")
}

# Show the actual CSV header and first five records
cat(
  paste(readLines(output_file, n = 6), collapse = "\n"),
  "\n"
)

message(
  "Export complete: ",
  nrow(CDC_USA_clean_data),
  " USA weekly records saved to ",
  output_file
)














###############################################################################
# Compare existing DataDot USA hospitalizations with the new CDC HRD series
###############################################################################

pacman::p_load(
  here,
  readr,
  dplyr,
  tidyr,
  lubridate,
  ggplot2
)

# Import existing DataDot export
datadot_file <- here::here(
  "data",
  "data_export_NCOV_DATADOT_COVID_HOSP_ICU_WEEKLY_NATIONAL.csv"
)

if (!file.exists(datadot_file)) {
  stop("File not found: ", datadot_file)
}

datadot_data <- readr::read_csv(
  datadot_file,
  col_types = readr::cols(.default = readr::col_character()),
  na = c("", "NA", "NULL"),
  show_col_types = FALSE
)

required_columns <- c("ISO3", "ISO_WEEK_END_DATE", "COVID_NEW_HOSP")

if (!all(required_columns %in% names(datadot_data))) {
  stop(
    "Missing required DataDot columns: ",
    paste(
      setdiff(required_columns, names(datadot_data)),
      collapse = ", "
    )
  )
}

###############################################################################
# Prepare the two USA series
###############################################################################

# Existing DataDot series
datadot_usa_hosp <- datadot_data %>%
  filter(trimws(ISO3) == "USA") %>%
  transmute(
    iso_week_end_date = lubridate::ymd(ISO_WEEK_END_DATE),
    datadot_hosp = readr::parse_double(COVID_NEW_HOSP)
  )

if (nrow(datadot_usa_hosp) == 0L) {
  stop("No USA records found in the DataDot file.")
}

if (anyNA(datadot_usa_hosp$iso_week_end_date)) {
  stop("Some DataDot USA week-ending dates are missing or invalid.")
}

datadot_usa_hosp <- datadot_usa_hosp %>%
  transmute(
    epiweek = as.Date(
      lubridate::floor_date(
        iso_week_end_date,
        unit = "week",
        week_start = 1
      )
    ),
    datadot_hosp
  )

# New CDC series created by the preceding code
cdc_new_usa_hosp <- CDC_USA_clean_data %>%
  filter(ISO_3_CODE == "USA") %>%
  transmute(
    epiweek = as.Date(epiweek),
    cdc_hrd_hosp = as.numeric(new_hospitalization)
  )

if (nrow(cdc_new_usa_hosp) == 0L ||
    anyNA(cdc_new_usa_hosp$epiweek)) {
  stop("The new CDC USA series is empty or has invalid dates.")
}

# Check uniqueness before joining
if (anyDuplicated(datadot_usa_hosp$epiweek) > 0L) {
  stop("DataDot contains multiple USA records per week. Review before comparing.")
}

if (anyDuplicated(cdc_new_usa_hosp$epiweek) > 0L) {
  stop("The new CDC series contains multiple USA records per week.")
}

###############################################################################
# Align weeks and calculate differences
###############################################################################

# Include every week within the new CDC series' date range.
# Absent records remain NA, creating gaps in the graph.
usa_hosp_comparison <- tibble::tibble(
  epiweek = seq.Date(
    from = min(cdc_new_usa_hosp$epiweek),
    to = max(cdc_new_usa_hosp$epiweek),
    by = "week"
  )
) %>%
  left_join(datadot_usa_hosp, by = "epiweek") %>%
  left_join(cdc_new_usa_hosp, by = "epiweek") %>%
  mutate(
    iso_year = lubridate::isoyear(epiweek),
    iso_week_number = lubridate::isoweek(epiweek),
    
    # Positive difference means the new CDC count is higher.
    difference = cdc_hrd_hosp - datadot_hosp,
    
    percent_difference = 100 * difference /
      dplyr::na_if(datadot_hosp, 0),
    
    comparison_status = case_when(
      is.na(datadot_hosp) & is.na(cdc_hrd_hosp) ~ "Both missing",
      is.na(datadot_hosp) ~ "DataDot missing",
      is.na(cdc_hrd_hosp) ~ "CDC HRD missing",
      difference == 0 ~ "Equal",
      TRUE ~ "Different"
    )
  ) %>%
  select(
    epiweek,
    iso_year,
    iso_week_number,
    datadot_hosp,
    cdc_hrd_hosp,
    difference,
    percent_difference,
    comparison_status
  ) %>%
  arrange(epiweek)

print(
  usa_hosp_comparison %>%
    count(comparison_status, name = "number_of_weeks")
)

View(usa_hosp_comparison)

###############################################################################
# Graph both hospitalization series
###############################################################################

usa_hosp_plot_data <- usa_hosp_comparison %>%
  select(epiweek, datadot_hosp, cdc_hrd_hosp) %>%
  pivot_longer(
    cols = c(datadot_hosp, cdc_hrd_hosp),
    names_to = "source",
    values_to = "hospitalizations"
  ) %>%
  mutate(
    source = factor(
      source,
      levels = c("datadot_hosp", "cdc_hrd_hosp"),
      labels = c("Existing DataDot", "New CDC HRD")
    )
  )

usa_hosp_comparison_plot <- ggplot(
  usa_hosp_plot_data,
  aes(
    x = epiweek,
    y = hospitalizations,
    colour = source,
    linetype = source
  )
) +
  geom_line(linewidth = 0.9, na.rm = TRUE) +
  geom_point(size = 1.2, na.rm = TRUE) +
  scale_colour_manual(
    values = c(
      "Existing DataDot" = "#0072B2",
      "New CDC HRD" = "#D55E00"
    )
  ) +
  scale_linetype_manual(
    values = c(
      "Existing DataDot" = "solid",
      "New CDC HRD" = "dashed"
    )
  ) +
  scale_x_date(
    date_breaks = "3 months",
    date_labels = "%b %Y"
  ) +
  scale_y_continuous(
    labels = scales::label_comma(),
    limits = c(0, NA)
  ) +
  labs(
    title = "USA weekly COVID-19 hospital admissions",
    subtitle = "Existing DataDot export compared with the new CDC HRD series",
    x = "Week commencing Monday",
    y = "Weekly hospital admissions",
    colour = NULL,
    linetype = NULL,
    caption = paste(
      "Aligned by Monday week labels; missing values remain missing.",
      "CDC HRD counts cover Sunday–Saturday.",
      sep = "\n"
    )
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "top",
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(face = "bold")
  )

print(usa_hosp_comparison_plot)

# Save the graph alongside the input CSV
ggplot2::ggsave(
  filename = here::here("data", "usa_hosp_comparison.png"),
  plot = usa_hosp_comparison_plot,
  width = 12,
  height = 6,
  units = "in",
  dpi = 300,
  bg = "white"
)