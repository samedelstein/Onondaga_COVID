library(jsonlite)
library(data.table)
library(tidyverse)
library(ggplot2)
ny_COVID_data_CDC <- read_json('https://covid.cdc.gov/covid-data-tracker/COVIDData/getAjaxData?id=integrated_county_timeseries_state_NY_external')
ny_COVID_data_CDC_list <- ny_COVID_data_CDC$integrated_county_timeseries_external_data
lstData <- Map(as.data.frame, ny_COVID_data_CDC_list)
ny_COVID_data_CDC_df <- rbindlist(lstData) %>%
  filter(county == 'Onondaga County') %>%
  mutate(date = as.Date(date))
names(ny_COVID_data_CDC_df)
ggplot(ny_COVID_data_CDC_df) +
  geom_line(aes(date, new_cases_7_day_rolling_average, group = 1))


tail(ny_COVID_data_CDC_df)
