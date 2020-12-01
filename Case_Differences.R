library(tidyverse)
library(jsonlite)
county_data <- read.csv("https://raw.githubusercontent.com/samedelstein/Onondaga_COVID/master/data/county_case_mapping.csv") %>%
  mutate(new_cases = CONFIRMED - lag(CONFIRMED,1),
         Last.7.Days.Mean_County = zoo::rollmean(new_cases, k = 7, fill = NA, align = "right"),
         DATE = as.Date(DATE))

#State Data
state_data <- read.csv("https://health.data.ny.gov/api/views/xdss-u53e/rows.csv?accessType=DOWNLOAD", stringsAsFactors = FALSE) %>%
  filter(County == "Onondaga") %>%
  mutate(Test.Date = as.Date(Test.Date, "%m/%d/%Y"),
         rollmean = zoo::rollmean(New.Positives, k = 7, fill = NA, align = "right"))


#CDC Data
ny_COVID_data_CDC <- read_json('https://covid.cdc.gov/covid-data-tracker/COVIDData/getAjaxData?id=integrated_county_timeseries_state_NY_external')
ny_COVID_data_CDC_list <- ny_COVID_data_CDC$integrated_county_timeseries_external_data
lstData <- Map(as.data.frame, ny_COVID_data_CDC_list)
CDC_data  <- rbindlist(lstData) %>%
  filter(county == 'Onondaga County') %>%
  mutate(date = as.Date(date)) %>% arrange(date)


state_cdc <- merge(CDC_data, state_data, by.x = "date", by.y = "Test.Date")
data_df <- merge(state_cdc, county_data, by.x = 'date', by.y = 'DATE')


colors_cases <- c(
  'CDC Data Source' = '#1b9e77',
  'NYS Data Source' = '#d95f02',
  'Onondaga County Data Source' = '#7570b3')
CDC_NYS_County_vis <- data_df %>%
  ggplot() +
  geom_line(aes(date, new_cases_7_day_rolling_average, group = 1, color = 'CDC Data Source'),size = 2, alpha = .5,) +
  geom_line(aes(date, rollmean, group = 1, color = 'NYS Data Source'),size = 2, alpha = .5,) +
  geom_line(aes(date, Last.7.Days.Mean_County, group = 1, color = 'Onondaga County Data Source'),size = 2, alpha = .5) +
  #geom_line(aes(Test.Date, Last.7.Days.Mean, group = 1), color = 'black') +
  labs(title = "Positive COVID-19 Cases in Onondaga County",
       subtitle = paste("Data as of", max(x$Test.Date), sep = " "),
       caption = "Source: data.ny.gov, covid19.ongov.net/data, covid.cdc.gov/covid-data-tracker/",
       x = "",
       y = "Confirmed Cases",
       color = '') +
  scale_color_manual(values = colors_cases) +
  scale_fill_manual(values = colors_cases) +
  scale_x_date(date_breaks = "1 week", date_labels = "%m/%d") +
  ggthemes::theme_economist() +
  theme(axis.text.x = element_text(angle = 90))+
  theme(legend.title = element_blank())
ggsave("visualizations/CDC_NYS_County_vis.jpg", plot = CDC_NYS_County_vis, width = 10, height = 7)



#mean difference in cases reported
data_df %>%
  select(date, new_cases_7_day_rolling_average,rollmean,Last.7.Days.Mean_County) %>%
  mutate(CDC_NYS_Difference = abs(new_cases_7_day_rolling_average - rollmean),
         CDC_County_Difference = abs(new_cases_7_day_rolling_average - Last.7.Days.Mean_County),
         NYS_County_Difference = abs(rollmean - Last.7.Days.Mean_County)) %>%
  summarise(mean(CDC_NYS_Difference, na.rm = TRUE),
            mean(CDC_County_Difference, na.rm = TRUE),
            mean(NYS_County_Difference, na.rm = TRUE))


colors_source <- c(
  'CDC' = '#1b9e77',
  'NYS' = '#d95f02',
  'County' = '#7570b3')
#CDC & NYS differences
CDC_NYS_Difference_viz <- data_df %>%
  select(date, new_cases_7_day_rolling_average,rollmean,Last.7.Days.Mean_County) %>%
  mutate(CDC_NYS_Difference = new_cases_7_day_rolling_average - rollmean,
         CDC_County_Difference = new_cases_7_day_rolling_average - Last.7.Days.Mean_County,
         NYS_County_Difference = rollmean - Last.7.Days.Mean_County,
         CDC_NYS_source = case_when(CDC_NYS_Difference > 0 ~ 'CDC',
                                    TRUE ~ 'NYS'),
         CDC_County_source = case_when(CDC_County_Difference > 0 ~ 'CDC',
                                    TRUE ~ 'County'),
         NYS_County_source = case_when(NYS_County_Difference > 0 ~ 'NYS',
                                    TRUE ~ 'County')) %>%
  ggplot(aes(date, CDC_NYS_Difference, fill = CDC_NYS_source)) +
  geom_col() +
  labs(title = "Difference in Positive Reported Cases CDC and NYS",
       subtitle = 'Based on Rolling 7-day Average',
                  caption = "Source: data.ny.gov, covid19.ongov.net/data, covid.cdc.gov/covid-data-tracker",
                  x = "",
                  y = "Positive Case Differences",
                  color = '') +
  scale_x_date(date_breaks = "1 week", date_labels = "%m/%d") +
  scale_fill_manual(values=colors_source)+
  ggthemes::theme_economist() +
  theme(axis.text.x = element_text(angle = 90))+
  theme(legend.title = element_blank())
ggsave("visualizations/CDC_NYS_Difference_viz.jpg", plot = CDC_NYS_Difference_viz, width = 10, height = 7)

#CDC & County differences

CDC_County_Difference_viz <- data_df %>%
  select(date, new_cases_7_day_rolling_average,rollmean,Last.7.Days.Mean_County) %>%
  mutate(CDC_NYS_Difference = new_cases_7_day_rolling_average - rollmean,
         CDC_County_Difference = new_cases_7_day_rolling_average - Last.7.Days.Mean_County,
         NYS_County_Difference = rollmean - Last.7.Days.Mean_County,
         CDC_NYS_source = case_when(CDC_NYS_Difference > 0 ~ 'CDC',
                                    TRUE ~ 'NYS'),
         CDC_County_source = case_when(CDC_County_Difference > 0 ~ 'CDC',
                                       TRUE ~ 'County'),
         NYS_County_source = case_when(NYS_County_Difference > 0 ~ 'NYS',
                                       TRUE ~ 'County')) %>%
  ggplot(aes(date, CDC_County_Difference, fill = CDC_County_source)) +
  geom_col() +
  labs(title = "Difference in Positive Reported Cases CDC and Onondaga County",
       subtitle = 'Based on Rolling 7-day Average',
       caption = "Source: data.ny.gov, covid19.ongov.net/data, covid.cdc.gov/covid-data-tracker",
       x = "",
       y = "Positive Case Differences",
       color = '') +
  scale_x_date(date_breaks = "1 week", date_labels = "%m/%d") +
  scale_fill_manual(values=colors_source)+
  ggthemes::theme_economist() +
  theme(axis.text.x = element_text(angle = 90))+
  theme(legend.title = element_blank())
ggsave("visualizations/CDC_County_Difference_viz.jpg", plot = CDC_County_Difference_viz, width = 10, height = 7)


#NYS & County differences

NYS_County_Difference_viz <- data_df %>%
  select(date, new_cases_7_day_rolling_average,rollmean,Last.7.Days.Mean_County) %>%
  mutate(CDC_NYS_Difference = new_cases_7_day_rolling_average - rollmean,
         CDC_County_Difference = new_cases_7_day_rolling_average - Last.7.Days.Mean_County,
         NYS_County_Difference = rollmean - Last.7.Days.Mean_County,
         CDC_NYS_source = case_when(CDC_NYS_Difference > 0 ~ 'CDC',
                                    TRUE ~ 'NYS'),
         CDC_County_source = case_when(CDC_County_Difference > 0 ~ 'CDC',
                                       TRUE ~ 'County'),
         NYS_County_source = case_when(NYS_County_Difference > 0 ~ 'NYS',
                                       TRUE ~ 'County')) %>%
  ggplot(aes(date, NYS_County_Difference, fill = NYS_County_source)) +
  geom_col() +
  labs(title = "Difference in Positive Reported Cases NYS and Onondaga County",
       subtitle = 'Based on Rolling 7-day Average',
       caption = "Source: data.ny.gov, covid19.ongov.net/data, covid.cdc.gov/covid-data-tracker",
       x = "",
       y = "Positive Case Differences",
       color = '') +
  scale_x_date(date_breaks = "1 week", date_labels = "%m/%d") +
  scale_fill_manual(values=colors_source)+
  ggthemes::theme_economist() +
  theme(axis.text.x = element_text(angle = 90))+
  theme(legend.title = element_blank())
ggsave("visualizations/NYS_County_Difference_viz.jpg", plot = NYS_County_Difference_viz, width = 10, height = 7)

