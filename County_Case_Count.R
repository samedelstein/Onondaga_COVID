library(lubridate)
library(jsonlite)
library(RCurl)
library(ggrepel)
library(tidyverse)


county_case_mapping_old <- read.csv("data/county_case_mapping.csv",stringsAsFactors = FALSE)



county_case_mapping <- fromJSON(paste0("https://services3.arcgis.com/6QuzuucBh0MLJk7u/arcgis/rest/services/Case_mapping_by_municipality_",gsub('(\\D)0', '\\1', format(Sys.Date(), "%B_%d")),"/FeatureServer/1/query?f=json&where=1%3D1&returnGeometry=false&spatialRel=esriSpatialRelIntersects&outFields=*&outSR=102100&resultOffset=0&resultRecordCount=4000&resultType=standard&cacheHint=true")) 
county_case_mapping_df <- county_case_mapping$features$attributes

duprows <- rownames(county_case_mapping_old) %in% rownames(county_case_mapping_df)
county_case_mapping_df_new <- data.frame(rbind(county_case_mapping_df, county_case_mapping_old[!duprows,]))
county_case_mapping_df_new <- county_case_mapping_df_new %>% 
  mutate(new_cases = CONFIRMED - lag(CONFIRMED,1),
         new_deaths = DEATHS - lag(DEATHS,1),
         DATE = as.Date(paste0(county_case_mapping_df_new$DATE, '-', year(Sys.Date())), '%B%d-%Y'))
write.csv(county_case_mapping_df_new, "data/county_case_mapping.csv", row.names = FALSE)

county_case_mapping_df_new %>%
  mutate(month = month(DATE),
         new_deaths = DEATHS - lag(DEATHS,1)) %>%
  group_by(month) %>%
  summarise(sum_deaths = sum(new_deaths, na.rm = TRUE),
            sum_cases = sum(new_cases, na.rm = TRUE))
county_case_mapping_df_new %>%
  select(DATE, new_deaths) %>%
  arrange(new_deaths)
  

colors_Positives <- c(
  'Rolling.7.Day.Positives' = '#fdae61',
  'New.Daily.Positives' = '#7b3294')

county_case_mapping_viz <- county_case_mapping_df_new %>%
  mutate(Last.7.Days.Mean = (CONFIRMED - lag(CONFIRMED,7))/7) %>%
  ggplot() +
  geom_col(aes(DATE, new_cases, group = 1, fill='New.Daily.Positives')) +
  geom_line(aes(DATE, Last.7.Days.Mean, group = 1, color = 'Rolling.7.Day.Positives'), size = 2) +
  geom_hline(aes(yintercept = max(Last.7.Days.Mean, na.rm = TRUE)), alpha = .5, color = 'red', linetype = 'dashed') +
  #geom_vline(mapping = aes(xintercept = as.Date('2020-10-31')), linetype = 'dashed') +
  labs(title = "Positive Tests in Onondaga County with Rolling Average",
       subtitle = paste("Data as of", max(county_case_mapping_df_new$DATE), sep = " "),
       caption = "Source: covid19.ongov.net/data",
       x = "",
       y = "Confirmed Cases",
       color = '') +
  scale_x_date(date_breaks = "1 week", date_labels = "%m/%d") +
  scale_color_manual(values = colors_Positives) +
  scale_fill_manual(values = colors_Positives) +
  ggthemes::theme_economist() +
  theme(axis.text.x = element_text(angle = 90)) +
  theme(legend.title = element_blank())
ggsave("/Users/samedelstein/Onondaga_COVID/visualizations/county_case_mapping_viz.jpg", plot = county_case_mapping_viz, width = 10, height = 7)

county_case_mapping_df_new %>%
  mutate(more_than_city = case_when(new_cases > 123 ~ 1,
                   TRUE ~ 0)) %>%
  ggplot() +
  geom_col(aes(DATE, new_cases, group = 1, fill=factor(more_than_city))) +
  labs(title = "Positive Tests in Onondaga County with Rolling Average",
       subtitle = paste("Data as of", max(county_case_mapping_df_new$DATE), sep = " "),
       caption = "Source: covid19.ongov.net/data",
       x = "",
       y = "Confirmed Cases",
       color = '') +
  scale_x_date(date_breaks = "1 week", date_labels = "%m/%d") +
  scale_fill_manual(values = c('black', 'red'), labels= c('Less than City Cases on 12/3', "More than City Cases on 12/3")) +
  ggthemes::theme_economist() +
  theme(axis.text.x = element_text(angle = 90)) +
  theme(legend.title = element_blank())
  

timeto1000_County <- county_case_mapping_df_new %>%
  mutate(by1000 = floor(CONFIRMED/1000)*1000) %>%
  group_by(by1000) %>%
  slice(which.min(DATE)) %>%
  select(DATE, by1000)
county_case_mapping_df_new %>% filter(CONFIRMED > 6243.5)
timeto1000_County$days <- difftime( timeto1000_County$DATE,lag(timeto1000_County$DATE,1))
timeto1000_County <- timeto1000_County %>% filter(by1000 != 0)
DaysToReach1000Cases_County <- ggplot(timeto1000_County, aes(factor(by1000), days)) +
  geom_col() +
  geom_text(
    aes(label = paste0(days, " days"), y = days + 0.1),
    position = position_dodge(0.9),
    vjust = -.5
  )+ 
  geom_text(aes(label = as.character(DATE)), color = "white", size = 3, position = position_stack(vjust = 0.5)) + 
  labs(title = "Days to Reach the Next 1,000 Cases in Onondaga County",
       caption = "Source: covid19.ongov.net/data",
       x = "Thousand Cases",
       y = "Number of Days",
       color = '') +
  ggthemes::theme_economist() +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 90)) 
ggsave("/Users/samedelstein/Onondaga_COVID/visualizations/DaysToReach1000Cases_County.jpg", plot = DaysToReach1000Cases_County, width = 10, height = 7)




cuts <- data.frame(Ref = c("SU Students Return", "County In-Person &\n SCSD Remote Schooling \nStart", "SCSD Hybrid \nLearning Starts", "Halloween", 'Thanksgiving'),
                   vals = c(as.Date('2020-08-17'),as.Date('2020-09-14'), as.Date('2020-10-05'), as.Date('2020-10-31'), as.Date('2020-11-26')),
                   yvals = c(100,50,100,200,220),
                   xmin = as.Date('2020-09-09'),
                   xmax = as.Date('2020-09-14'),
                   ymin = 0,
                   ymax = 250,
                   stringsAsFactors = FALSE)

d=data.frame(
  xmin = as.Date('2020-09-09'),
  xmax = as.Date('2020-09-14'),
  ymin = 0,
  ymax = 250)

county_blog_data <- county_case_mapping_df_new %>%
  mutate(Last.7.Days.Mean = (CONFIRMED - lag(CONFIRMED,7))/7)

ggplot() +
  geom_vline(mapping = aes(xintercept = vals,
                           colour = Ref),
             data = cuts,
             show.legend = FALSE) +
  geom_rect(data = cuts, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), fill = '#fb9a99', alpha = .5) +
  geom_text(mapping = aes(x = vals,
                          y = yvals,
                          label = Ref,
                          hjust = 1,
                          vjust = 0,
                          color = Ref),
            data = cuts) +
  geom_point(data = county_blog_data, aes(DATE, new_cases ),alpha = .2, size = 1) +
  geom_point(data = SU_Cases, aes(DateRecorded, NewStudentPositives), color = "red", alpha = .2) +
  geom_line(data = SU_Cases, aes(DateRecorded, Last.7.Days.Mean), color = "red") +
  geom_line(data = county_blog_data, aes(DATE, Last.7.Days.Mean)) +
  scale_x_date(date_breaks = "1 week", date_labels = "%m/%d") +
  labs(title = "New Cases in Onondaga County: County Data",
       subtitle = "Key Dates Highlighted",
       caption = "Source: covid19.ongov.net/data",
       x = "",
       y = "Confirmed Cases",
       color = '') +
  ggthemes::theme_economist() +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 90))




new_cases_key_dates_CountyData <- ggplot(county_case_mapping_df_new, aes(DATE, new_cases )) +
  geom_vline(mapping = aes(xintercept = vals,
                           colour = Ref),
             data = cuts,
             show.legend = FALSE) +
  geom_text(mapping = aes(x = vals,
                          y = yvals,
                          label = Ref,
                          hjust = 1,
                          vjust = 0,
                          color = Ref),
            data = cuts) +
  geom_point() +
  geom_point(data = SU_Cases, aes(DateRecorded, NewStudentPositives), color = "red") +
  scale_x_date(date_breaks = "1 week", date_labels = "%m/%d") +
  labs(title = "New Cases in Onondaga County: County Data",
       subtitle = "Key Dates Highlighted",
       caption = "Source: covid19.ongov.net/data",
       x = "",
       y = "Confirmed Cases",
       color = '') +
  ggthemes::theme_economist() +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 90)) 
ggsave("/Users/samedelstein/Onondaga_COVID/visualizations/new_cases_key_dates_CountyData.jpg", plot = new_cases_key_dates_CountyData, width = 10, height = 7)


cases_by_week_county <- county_case_mapping_df_new %>%
  mutate(week_number = epiweek(DATE)) %>%
  group_by(week_number) %>%
  summarize(sum_cases = sum(new_cases, na.rm = TRUE)) %>%
  mutate(pct_change = (sum_cases/lag(sum_cases) - 1) * 100)

cases_by_week_county_viz <- ggplot(cases_by_week_county, aes(week_number, sum_cases)) +
  geom_col(fill = "steelblue") +  
  geom_text(
    aes(label = sum_cases),
    position = position_dodge(0.9),
    vjust = -.5
  ) +
  #scale_x_date(date_breaks = "1 week", date_labels = "%m/%d") +
  labs(title = "Cases per week (Sunday - Saturday): County Data",
       caption = "Source: covid19.ongov.net/data",
       x = "",
       y = "Confirmed Cases",
       color = '') +
  ggthemes::theme_economist() +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 90)) 
ggsave("/Users/samedelstein/Onondaga_COVID/visualizations/cases_by_week_county_viz.jpg", plot = cases_by_week_county_viz, width = 10, height = 7)

