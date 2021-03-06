library(jsonlite)
library(lubridate)
library(tidyverse)

city_case_mapping_old <- read.csv("data/city_case_mapping.csv")
city_case_mapping_old <- city_case_mapping_old %>%
  mutate(DATE = case_when(CONFIRMED < 7785 ~ ymd(DATE) - years(1),
                   TRUE ~ ymd(DATE)))

City_case_mapping <- fromJSON(paste0("https://services3.arcgis.com/6QuzuucBh0MLJk7u/arcgis/rest/services/City_case_mapping_",gsub('(\\D)0', '\\1', format(Sys.Date(), "%b_%d")),"/FeatureServer/1/query?f=json&where=OBJECTID%3E1&returnGeometry=false&spatialRel=esriSpatialRelIntersects&outFields=*&resultOffset=0&resultRecordCount=4000&resultType=standard&cacheHint=true"))

City_case_mapping_df <- City_case_mapping$features$attributes %>%
  mutate(DATE = case_when(CONFIRMED < 7785 ~ as.Date(paste0(DATE, '-', 2020), '%B%d-%Y'),
                          TRUE ~ as.Date(paste0(DATE, '-', 2021), '%B%d-%Y')))

duprows <- rownames(city_case_mapping_old) %in% rownames(City_case_mapping_df)
city_case_mapping_df_new <- data.frame(rbind(City_case_mapping_df, city_case_mapping_old[!duprows,]))

city_case_mapping_df_new <- city_case_mapping_df_new %>%
  mutate(new_cases = CONFIRMED - lag(CONFIRMED,1))

write.csv(city_case_mapping_df_new, "data/city_case_mapping.csv", row.names = FALSE)


colors_Positives <- c(
  'Rolling.7.Day.Positives' = '#fdae61',
  'New.Daily.Positives' = '#7b3294')


city_cases_rolling <- city_case_mapping_df_new %>%
  mutate(Last.7.Days.Mean = (CONFIRMED - lag(CONFIRMED,7))/7) %>%
  ggplot() +
  geom_col(aes(DATE, new_cases, group = 1, fill='New.Daily.Positives')) +
  geom_line(aes(DATE, Last.7.Days.Mean, group = 1, color = 'Rolling.7.Day.Positives'), size = 2) +
  geom_hline(aes(yintercept = max(Last.7.Days.Mean, na.rm = TRUE)), alpha = .5, color = 'red', linetype = 'dashed') +
  labs(title = "Positive Tests in the City of Syracuse with Rolling Average",
       subtitle = paste("Data as of", max(city_case_mapping_df_new$DATE), sep = " "),
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
ggsave("/Users/samedelstein/Onondaga_COVID/visualizations/city_cases_rolling.jpg", plot = city_cases_rolling, width = 10, height = 7)

city_new_cases <- city_case_mapping_df_new %>%
  mutate(Last.7.Days.Mean = (CONFIRMED - lag(CONFIRMED,7))/7)



timeto1000_City<- city_case_mapping_df_new %>%
  mutate(by1000 = floor(CONFIRMED/1000)*1000) %>%
  group_by(by1000) %>%
  slice(which.min(DATE)) 

timeto1000_City$days <- difftime( timeto1000_City$DATE,lag(timeto1000_City$DATE,1))
timeto1000_City <- timeto1000_City %>% filter(by1000 != 0)
DaysToReach1000Cases_City <- ggplot(timeto1000_City, aes(factor(by1000), days)) +
  geom_col() +
  geom_text(
    aes(label = paste0(days, " days"), y = days + 0.1),
    position = position_dodge(0.9),
    vjust = -.5
  )+ 
  geom_text(aes(label = as.character(DATE)), color = "white", size = 3, position = position_stack(vjust = 0.5)) + 
  labs(title = "Days to Reach the Next 1,000 Cases in the City of Syracuse",
       caption = "Source: covid19.ongov.net/data",
       x = "Thousand Cases",
       y = "Number of Days",
       color = '') +
  ggthemes::theme_economist() +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 90)) 
ggsave("/Users/samedelstein/Onondaga_COVID/visualizations/DaysToReach1000Cases_City.jpg", plot = DaysToReach1000Cases_City, width = 10, height = 7)


cuts <- data.frame(Ref = c("SCSD Goes Remote", "Orange Zone Begins", "SCSD Goes In Person", "Orange Zone Ends"),
                   vals = c(as.Date('2020-12-21'),as.Date('2020-11-23'), as.Date('2021-01-19'), as.Date('2021-01-14')),
                   yvals = c(100,50,410,350),
                   stringsAsFactors = FALSE)

d=data.frame(
  xmin = as.Date('2020-09-09'),
  xmax = as.Date('2020-09-14'),
  ymin = 0,
  ymax = 250)

city_blog_data <- city_case_mapping_df_new %>%
  mutate(Last.7.Days.Mean = (CONFIRMED - lag(CONFIRMED,7))/7,
         new_cases = CONFIRMED - lag(CONFIRMED,1))

ggplot() +
  geom_vline(mapping = aes(xintercept = vals,
                           colour = Ref),
             data = cuts,
             show.legend = FALSE) +
  #geom_rect(data = cuts, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), fill = '#fb9a99', alpha = .5) +
  geom_text(mapping = aes(x = vals,
                          y = yvals,
                          label = Ref,
                          hjust = 1,
                          vjust = 0,
                          color = Ref),
            data = cuts) +
  geom_point(data = city_blog_data, aes(DATE, new_cases ),alpha = .2, size = 1) +
  # geom_point(data = SU_Cases, aes(DateRecorded, NewStudentPositives), color = "red", alpha = .2) +
  # geom_line(data = SU_Cases, aes(DateRecorded, Last.7.Days.Mean), color = "red") +
  geom_line(data = city_blog_data, aes(DATE, Last.7.Days.Mean)) +
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

