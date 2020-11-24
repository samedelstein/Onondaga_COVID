library(jsonlite)
library(lubridate)

city_case_mapping_old <- read.csv("data/city_case_mapping.csv")
City_case_mapping <- fromJSON(paste0("https://services3.arcgis.com/6QuzuucBh0MLJk7u/arcgis/rest/services/City_case_mapping_",format(Sys.Date(), "%B_%d"),"/FeatureServer/1/query?f=json&where=OBJECTID%3E187&returnGeometry=false&spatialRel=esriSpatialRelIntersects&outFields=*&resultOffset=0&resultRecordCount=4000&resultType=standard&cacheHint=true"))
City_case_mapping_df <- City_case_mapping$features$attributes

duprows <- rownames(city_case_mapping_old) %in% rownames(City_case_mapping_df)
city_case_mapping_df_new <- data.frame(rbind(City_case_mapping_df, city_case_mapping_old[!duprows,]))

city_case_mapping_df_new <- city_case_mapping_df_new %>%
  mutate(new_cases = CONFIRMED - lag(CONFIRMED,1),
         DATE = as.Date(paste0(DATE, '-', year(Sys.Date())), '%B%d-%Y'))

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

