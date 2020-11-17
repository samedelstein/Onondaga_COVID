library(lubridate)
library(jsonlite)
library(RCurl)
county_case_mapping_old <- read.csv("data/county_case_mapping.csv")



county_case_mapping <- fromJSON(paste0("https://services3.arcgis.com/6QuzuucBh0MLJk7u/arcgis/rest/services/Case_mapping_by_municipality_",format(Sys.Date(), "%B_%d"),"/FeatureServer/1/query?f=json&where=1%3D1&returnGeometry=false&spatialRel=esriSpatialRelIntersects&outFields=*&outSR=102100&resultOffset=0&resultRecordCount=4000&resultType=standard&cacheHint=true")) 
county_case_mapping_df <- county_case_mapping$features$attributes

duprows <- rownames(county_case_mapping_old) %in% rownames(county_case_mapping_df)
county_case_mapping_df_new <- data.frame(rbind(county_case_mapping_df, county_case_mapping_old[!duprows,]))
county_case_mapping_df_new <- county_case_mapping_df_new %>% 
  mutate(new_cases = CONFIRMED - lag(CONFIRMED,1),
         DATE = as.Date(paste0(county_case_mapping_df_new$DATE, '-', year(Sys.Date())), '%B%d-%Y'))
write.csv(county_case_mapping_df_new, "data/county_case_mapping.csv", row.names = FALSE)




colors_Positives <- c(
  'Rolling.7.Day.Positives' = '#fdae61',
  'New.Daily.Positives' = '#7b3294')


county_case_mapping_df_new %>%
  mutate(Last.7.Days.Mean = (CONFIRMED - lag(CONFIRMED,7))/7) %>%
  ggplot() +
  geom_col(aes(DATE, new_cases, group = 1, fill='New.Daily.Positives')) +
  geom_line(aes(DATE, Last.7.Days.Mean, group = 1, color = 'Rolling.7.Day.Positives'), size = 2) +
  geom_hline(aes(yintercept = max(Last.7.Days.Mean, na.rm = TRUE)), alpha = .5, color = 'red', linetype = 'dashed') +
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
  