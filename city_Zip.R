library(jsonlite)

city_cases_zip_old_df <- read.csv("data/city_cases_zip.csv") %>% mutate(Date = as.Date(Date))
City_Cases_Zip <- fromJSON(paste0("https://services3.arcgis.com/6QuzuucBh0MLJk7u/arcgis/rest/services/City_case_mapping_",format(Sys.Date(), "%B_%d"),"/FeatureServer/0/query?f=json&where=1%3D1&returnGeometry=false&spatialRel=esriSpatialRelIntersects&outFields=*&orderByFields=RECOVERED%20desc%2CZIP%20asc&resultOffset=0&resultRecordCount=25&resultType=standard&cacheHint=true")) 
City_Cases_Zip <- City_Cases_Zip$features$attributes

City_Cases_Zip <- City_Cases_Zip %>%  mutate(Date = Sys.Date())

city_cases_zip_new <- rbind(city_cases_zip_old_df,City_Cases_Zip )
write.csv(city_cases_zip_new, "data/city_cases_zip.csv", row.names = FALSE)

city_cases_zip_new %>%
  group_by(ZIP) %>%
  mutate(new_cases = CONFIRMED - lag(CONFIRMED,1)) %>%
  ggplot(aes(Date, CONFIRMED, color = factor(ZIP))) +
  geom_line()
 
city_cases_zip_new %>%
  filter(ZIP == 13210) %>%
  mutate(active = CONFIRMED-RECOVERED) %>%
  ggplot(aes(Date, CONFIRMED)) +
  geom_col()

SU13210 <- city_cases_zip_new %>%
  group_by(ZIP) %>%
  mutate(new_cases = CONFIRMED - lag(CONFIRMED,1)) %>%
  filter(ZIP == 13210) %>%
  merge(SU_Cases, by.x = 'Date', by.y = 'DateRecorded') 

colors_Zip_SU <- c(
  'SU' = 'orange',
  '13210' = 'blue')

  ggplot(SU13210) +
  geom_line(aes(Date, NewStudentPositives, color = "SU")) +
  geom_line(aes(Date, new_cases , color = "13210")) +
    scale_x_date(date_breaks = "1 week", date_labels = "%m/%d") +
    scale_color_manual(values = colors_Zip_SU) +
    scale_y_continuous(expand = c(0, 0), limits = c(0, NA)) +
    labs(title = "Syracuse University Cases and Cases in 13210 Zip Code",
         caption = "Source: https://covid19.ongov.net/data/",
         x = "",
         y = "Confirmed Cases",
         color = '') +
    ggthemes::theme_economist() +
    theme(legend.position = "none",
          axis.text.x = element_text(angle = 90)) 
