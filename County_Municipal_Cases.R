library(tidyverse)
library(jsonlite)
county_cases_zip_old_df <- read.csv("data/County_cases_by_municipality.csv") %>% mutate(Date = as.Date(Date))
County_cases_by_municipality <- fromJSON(paste0("https://services3.arcgis.com/6QuzuucBh0MLJk7u/arcgis/rest/services/Case_mapping_by_municipality_",gsub('(\\D)0', '\\1', format(Sys.Date(), "%b_%d")),"/FeatureServer/0/query?f=json&where=1%3D1&returnGeometry=false&spatialRel=esriSpatialRelIntersects&outFields=*&orderByFields=RECOVERED%20desc%2CMUNI%20asc&outSR=102100&resultOffset=0&resultRecordCount=25&resultType=standard&cacheHint=true")) 
fromJSON("https://services3.arcgis.com/6QuzuucBh0MLJk7u/arcgis/rest/services/Case_mapping_by_municipality_December_11/FeatureServer/0/query?f=json&where=1%3D1&returnGeometry=false&spatialRel=esriSpatialRelIntersects&outFields=*&orderByFields=RECOVERED%20desc%2CMUNI%20asc&outSR=102100&resultOffset=0&resultRecordCount=25&resultType=standard&cacheHint=true")
County_cases_by_municipality <- County_cases_by_municipality$features$attributes

County_cases_by_municipality <- County_cases_by_municipality %>%  mutate(Date = Sys.Date())

County_cases_by_municipality_new <- rbind(county_cases_zip_old_df,County_cases_by_municipality )

write.csv(County_cases_by_municipality_new, "data/County_cases_by_municipality.csv", row.names = FALSE)

ggplot(filter(County_cases_by_municipality_new, MUNI == 'Syracuse'), aes(Date, RATELABEL, group = MUNI, color = MUNI)) +
  geom_line()

