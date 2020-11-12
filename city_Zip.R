library(jsonlite)

city_cases_zip_old_df <- read.csv("data/city_cases_zip.csv") %>% mutate(Date = as.Date(Date))
City_Cases_Zip <- fromJSON(paste0("https://services3.arcgis.com/6QuzuucBh0MLJk7u/arcgis/rest/services/City_case_mapping_",format(Sys.Date(), "%b_%d"),"/FeatureServer/0/query?f=json&where=1%3D1&returnGeometry=false&spatialRel=esriSpatialRelIntersects&outFields=*&orderByFields=RECOVERED%20desc%2CZIP%20asc&resultOffset=0&resultRecordCount=25&resultType=standard&cacheHint=true")) 
City_Cases_Zip <- City_Cases_Zip$features$attributes

City_Cases_Zip <- City_Cases_Zip %>%  mutate(Date = Sys.Date())

city_cases_zip_new <- rbind(city_cases_zip_old_df,City_Cases_Zip )
write.csv(city_cases_zip_new, "data/city_cases_zip.csv", row.names = FALSE)

