library(lubridate)
library(jsonlite)
library(RCurl)
county_case_mapping_old <- read.csv("data/county_case_mapping.csv")



county_case_mapping <- fromJSON(paste0("https://services3.arcgis.com/6QuzuucBh0MLJk7u/arcgis/rest/services/Case_mapping_by_municipality_",format(Sys.Date(), "%b_%d"),"/FeatureServer/1/query?f=json&where=1%3D1&returnGeometry=false&spatialRel=esriSpatialRelIntersects&outFields=*&outSR=102100&resultOffset=0&resultRecordCount=4000&resultType=standard&cacheHint=true")) 
county_case_mapping_df <- county_case_mapping$features$attributes

duprows <- rownames(county_case_mapping_old) %in% rownames(county_case_mapping_df)
county_case_mapping_df_new <- data.frame(rbind(county_case_mapping_df, county_case_mapping_old[!duprows,]))
county_case_mapping_df_new <- county_case_mapping_df_new %>% 
  mutate(new_cases = CONFIRMED - lag(CONFIRMED,1),
         DATE = as.Date(paste0(county_case_mapping_df_new$DATE, '-', year(Sys.Date())), '%B%d-%Y'))
write.csv(county_case_mapping_df_new, "data/county_case_mapping.csv", row.names = FALSE)


  ggplot(county_case_mapping_df_new, aes(DATE, new_cases)) +
    geom_col()
  