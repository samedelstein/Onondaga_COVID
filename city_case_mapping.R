library(jsonlite)

city_case_mapping_old <- read.csv("data/city_case_mapping.csv")
City_case_mapping <- fromJSON(paste0("https://services3.arcgis.com/6QuzuucBh0MLJk7u/arcgis/rest/services/City_case_mapping_",format(Sys.Date(), "%b_%d"),"/FeatureServer/1/query?f=json&where=OBJECTID%3E187&returnGeometry=false&spatialRel=esriSpatialRelIntersects&outFields=*&resultOffset=0&resultRecordCount=4000&resultType=standard&cacheHint=true"))
City_case_mapping_df <- City_case_mapping$features$attributes

duprows <- rownames(city_case_mapping_old) %in% rownames(City_case_mapping_df)
city_case_mapping_df_new <- data.frame(rbind(City_case_mapping_df, city_case_mapping_old[!duprows,]))

write.csv(city_case_mapping_df_new, "data/city_case_mapping.csv", row.names = FALSE)



