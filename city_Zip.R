library(jsonlite)

City_Cases_Zip <- fromJSON("https://services3.arcgis.com/6QuzuucBh0MLJk7u/arcgis/rest/services/City_case_mapping_November_9/FeatureServer/0/query?f=json&where=1%3D1&returnGeometry=false&spatialRel=esriSpatialRelIntersects&outFields=*&orderByFields=RECOVERED%20desc%2CZIP%20asc&resultOffset=0&resultRecordCount=25&resultType=standard&cacheHint=true") 
City_case_mapping_November_9_df <- City_case_mapping_November_9$features %>%
  mutate(Date = Sys.Date())
