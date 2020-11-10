library(jsonlite)

City_case_mapping_November_9 <- fromJSON("https://services3.arcgis.com/6QuzuucBh0MLJk7u/arcgis/rest/services/City_case_mapping_November_9/FeatureServer/1/query?f=json&where=OBJECTID%3E187&returnGeometry=false&spatialRel=esriSpatialRelIntersects&outFields=*&resultOffset=0&resultRecordCount=4000&resultType=standard&cacheHint=true")
City_case_mapping_November_9_df <- City_case_mapping_November_9$features
