library(jsonlite)

Case_Count <- fromJSON("https://services3.arcgis.com/6QuzuucBh0MLJk7u/arcgis/rest/services/Case_mapping_by_municipality_November_9/FeatureServer/1/query?f=json&where=1%3D1&returnGeometry=false&spatialRel=esriSpatialRelIntersects&outFields=*&outSR=102100&resultOffset=0&resultRecordCount=4000&resultType=standard&cacheHint=true") 
Case_Count_df <- Case_Count$features 
