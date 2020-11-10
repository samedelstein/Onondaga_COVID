library(jsonlite)

County_Municipality_Cases <- fromJSON("https://services3.arcgis.com/6QuzuucBh0MLJk7u/arcgis/rest/services/Case_mapping_by_municipality_November_9/FeatureServer/0/query?f=json&where=1%3D1&returnGeometry=false&spatialRel=esriSpatialRelIntersects&outFields=*&orderByFields=RECOVERED%20desc%2CMUNI%20asc&outSR=102100&resultOffset=0&resultRecordCount=25&resultType=standard&cacheHint=true") 
County_Municipality_Cases_df <- County_Municipality_Cases$features %>%
  mutate(Date = Sys.Date())
