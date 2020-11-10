library(jsonlite)

Syracuse_COVID <- fromJSON("https://services3.arcgis.com/6QuzuucBh0MLJk7u/arcgis/rest/services/City_case_mapping_November_9/FeatureServer/1/query?f=json&where=OBJECTID%3E187&returnGeometry=false&spatialRel=esriSpatialRelIntersects&outFields=*&outSR=102100&resultOffset=0&resultRecordCount=4000&resultType=standard&cacheHint=true")
Syracuse_COVID_df <- Syracuse_COVID$features
