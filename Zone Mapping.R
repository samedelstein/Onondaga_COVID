library(leaflet)
library(rgdal)
library(raster)
library(rgeos)
library(htmlwidgets)

Assembly_districts <- rgdal::readOGR("C:/Users/SamEdelstein/Onondaga_COVID/hotspot data/state_assembly.geojson")
Congressional_districts <- rgdal::readOGR("C:/Users/SamEdelstein/Onondaga_COVID/hotspot data/congessional_districts.geojson")
Senate_districts <- rgdal::readOGR("C:/Users/SamEdelstein/Onondaga_COVID/hotspot data/senate_districts.geojson")
Syracuse_Wards <- rgdal::readOGR(dsn = "C:/Users/SamEdelstein/Onondaga_COVID/hotspot data/City_of_Syracuse_Wards-shp", layer = 'City_of_Syracuse_Wards')
Syracuse_Census_Tracts <- rgdal::readOGR(dsn = "C:/Users/SamEdelstein/Onondaga_COVID/hotspot data/Census_Tracts_in_Syracuse%2C_NY__2010_-shp", layer = 'Census_Tracts_in_Syracuse%2C_NY__2010_')
head(Syracuse_Census_Tracts)

#hotspots
hotspots <- rgdal::readOGR("C:/Users/SamEdelstein/Onondaga_COVID/hotspot data/HotSpots_Zone.geojson")

#schools
schools <- rgdal::readOGR("C:/Users/SamEdelstein/Onondaga_COVID/hotspot data/schools.geojson")

#WIC
WIC <- read.csv('C:/Users/SamEdelstein/Onondaga_COVID/hotspot data/Women__Infants__Children__WIC__Vendor_Information.csv')
WIC$Location <- gsub(".*\\(","",WIC$Location)
WIC$Location <- gsub("\\).*","",WIC$Location)
WIC$Lat <- as.numeric(gsub(",.*","",WIC$Location))
WIC$Lon <- as.numeric(gsub(".*,","",WIC$Location))

#Liquor Licenses
liquor_licenses <- rgdal::readOGR("C:/Users/SamEdelstein/Onondaga_COVID/hotspot data/Liquor Authority Current List of Active Licenses.geojson")
liquor_licenses <- liquor_licenses[liquor_licenses$premise_state == 'NY',]
types <- "RESTAURANT|CLUB|HOTEL|EATING|LUNCHEON|ON-PREMISES|TAVERN"
liquor_licenses <- liquor_licenses[grepl(types,liquor_licenses$method_of_operation),]

#Barbers
Barbers <- read.csv('C:/Users/SamEdelstein/Onondaga_COVID/hotspot data/Active_Appearance_Enhancement_and_Barber_Business_and_Area_Renter_Licensees.csv')
Barbers$GEOCODING.INFORMATION.FROM.ADDRESS.COMPONENTS <- gsub(".*\\(","",Barbers$GEOCODING.INFORMATION.FROM.ADDRESS.COMPONENTS)
Barbers$GEOCODING.INFORMATION.FROM.ADDRESS.COMPONENTS<- gsub("\\).*","",Barbers$GEOCODING.INFORMATION.FROM.ADDRESS.COMPONENTS)
Barbers$Lat <- as.numeric(gsub(",.*","",Barbers$GEOCODING.INFORMATION.FROM.ADDRESS.COMPONENTS))
Barbers$Lon <- as.numeric(gsub(".*,","",Barbers$GEOCODING.INFORMATION.FROM.ADDRESS.COMPONENTS))

#Retail Food
RetailFood <- read.csv('C:/Users/SamEdelstein/Onondaga_COVID/hotspot data/Retail_Food_Stores.csv')
RetailFood$Location <- gsub(".*\\(","",RetailFood$Location)
RetailFood$Location<- gsub("\\).*","",RetailFood$Location)
RetailFood$Lat <- as.numeric(gsub(",.*","",RetailFood$Location))
RetailFood$Lon <- as.numeric(gsub(".*,","",RetailFood$Location))



pal <- colorFactor(
  palette = c('orange', 'yellow'),
  domain = hotspots$Zone
)

services_hotspots <- leaflet() %>%
  setView(lng = -76, lat = 43, zoom = 10) %>%
  addTiles() %>%
  addPolygons(stroke = FALSE, smoothFactor = 0.3,
              fillColor = ~pal(Zone), fillOpacity = .5,
              label = ~paste0(Cluster, ", ", Zone), data = hotspots) %>% 
  addPolygons(stroke = TRUE, smoothFactor = 0.5, weight = 1,
              color = "black", opacity = 1,
              fillColor = "transparent",
              data = Congressional_districts, group = 'Congressional districts',
              label = ~paste0(DISTRICT, ', ', REP_NAME )) %>%  
  addPolygons(stroke = TRUE, smoothFactor = 0.5, weight = 1,
              color = "black", opacity = 1,
              fillColor = "transparent",
              data = Assembly_districts, group = 'Assembly Districts',
              label = ~paste0(DISTRICT, ', ', REP_NAME )) %>%  
  addPolygons(stroke = TRUE, smoothFactor = 0.5, weight = 1,
              color = "black", opacity = 1,
              fillColor = "transparent",
              data = Senate_districts, group = 'Senate districts',
              label = ~paste0(DISTRICT, ', ', REP_NAME )) %>%  
  addPolygons(stroke = TRUE, smoothFactor = 0.5, weight = 1,
              color = "black", opacity = 1,
              fillColor = "transparent",
              data = Syracuse_Wards, group = 'Syracuse Wards',
              label = ~paste0(CITY_WARD)) %>%  
  addPolygons(stroke = TRUE, smoothFactor = 0.5, weight = 1,
              color = "black", opacity = 1,
              fillColor = "transparent",
              data = Syracuse_Census_Tracts, group = 'Syracuse Census Tracts',
              label = ~paste0(NAMELSAD10)) %>%
  addCircleMarkers(data = schools, lng = ~GDTLONG, lat = ~GDTLAT, opacity = 1, fillOpacity = 1, radius = 4, color = '#cab2d6', weight = 1, label = ~NAME, group = 'Public Schools') %>%
  addCircleMarkers(data = liquor_licenses, weight = 1, opacity = 1, fillOpacity = 1, radius = 4,color = '#1f78b4',label = ~paste0(premise_name, ', ', premise_city, '\n', method_of_operation), group = 'Restaurant/Bar') %>%
  addCircleMarkers(data = WIC, lat = ~Lat, lng = ~Lon, opacity = 1, fillOpacity = 1, radius = 4, weight = 1,color = '#b2df8a',label = ~paste0(Facility.Name, ', ', City, '\n', Type), group = 'WIC Vendor Listing') %>%
  addCircleMarkers(data = Barbers, lat = ~Lat, lng = ~Lon, opacity = 1, fillOpacity = 1, radius = 4,  weight = 1,color = '#33a02c',label = ~paste0(LICENSE.HOLDER.NAME, ', ', BUSINESS.CITY, '\n', LICENSE.TYPE), group = 'Barber/Hair Salon') %>%
  addCircleMarkers(data = RetailFood, lat = ~Lat, lng = ~Lon, opacity = 1, fillOpacity = 1, radius = 4, weight = 1,color = '#fb9a99',label = ~paste0(Entity.Name, ', ', City, '\n', Operation.Type), group = 'Retail Food') %>%
  addLayersControl(baseGroups = c('Assembly Districts','Congressional districts', 'Senate districts', 'Syracuse Wards', 'Syracuse Census Tracts'),
    overlayGroups = c("Public Schools", 'Restaurant/Bar', 'WIC Vendor Listing','Barber/Hair Salon', 'Retail Food'),
    options = layersControlOptions(collapsed = FALSE)
  ) %>% 
  hideGroup(c("Public Schools", 'Restaurant/Bar', 'WIC Vendor Listing','Barber/Hair Salon', 'Retail Food'))
saveWidget(services_hotspots, file="C:/Users/SamEdelstein/Onondaga_COVID/hotspot data/services_hotspots.html")
