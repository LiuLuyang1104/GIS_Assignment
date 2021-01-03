#first library a few packages that we will use during the practical
#note you may need to install them first...
library(spatstat)
library(sp)
library(rgeos)
library(maptools)
library(GISTools)
library(tmap)
library(sf)
library(geojson)
library(geojsonio)
library(tmaptools)
library(dplyr)
library(readr)

# loading data
LondonWards <- st_read('C:/Users/Louriee/Desktop/gis/assignment/Assignment/
                       London-wards-2018/London-wards-2018_ESRI/London_Ward.shp')
LondonWardsMerged <- st_read("C:/Users/Louriee/Desktop/gis/assignment/Assignment/
                             London-wards-2018/London-wards-2018_ESRI/
                             London_Ward_CityMerged.shp")%>%
  st_transform(.,27700)
library(readr)
obesity <- read_csv("obesity.csv")
LondonWardsMerged <- LondonWardsMerged %>%
  left_join(WardData,
            by = c("GSS_CODE" = "new_code"))%>%
  distinct(GSS_CODE, ward_name, average_gcse_capped_point_scores_2014)
