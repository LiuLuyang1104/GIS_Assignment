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

# loading data
LondonWards <- st_read('C:/Users/Louriee/Desktop/gis/assignment/Assignment/
                       London-wards-2018/London-wards-2018_ESRI/London_Ward.shp')
