###########################################################
# Required libraries
###########################################################
library(aire.zmvm)
library(mapview)
library(tidyverse)
library(purrr)
library(plyr)
library(tidyr)
library(reshape2)
library(dplyr)
library(raster)
library(rgeos)
library(sp) 
library(sf)
library(rgdal) 
library(viridis)
library(rvest)
library(ggspatial)
library(ggplot2)
###########################################################
# DATA
###########################################################
###########################################################
# Loading INEGI Polygons
###########################################################

polygons <- readOGR(
  dsn= '.', 
  layer = '09mun')

curvasnivel <- readOGR(
  dsn= '.', 
  layer = 'curva_nivel250_l')

# Preliminar view
# mapview::mapView(polygons) + mapView(curvasnivel)


###########################################################
# Formatting polygon data CDMX
###########################################################
#Transform coordinates to Lat lon 
polygons.lonlat <- spTransform(polygons, CRS=CRS("+proj=longlat +datum=WGS84"))

### Fortify polygons_lonlat
polygons.lonlat@data$id = rownames(polygons.lonlat@data)
polygons.points.lonlat = fortify(polygons.lonlat, region="id")
polygons.df.lonlat = join(polygons.points.lonlat, polygons.lonlat@data, by="id")
head(polygons.df.lonlat)

###########################################################
# Formatting polygon data "Elevation"
###########################################################
#Transform coordinates to Lat lon 
curvasnivel.lonlat <- spTransform(curvasnivel, 
                                  CRS=CRS("+proj=longlat +datum=WGS84"
                                          )
                      )

### Fortify polygons_lonlat
curvasnivel.lonlat@data$id = rownames(curvasnivel.lonlat@data)
curvasnivel.points.lonlat = fortify(curvasnivel.lonlat, region="id")
curvasnivel.df.lonlat = join(curvasnivel.points.lonlat, curvasnivel.lonlat@data, by="id")
head(curvasnivel.df.lonlat)

###########################################################
# Loading Monitoring stations coordinates
###########################################################

head(stations)
# > head(stations)
# station_code station_name       lon      lat altitude                    comment   station_id
# 1          ACO      Acolman -98.91200 19.63550     2198                            484150020109
# 2          AJU       Ajusco -99.16261 19.15429     2942                            484090120400
# 3          AJM Ajusco Medio -99.20774 19.27216     2548                            484090120609
# 4          ARA       Aragón -99.07455 19.47022     2200 Finalizó operación en 2010 484090050301
# 5          ATI     Atizapan -99.25413 19.57696     2341                            484150130101
# 6          AZC Azcapotzalco -99.19866 19.48773     2279 Finalizó operación en 2010 484090020201

# head(stations)
# station_code station_name altitude                    comment   station_id
# 1          ACO      Acolman     2198                            484150020109
# 2          AJU       Ajusco     2942                            484090120400
#save(stations, file = "stations_info.csv")

###########################################################
# Formatting stations data
###########################################################

#Convert stations coordinates to sf, set the CRS
#to EPSG:4326 (lat/long), and trasform to EPSG:3035

xy <- stations[,c(3,4)]
stations_sp <- SpatialPointsDataFrame(coords = xy, data = stations,
                                      proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
#class(stations_sp)
# [1] "SpatialPointsDataFrame"
# attr(,"package")
# [1] "sp"

stations_sf <- st_as_sf(stations_sp, 
                        coords = c("lon","lat"), 
                        crs = 4326) %>%
               st_transform(3050)
#class(stations_sf)
#[1] "sf"         "data.frame"

# head(stations_sf)
# Simple feature collection with 6 features and 7 fields
# geometry type:  POINT
# dimension:      XY
# bbox:           xmin: -3489317 ymin: 17357950 xmax: -3449288 ymax: 17427620
# epsg (SRID):    3050
# proj4string:    +proj=utm +zone=38 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs
# station_code station_name       lon      lat altitude                    comment   station_id                  geometry
# 1          ACO      Acolman -98.91200 19.63550     2198                            484150020109 POINT (-3489317 17357949)
# 2          AJU       Ajusco -99.16261 19.15429     2942                            484090120400 POINT (-3473596 17427621)

### <stations_sf> input for buffer generation 

###########################################################
# Create buffer around stations' coordinates
###########################################################
#Buffer circles by 3000m
stations_circles <- st_buffer(stations_sf, dist = 5000)
#class(stations_circles)
#[1] "sf"         "data.frame"


###########################################################
# Formatting buffer data
###########################################################

# Transform pt_buffer from GRS80 (m) to EPSG: 4326 +proj4string: "+proj=longlat +datum=WGS84 +no_defs"
pt_buffer_sp <- sf::as_Spatial(stations_circles)
pt_buffer_latlon <- spTransform(pt_buffer_sp, CRS=CRS("+proj=longlat +datum=WGS84"))

pt_buffer_latlon@data$id = rownames(pt_buffer_latlon@data)
pt_buffer.points.lonlat = fortify(pt_buffer_latlon, region="id")
pt_buffer.df.lonlat = join(pt_buffer.points.lonlat, pt_buffer_latlon@data, by="id")
head(pt_buffer.df.lonlat)

### pt_buffer.df.lonlat input for mapping

###########################################################
# Plotting layers
###########################################################
#Without backgroud
ggplot()+
  geom_polygon(data = polygons.df.lonlat,   
               aes(x = long,
                   y = lat, 
                   group = group,
                   fill = id)) +
  geom_point(data = pt_buffer.df.lonlat[,-13], 
             aes(x = long, y = lat))+
  geom_point(data = stations, aes(x = lon, y = lat, color = "red"))+
  geom_path(color = "white") +
  coord_sf()


###########################################################
# Buffer intersection
###########################################################
sf_combined <- st_union(stations_circles)
# class(sf_combined)
# [1] "sfc_MULTIPOLYGON" "sfc"
mapview(sf_combined)

sf_combined %>%
  leaflet() %>%
  addTiles() %>%
  addPolygons()