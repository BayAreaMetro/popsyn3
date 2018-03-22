
#R point in polygon
#Ben Stabler, stabler@rsginc.com, 05/05/16

library(rgeos)
library(sp)
library(rgdal)

setwd("C:/projects/metrofreight/zones")

zones = readOGR("zonepolygons_zone.SHP", layer="zonepolygons_zone") #same as filename for shapefile

points = data.frame(x=c(7920659,8283506), y=c(1551007,717673))
points = SpatialPoints(points)

proj4string(points) = proj4string(zones) # use same projection 

over(points,zones)
