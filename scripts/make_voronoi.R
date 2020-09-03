library(leaflet)
library(rgdal)
source("scripts/voronoi-shapefile.R")

stations <- read.csv("Datasets/acorn_sat_stations.csv")
states = readOGR("Datasets/simple_states_2/simple_states.shp")

shapes <- voronoiShapefile(stations$long, stations$lat, states)
shapes$stationName <- stations$name
shapes$stationNum <- stations$stnnum

writeOGR(shapes, dsn="voronoi", layer="voronoi", driver="ESRI Shapefile")
