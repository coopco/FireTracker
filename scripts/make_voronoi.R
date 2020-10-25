### Makes the shapefile for use in the risk map
### Station data retrieved from the ACORN_SAT dataset from the Bureau of Meteorology

library(leaflet)
library(rgdal)
source("scripts/voronoi-shapefile.R")

# Read station data and Australia shapefile
stations <- read.csv("Datasets/acorn_sat_stations.csv")
states <- readOGR("Datasets/simple_states_2/simple_states.shp")

# Compute shapefile
shapes <- voronoiShapefile(stations$long, stations$lat, states)
shapes$stationName <- stations$name
shapes$stationNum <- stations$stnnum

# Write to file
writeOGR(shapes, dsn="voronoi", layer="voronoi", driver="ESRI Shapefile")
