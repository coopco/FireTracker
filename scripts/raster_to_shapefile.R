library(rgdal)
library(raster)
vegetation <- raster("Data/vegetation.gpkg")


vegetation <- aggregate(vegetation, 8)

#writeOGR(vegetation, dsn="test", layer="test", driver="ESRI Shapefile")

polygons <- rasterToPolygons(vegetation, dissolve=T)

plot