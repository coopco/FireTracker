# Must convert each station cell to its own shapefile for use in voronoiShapefile
library(raster)

vegetation <- raster("Data/vegetation.gpkg")
stations <- read.csv("Data/acorn_sat_stations.csv")

# Bounds from https://gist.github.com/graydon/11198540
minLat <- -43.6345972634
maxLat <- -10.6681857235
minLon <- 113.338953078
maxLon <- 153.569469029

n <- 20000
lons <- c()
lats <- c()
vegs <- c()

# Could make this faster
# Takes very long time to run as is
for (i in 1:n) {
    if (i%%100 == 0) print(i)
    veg <- NA
    
    while (is.na(veg)) {
        lon <- runif(1, minLon, maxLon)
        lat <- runif(1, minLat, maxLat)
        row <- rowFromY(vegetation, lat)
        col <- colFromX(vegetation, lon)
        
        veg <- vegetation[row, col]
    }
    
    lons <- c(lons, lon)
    lats <- c(lats, lat)
    vegs <- c(vegs, veg)
}
        
# Make uniform grid. Probably technically better, but voronoi diagrams look cooler
lons <- seq(minLon, maxLon, length.out=100)
lats <- seq(minLat, maxLat, length.out=82)
points <- expand.grid(lons, lats)
lons <- c()
lats <- c()
vegs <- c()
for (i in 1:nrow(points)) {
    if (i%%100 == 0) print(i)
    lon <- points$Var1[i]
    lat <- points$Var2[i]
    row <- rowFromY(vegetation, lat)
    col <- colFromX(vegetation, lon)
    veg <- vegetation[row, col]
    if (!is.na(veg)) {
        lons <- c(lons, lon)
        lats <- c(lats, lat)
        vegs <- c(vegs, veg)
    }
}
cells <- voronoiShapefile(lons, lats, station_shapes)

# Plot points to check that it works
plot(lons, lats)

library(rgdal)
source("scripts/voronoi-shapefile.R")
station_shapes <- readOGR("data/voronoi/voronoi.shp")

lons_save <- lons
lats_save <- lats
vegs_save <- vegs
write.csv(cbind(lons_save, lats_save, vegs_save), "points_for_cells.csv")

#lons <- lons_save[1:5000]
#lats <- lats_save[1:5000]
#vegs <- vegs_save[1:5000]

library(prevR) # for point.in.SpatialPolygons
cells_df <- station_shapes[1,]
cells_df <- cells_df[,1]
cells_df$longitude <- 0
cells_df$latitude <- 0
cells_df$vegetation <- 0
cells_df$spice <- 0
# Runs into error at i=89
for (i in 1:length(station_shapes)) {
    shape <- station_shapes[i,]
    print(paste(shape$stationNam, i))
    
    in_polygon <- point.in.SpatialPolygons(lons, lats, shape)
    shape_lons <- lons[in_polygon]
    shape_lats <- lats[in_polygon]
    shape_vegs <- vegs[in_polygon]
    
    if (length(shape_lons) < 2) {
        shape <- shape[,1]
        shape$longitude <- shape@polygons[[1]]@labpt[1]
        shape$latitude <- shape@polygons[[1]]@labpt[2]
        shape$vegetation <- 16
        shape$spice <- 0
        #path <- paste("data/stations/", shape$stationNam, sep="")
        #writeOGR(shape, dsn=path, layer="voronoi", driver="ESRI Shapefile")
        #writeOGR(shape, dsn="data/cells", layer=shape$stationNam, driver="ESRI Shapefile")
        #station_shapes$cells[i] <- shape
        # Append to SpatialPolygonsDataFrame
        cells_df <- rbind(cells_df, shape)
        next
    }
    
    station_cells <- voronoiShapefile(shape_lons, shape_lats, shape)
    
    station_cells$longitude <- shape_lons
    station_cells$latitude <- shape_lats
    station_cells$vegetation <- shape_vegs
    station_cells$spice <- 0
    station_cells$id <- shape$id
    #path <- paste("data/stations/", shape$stationNam, sep="")
    
    #writeOGR(station_cells, dsn=path, layer="voronoi", driver="ESRI Shapefile")
    #writeOGR(station_cells, dsn="data/cells", layer=shape$stationNam, driver="ESRI Shapefile")
    #station_shapes$cells[i] <- station_cells
    cells_df <- rbind(cells_df, station_cells)
    # Append to SpatialPolygonsDataFrame
}

cells_df <- cells_df[-1,]
writeOGR(cells_df, dsn="data/grid", layer="voronoi", driver="ESRI Shapefile")
