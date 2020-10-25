### Uses mapshaper to simplify the Australia shapefile
### Original shapefile retrieved from:
###     https://www.abs.gov.au/AUSSTATS/abs@.nsf/DetailsPage/1270.0.55.003July%202016?OpenDocument
library(rgdal)
library(rmapshaper)

shapes = readOGR("Datasets/1270055001_ste_2016_aust_shape/")
shapes_simple = ms_simplify(shapes)
shapes_simple_2 = ms_simplify(shapes)
writeOGR(shapes_simple_2, dsn="Datasets/simple_states_2", layer="simple_states", driver="ESRI Shapefile")