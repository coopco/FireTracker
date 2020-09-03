library(rgdal)
library(rmapshaper)

shapes = readOGR("Datasets/1270055001_ste_2016_aust_shape/")
shapes_simple = ms_simplify(shapes)
shapes_simple_2 = ms_simplify(shapes)
writeOGR(simple_states, dsn="Datasets/simple_states_2", layer="simple_states", driver="ESRI Shapefile")