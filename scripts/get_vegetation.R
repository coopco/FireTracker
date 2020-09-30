#library(rgdal)
#remoteness <- readOGR("Datasets/remoteness.gpkg")
library(raster)
vegetation <- raster("Datasets/vegetation.gpkg")
data <- read.csv("Datasets/training_data.csv")

#readOGR("Datasets", "Remoteness.gpkg")


rows <- rowFromY(vegetation, data$latitude)
cols <- colFromX(vegetation, data$longitude)
data$vegetation <- vegetation[cbind(rows, cols)] # Takes some time to run


# I ran the below code like 5 times to reduce NAs
# I assume these NAs come from points near the coast
data_na <- data[is.na(data$vegetation),]
data_na <- data_na[data_na$class == 0,]
n <- nrow(data_na)
lat <- data_na$latitude
lon <- data_na$longitude
# Generate latitude and longitude
r <- 0.25*sqrt(runif(n))
th <- 2*pi*runif(n)
latitude <- lat + r*cos(th)
longitude <- lon + r*sin(th)

data$latitude[data_na$X] <- latitude
data$longitude[data_na$X] <- longitude

rows <- rowFromY(vegetation, data$latitude)
cols <- colFromX(vegetation, data$longitude)
data$vegetation <- vegetation[cbind(rows, cols)] # Takes some time to run

sum(is.na(data$vegetation))


# Save
data <- data[!is.na(data$vegetation),]
data$vegetation[data$vegetation==35] = 0 # Change urban area class from 35 to 0
write.csv(data, "Datasets/training_data.csv", row.names=F)
