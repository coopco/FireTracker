fires <- read.csv("Datasets/fires_nice.csv")
weather <- read.csv("Datasets/cleanedWeather.csv")
stations <- read.csv("Datasets/stationLocations.csv")

fire_data <- fires[fires$instrument=="MODIS",]

# Fire classes:
# 1: frp <= 200, 2: frp <= 500, 3: frp < 900, 4: frp >= 900
fire_data$class <- NA 
fire_data$class[fire_data$frp <= 200] <- 1
fire_data$class[fire_data$frp > 200 & fire_data$frp <= 500] <- 2
fire_data$class[fire_data$frp > 500 & fire_data$frp <= 900] <- 3
fire_data$class[fire_data$frp > 900] <- 4

fire_data$acq_date <- as.Date(fire_data$acq_date)
# Initialise data frame
data <- data.frame(acq_date=character(), closest_station=character(), latitude=numeric(), longitude=numeric())
# Used for generating dates
dates <- seq(min(fire_data$acq_date), max(fire_data$acq_date), by="day")

# Generate points
for (i in 1:nrow(stations)) {
    # Get dates where there was not a fire
    non_fire_dates <- setdiff(dates, fire_data$acq_date[fire_data$closest_station==station])
    
    n <- min(length(fire_data$closest_station[fire_data$closest_station == station]), length(non_fire_dates))
    
    station <- stations$Location[i]
    lat <- stations$Lat[i]
    lon <- stations$Lon[i]
    
    # Generate latitude and longitude
    r <- 0.25*sqrt(runif(n))
    th <- 2*pi*runif(n)
    latitude <- lat + r*cos(th)
    longitude <- lon + r*sin(th)
    
    # Sample dates
    acq_date <- sample(non_fire_dates, n)
    # Add points to data
    closest_station <- rep(station, length.out=n)
    data <- rbind(data, as.data.frame(cbind(acq_date, closest_station, latitude, longitude)))
}

data$acq_date <- as.Date(as.numeric(data$acq_date), origin="1970-01-01")

# Combine fire_data and generated points
fire_data <- fire_data[,c("acq_date","closest_station","latitude","longitude","confidence","frp","class")]
data$confidence <- rep(100)
data$frp <- rep(0)
data$class <- rep(0)
combined <- rbind(fire_data, data)

# Merge points with weather data
combined <- merge(combined, weather, by.x = c("acq_date", "closest_station"), by.y = c("Date", "Location"))

write.csv(combined, "Datasets/training_data.csv")
