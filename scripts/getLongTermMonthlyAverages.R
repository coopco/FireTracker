### Code for calculating the long-term monthly averages for ACORN_SAT stations
### Uses weather data obtained from the WorldWeatherOnline API

library(lubridate)

# Read data
weather <- read.csv("acorn_weather/combined.csv")
stations <- read.csv("Data/acorn_sat_stations.csv")
weather$Date <- as.Date(weather$Date)

# Merge weather data and station data
weather <- weather[,c(25, 1, 2, 19, 18, 24)]
names(weather) <- c("Location", "Date", "MaxTemp", "Rainfall", "Humidity", "WindSpeed")
location_split <- strsplit(weather$Location, ",")
weather$lat <- as.numeric(sapply(location_split, "[[", 1))
weather$long <- as.numeric(sapply(location_split, "[[", 2))
weather <- merge(weather, stations)
weather <- weather[,c(11, 1, 2, 4, 5, 6, 7, 8)]
weather$Date <- as.Date(weather$Date)
weather$Month <- month(weather$Date)

# Initialise data frame
monthlyMeans <- data.frame(Station=character(), Month=numeric(), MaxTemp=numeric(), 
                           Rainfall=numeric(), Humidity=numeric(), WindSpeed=numeric())

# For each station and month, compute the average for each feature
for (j in 1:nrow(stations)) {
    station <- stations$name[j]
    for (i in 1:12) {
        monthData <- weather[weather$name == station & weather$Month == i,]
        meanMaxTemp <- mean(monthData$MaxTemp)
        meanRainfall <- mean(monthData$Rainfall)
        meanHumidity <- mean(monthData$Humidity)
        meanWindSpeed <- mean(monthData$WindSpeed)
        monthlyMeans[nrow(monthlyMeans)+1,] = list(station, i, meanMaxTemp, meanRainfall, meanHumidity, meanWindSpeed)
    }
}

# Write to file
write.csv(monthlyMeans, 'data/monthlyMeans.csv', row.names = F)
