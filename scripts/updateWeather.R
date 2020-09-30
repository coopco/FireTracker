library(bomrang)
# Get forecast
# Update rolling avergaes
#   So we need to keep data for entire previous year
#   16 (features) + 3*4 (prev features) + 35 (veg preds) + 7 (future preds) columns
#   112 rows
# Add forecast to existing data

weather <- read.csv("data/weather2020.csv")
stations <- read.csv("data/acorn_sat_stations.csv")
weather$Date <- as.Date(weather$Date)
currDate <- Sys.Date()

# Get forecast
library(bomrang)

rows <- list()
for (i in 1:nrow(stations)) {
    print(i)
    station <- stations$name[i]
    if (i==2) station = "HALLS CREEK AIRPORT"
    print(station)
    station_weather <- as.data.frame(get_current_weather(station, strict=T))
    station_weather$local_date_time_full <- as.Date(station_weather$local_date_time_full)
    station_weather <- station_weather[station_weather$local_date_time_full == currDate,]
    
    MaxTemp <- max(station_weather$air_temp)
    Rainfall <- max(station_weather$rain_trace)
    WindGustSpeed <- max(station_weather$gust_kmh)
    Humidity <- 100 - 5*(MaxTemp - max(station_weather$dewpt)) # Approximation
    
    # Update rolling averages
    row <- weather[weather$name == station & weather$Date == currDate,]
    weekBefore <- currDate-7
    monthBefore <- currDate-30
    yearBefore <- currDate-365
    row$Date <- currDate
    row$MaxTemp <- MaxTemp
    row$Rainfall <- Rainfall
    row$WindGustSpeed <- WindGustSpeed
    row$Humidity <- Humidity
    row$WeekMaxTempMean <- mean(weather[weather$Date >= weekBefore,]$MaxTemp)
    row$WeekRainMean <- mean(weather[weather$Date >= weekBefore,]$Rainfall)
    row$WeekWindMean <- mean(weather[weather$Date >= weekBefore,]$WindGustSpeed)
    row$WeekHumidityMean <- mean(weather[weather$Date >= weekBefore,]$Humidity)
    row$MonthMaxTempMean <-  mean(weather[weather$Date >= monthBefore,]$MaxTemp)
    row$MonthRainMean <- mean(weather[weather$Date >= monthBefore,]$Rainfall)
    row$MonthWindMean <- mean(weather[weather$Date >= monthBefore,]$WindGustSpeed)
    row$MonthHumidityMean <- mean(weather[weather$Date >= monthBefore,]$Humidity)
    row$YearMaxTempMean <- mean(weather[weather$Date >= yearBefore,]$MaxTemp)
    row$YearRainMean <- mean(weather[weather$Date >= yearBefore,]$Rainfall)
    row$YearWindMean <- mean(weather[weather$Date >= yearBefore,]$WindGustSpeed)
    row$YearHumidityMean <- mean(weather[weather$Date >= yearBefore,]$Humidity)
    rows <- c(rows, row)
}

# Make predictions
library(keras)

model <- load_model_tf("model")
# Make predictions
data <- weather[weather$Date == currDate,2:17]
for (i in 0:34) {
    preds <- predict(model, as.matrix(cbind(rep(i), data)))[,2]
    col_name <- paste("Prediction", i, sep="")
    weather[weather$Date == currDate,col_name] <- preds
}

write.csv(weather, "data/weather2020.csv")


# Update shapefiles
#cells$vegetation[cells$vegetation==35] <- 0
library(rgdal)
currWeather <- weather[weather$Date == currDate,]
cells <- readOGR("data/grid/voronoi.shp")
for (i in 1:length(cells)) {
    stationName <- stations$name[cells$id[i]]
    colNum <- cells$vegetation[i] + 19
    cells$spice[i] <- currWeather[currWeather$name == stationName, colNum]
}

# TODO delete previous file
writeOGR(cells, dsn="data/grid", layer="voronoi", driver="ESRI Shapefile")
