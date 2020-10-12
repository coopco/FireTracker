weather <- read.csv("data/weather2020.csv")
stations <- read.csv("data/acorn_sat_stations.csv")
weather$Date <- as.Date(weather$Date)
currDate <- Sys.Date()

# Get forecast
library(owmr)
Sys.setenv(OWM_API_KEY = "1e387518c257aa68289376fe93479d99")

library(lubridate)

weather <- weather[weather$Date < currDate,]
# Get current weather 
# Get forecast
for (i in 1:nrow(stations)) {
    print(i)
    lat <- stations$lat[i]
    lon <- stations$long[i]
    stationWeather <- weather[weather$name == stations$name[i],]
    # TODO figure out how to get rain for current day -- use forecast for now
    #current <- get_current(lat=lat, lon=lon)
    #curr_hours <- as.POSIXlt(current)
    forecast <- get_forecast(lat=lat, lon=lon)
    days <- day(as.POSIXlt(forecast$list$dt_txt))
    for (j in 0:4) { #1:4 TODO
        date = currDate + j
        day_forecast <- forecast$list[days==day(date),]
        hours <- hour(as.POSIXlt(day_forecast$dt_txt))
        MaxTemp <- max(day_forecast$main.temp_max) - 273.15
        Rainfall <- sum(day_forecast$rain.3h, na.rm=T)
        Humidity <- min(day_forecast$main.humidity[hours==9 | hours==15])
        WindSpeed <- max(day_forecast$wind.speed[hours==9 | hours==15])*3.6
        
        row <- stationWeather[weather$Date == "2020-09-29",]
        weekBefore <- currDate-7
        monthBefore <- currDate-30
        yearBefore <- currDate-365
        row$Date <- date
        row$MaxTemp <- MaxTemp
        row$Rainfall <- Rainfall
        row$WindSpeed <- WindSpeed
        row$Humidity <- Humidity
        row$WeekMaxTempMean <- mean(stationWeather[stationWeather$Date >= weekBefore,]$MaxTemp)
        row$WeekRainMean <- mean(stationWeather[stationWeather$Date >= weekBefore,]$Rainfall)
        row$WeekWindMean <- mean(stationWeather[stationWeather$Date >= weekBefore,]$WindSpeed)
        row$WeekHumidityMean <- mean(stationWeather[stationWeather$Date >= weekBefore,]$Humidity)
        row$MonthMaxTempMean <-  mean(stationWeather[stationWeather$Date >= monthBefore,]$MaxTemp)
        row$MonthRainMean <- mean(stationWeather[stationWeather$Date >= monthBefore,]$Rainfall)
        row$MonthWindMean <- mean(stationWeather[stationWeather$Date >= monthBefore,]$WindSpeed)
        row$MonthHumidityMean <- mean(stationWeather[stationWeather$Date >= monthBefore,]$Humidity)
        row$YearMaxTempMean <- mean(stationWeather[stationWeather$Date >= yearBefore,]$MaxTemp)
        row$YearRainMean <- mean(stationWeather[stationWeather$Date >= yearBefore,]$Rainfall)
        row$YearWindMean <- mean(stationWeather[stationWeather$Date >= yearBefore,]$WindSpeed)
        row$YearHumidityMean <- mean(stationWeather[stationWeather$Date >= yearBefore,]$Humidity)
        weather <- rbind(weather, row)
    }
}

## Recompute means because I messed up the code for the last week
#for (i in 1:nrow(stations)) {
#    print(i)
#    station <- stations$name[i]
#    stationWeather <- weather[weather$name == station,]
#    for (j in 0:5) {
#        date = as.Date("2020-10-11")+j
#        
#        weekBefore <- date-7
#        monthBefore <- date-30
#        yearBefore <- date-365
#        weather$WeekMaxTempMean[weather$name == station & weather$Date == date] <- mean(stationWeather[stationWeather$Date >= weekBefore & stationWeather$Date < date,]$MaxTemp)
#        weather$WeekRainMean[weather$name == station & weather$Date == date] <- mean(stationWeather[stationWeather$Date >= weekBefore & stationWeather$Date < date,]$Rainfall)
#        weather$WeekWindMean[weather$name == station & weather$Date == date] <- mean(stationWeather[stationWeather$Date >= weekBefore & stationWeather$Date < date,]$WindSpeed)
#        weather$WeekHumidityMean[weather$name == station & weather$Date == date] <- mean(stationWeather[stationWeather$Date >= weekBefore & stationWeather$Date < date,]$Humidity)
#        weather$MonthMaxTempMean[weather$name == station & weather$Date == date] <-  mean(stationWeather[stationWeather$Date >= monthBefore & stationWeather$Date < date,]$MaxTemp)
#        weather$MonthRainMean[weather$name == station & weather$Date == date] <- mean(stationWeather[stationWeather$Date >= monthBefore & stationWeather$Date < date,]$Rainfall)
#        weather$MonthWindMean[weather$name == station & weather$Date == date] <- mean(stationWeather[stationWeather$Date >= monthBefore & stationWeather$Date < date,]$WindSpeed)
#        weather$MonthHumidityMean[weather$name == station & weather$Date == date] <- mean(stationWeather[stationWeather$Date >= monthBefore & stationWeather$Date < date,]$Humidity)
#        weather$YearMaxTempMean[weather$name == station & weather$Date == date] <- mean(stationWeather[stationWeather$Date >= yearBefore & stationWeather$Date < date,]$MaxTemp)
#        weather$YearRainMean[weather$name == station & weather$Date == date] <- mean(stationWeather[stationWeather$Date >= yearBefore & stationWeather$Date < date,]$Rainfall)
#        weather$YearWindMean[weather$name == station & weather$Date == date] <- mean(stationWeather[stationWeather$Date >= yearBefore & stationWeather$Date < date,]$WindSpeed)
#        weather$YearHumidityMean[weather$name == station & weather$Date == date] <- mean(stationWeather[stationWeather$Date >= yearBefore & stationWeather$Date < date,]$Humidity)
#    }
#}

# Make predictions
library(keras)

model <- load_model_tf("model.h5")
# Make predictions
data <- weather[weather$Date >= currDate,3:18]
for (i in 0:34) {
    preds <- predict(model, as.matrix(cbind(rep(i), data)))[,2]
    col_name <- paste("Prediction", i, sep="")
    weather[weather$Date >= currDate,col_name] <- preds
}

write.csv(weather, "data/weather2020.csv", row.names=FALSE)

# Update shapefiles
library(rgdal)
currWeather <- weather[weather$Date == currDate,]
cells <- readOGR("data/grid/voronoi.shp")

for (i in 1:length(cells)) {
    stationName <- stations$name[cells$id[i]]
    colNum <- cells$vegetation[i] + 19
    cells$spice[i] <- currWeather[currWeather$name == stationName, colNum]
}

writeOGR(cells, dsn="data/grid", layer="voronoi", driver="ESRI Shapefile", overwrite_layer = T)

voronoi <- readOGR("data/voronoi/voronoi.shp")

for (i in 1:length(voronoi)) {
    stationName <- stations$name[voronoi$id[i]]
    colNum <- voronoi$vegetation[i] + 19
    voronoi$spice[i] <- currWeather[currWeather$name == stationName, colNum]
}
writeOGR(voronoi, dsn="data/voronoi", layer="voronoi", driver="ESRI Shapefile", overwrite_layer = T)

