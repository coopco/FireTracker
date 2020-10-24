### Script for fetching new data from the OpenWeatherMap API.
### Must be run daily.
### See https://openweathermap.org/api
### NOTE: This script will overwrite data. Make sure to backup data to repository. 

library(owmr) # For API
library(lubridate)
library(keras) # For predictions
library(rgdal) # For shapefiles

# Set API key
Sys.setenv(OWM_API_KEY = "1e387518c257aa68289376fe93479d99")

# Load data
weather <- read.csv("data/weather2020.csv") # Records data from Oct 2019 onwards
stations <- read.csv("data/acorn_sat_stations.csv") # Data for stations
weather$Date <- as.Date(weather$Date)
currDate <- Sys.Date()

weather <- weather[weather$Date < currDate,] # Avoid doubling up data on current date

# Get forecast
for (i in 1:nrow(stations)) {
    print(i) 
    # Station coordinates
    lat <- stations$lat[i]
    lon <- stations$long[i]
    stationWeather <- weather[weather$name == stations$name[i],] # Weather for just current station
    forecast <- get_forecast(lat=lat, lon=lon) # Get forecast
    days <- day(as.POSIXlt(forecast$list$dt_txt)) # Extract days
    
    # For current day and next 4 days
    for (j in 0:4) {
        date = currDate + j 
        day_forecast <- forecast$list[days==day(date),]
        hours <- hour(as.POSIXlt(day_forecast$dt_txt))
        MaxTemp <- max(day_forecast$main.temp_max) - 273.15 # Convert to Celsius
        Rainfall <- sum(day_forecast$rain.3h, na.rm=T) # Sum of rainfall for day
        # Min/max of feature at 9am or 3pm
        Humidity <- min(day_forecast$main.humidity[hours==9 | hours==15])
        WindSpeed <- max(day_forecast$wind.speed[hours==9 | hours==15])*3.6 # Convert to km/h
        
        # Get placeholder row
        row <- stationWeather[stationWeather$Date == "2020-09-29",]
        weekBefore <- currDate-7
        monthBefore <- currDate-30
        yearBefore <- currDate-365
        # Update all columns for new row
        # See documentation for prediction method for more information on columns
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

### Make predictions

# Load the model
model <- load_model_tf("model.h5")
data <- weather[weather$Date >= currDate,3:18] # Extract relevant columns
# Make predictions for each type of vegetation
for (i in 0:34) {
    # Get predictions
    preds <- predict(model, as.matrix(cbind(rep(i), data)))[,2]
    col_name <- paste("Prediction", i, sep="") # Create columns name
    weather[weather$Date >= currDate,col_name] <- preds # Add predictions to dataset
}

# Write new data
write.csv(weather, "data/weather2020.csv", row.names=FALSE)

# Update shapefiles
currWeather <- weather[weather$Date == currDate,]

# Read stations grid data
cells <- readOGR("data/grid/voronoi.shp")

# Store prediction for each cell
for (i in 1:length(cells)) {
    stationName <- stations$name[cells$id[i]]
    colNum <- cells$vegetation[i] + 19
    cells$spice[i] <- currWeather[currWeather$name == stationName, colNum]
}

# Write updated data
writeOGR(cells, dsn="data/grid", layer="voronoi", driver="ESRI Shapefile", overwrite_layer = T)

# Read station shape data
voronoi <- readOGR("data/voronoi/voronoi.shp")

# Store prediction for each station, using average vegetation in that stations area
for (i in 1:length(voronoi)) {
    stationName <- stations$name[voronoi$id[i]]
    colNum <- voronoi$vegetation[i] + 19
    voronoi$spice[i] <- currWeather[currWeather$name == stationName, colNum]
}

# Write updated data
writeOGR(voronoi, dsn="data/voronoi", layer="voronoi", driver="ESRI Shapefile", overwrite_layer = T)

