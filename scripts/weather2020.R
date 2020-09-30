weather <- read.csv("data/weather2020.csv")
stations <- read.csv("data/acorn_sat_stations.csv")
weather$date_time <- as.Date(weather$date_time)

weather <- weather[,c(1,2,16,18,19,25)]
colnames(weather) <- c("Date", "MaxTemp", "WindGustSpeed", "Humidity", "Rainfall", "Position")

strings <- strsplit(weather$Position, ",")

weather$lat <- as.numeric(sapply(strings, `[[`, 1))
weather$long <- as.numeric(sapply(strings, `[[`, 2))
weather <- weather[,-6]
weather <- merge(weather, stations)

# Compute means
weather$WeekMaxTempMean <- 0
weather$WeekRainMean <- 0
weather$WeekWindMean <- 0
weather$WeekHumidityMean <- 0
weather$MonthMaxTempMean <- 0
weather$MonthRainMean <- 0
weather$MonthWindMean <- 0
weather$MonthHumidityMean <- 0
weather$YearMaxTempMean <- 0
weather$YearRainMean <- 0
weather$YearWindMean <- 0
weather$YearHumidityMean <- 0

currWeather <- weather[weather$Date == as.Date("2020-09-29"),]

for (i in 1:nrow(stations)) {
    station <- stations$name[i]
    stationWeather <- weather[weather$name == station,]
    
    # yes i know this is bad
    currWeather[currWeather$name == station,]$WeekMaxTempMean <- mean(stationWeather[stationWeather$Date >= "2020-09-22",]$MaxTemp)
    currWeather[currWeather$name == station,]$WeekRainMean <- mean(stationWeather[stationWeather$Date >= "2020-09-22",]$Rainfall)
    currWeather[currWeather$name == station,]$WeekWindMean <- mean(stationWeather[stationWeather$Date >= "2020-09-22",]$WindGustSpeed)
    currWeather[currWeather$name == station,]$WeekHumidityMean <- mean(stationWeather[stationWeather$Date >= "2020-09-22",]$Humidity)
    currWeather[currWeather$name == station,]$MonthMaxTempMean <- mean(stationWeather[stationWeather$Date >= "2020-08-29",]$MaxTemp)
    currWeather[currWeather$name == station,]$MonthRainMean <- mean(stationWeather[stationWeather$Date >= "2020-08-29",]$Rainfall)
    currWeather[currWeather$name == station,]$MonthWindMean <- mean(stationWeather[stationWeather$Date >= "2020-08-29",]$WindGustSpeed)
    currWeather[currWeather$name == station,]$MonthHumidityMean <- mean(stationWeather[stationWeather$Date >= "2020-08-29",]$Humidity)
    currWeather[currWeather$name == station,]$YearMaxTempMean <- mean(stationWeather$MaxTemp)
    currWeather[currWeather$name == station,]$YearRainMean <- mean(stationWeather$Rainfall)
    currWeather[currWeather$name == station,]$YearWindMean <- mean(stationWeather$WindGustSpeed)
    currWeather[currWeather$name == station,]$YearHumidityMean <- mean(stationWeather$Humidity)
    #currWeather$PrevWeekMaxTemp[currWeather$name == station,] <- stationWeather[stationWeather$Date == "2020-09-22"]$MaxTemp
    #currWeather$PrevWeekRain[currWeather$name == station,] <- stationWeather[stationWeather$Date == "2020-09-22"]$Rainfall
    #currWeather$PrevWeekWind[currWeather$name == station,] <- stationWeather[stationWeather$Date == "2020-09-22"]$WindGustSpeed
    #currWeather$PrevWeekHumidity[currWeather$name == station,] <- stationWeather[stationWeather$Date == "2020-09-22"]$Humidity
    #currWeather$PrevMonthMaxTemp[currWeather$name == station,] <- stationWeather[stationWeather$Date == "2020-08-29"]$MaxTemp
    #currWeather$PrevMonthRain[currWeather$name == station,] <- stationWeather[stationWeather$Date == "2020-08-29"]$Rainfall
    #currWeather$PrevMonthWind[currWeather$name == station,] <- stationWeather[stationWeather$Date == "2020-08-29"]$WindGustSpeed
    #currWeather$PrevMonthHumidity[currWeather$name == station,] <- stationWeather[stationWeather$Date == "2020-08-29"]$Humidity
    #currWeather$PrevYearMaxTemp[currWeather$name == station,] <- stationWeather[stationWeather$Date == "2019-10-01"]$MaxTemp
    #currWeather$PrevYearRain[currWeather$name == station,] <- stationWeather[stationWeather$Date == "2019-10-01"]$Rainfall
    #currWeather$PrevYearWind[currWeather$name == station,] <- stationWeather[stationWeather$Date == "2019-10-01"]$WindGustSpeed
    #currWeather$PrevYearHumidity[currWeather$name == station,] <- stationWeather[stationWeather$Date == "2019-10-01"]$Humidity
}

#currWeather <- currWeather[,c(5, 4, 2, 3, 6:17)]
weather <- weather[weather$Date < "2020-09-29",]
weather <- rbind(weather, currWeather)
weather <- weather[,c(3, 10, 4, 7, 5, 6, 11:22)]

for (i in 0:34) {
    weather[,paste("Prediction", i, sep="")] <- 0
}

library(keras)

model <- load_model_tf("model")
# Make predictions
data <- weather[weather$Date == "2020-09-29",3:18]
for (i in 0:34) {
    preds <- predict(model, as.matrix(cbind(rep(i), data)))[,2]
    col_name <- paste("Prediction", i, sep="")
    weather[weather$Date == "2020-09-29",col_name] <- preds
}

write.csv(weather, "data/weather2020.csv", row.names = F)
#write.csv(currWeather, "data/currentWweather.csv", row.names=F)
