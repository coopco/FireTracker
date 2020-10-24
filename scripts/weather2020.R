### Cleans the weather data for 2020 
weather <- read.csv("data/weather2020.csv") # TODO Update path
stations <- read.csv("data/acorn_sat_stations.csv")
weather$date_time <- as.Date(weather$date_time)

weather <- weather[,c(1,2,19,18,24,25)]
colnames(weather) <- c("Date", "MaxTemp", "Rainfall", "Humidity", "WindSpeed", "Position")

strings <- strsplit(weather$Position, ",")

weather$lat <- as.numeric(sapply(strings, `[[`, 1))
weather$long <- as.numeric(sapply(strings, `[[`, 2))
weather <- weather[,-6]
weather <- merge(weather, stations)

# Compute means
weather$WeekMaxTempMean <- 0
weather$WeekRainMean <- 0
weather$WeekHumidityMean <- 0
weather$WeekWindMean <- 0
weather$MonthMaxTempMean <- 0
weather$MonthRainMean <- 0
weather$MonthHumidityMean <- 0
weather$MonthWindMean <- 0
weather$YearMaxTempMean <- 0
weather$YearRainMean <- 0
weather$YearHumidityMean <- 0
weather$YearWindMean <- 0

currDate <- Sys.Date()
currWeather <- weather[weather$Date == currDate,]

for (i in 1:nrow(stations)) {
    station <- stations$name[i]
    stationWeather <- weather[weather$name == station,]
    
    # yes i know this is bad
    # Script was run on 2020-10-10
    currWeather[currWeather$name == station,]$WeekMaxTempMean <- mean(stationWeather[stationWeather$Date >= "2020-10-03",]$MaxTemp)
    currWeather[currWeather$name == station,]$WeekRainMean <- mean(stationWeather[stationWeather$Date >= "2020-10-03",]$Rainfall)
    currWeather[currWeather$name == station,]$WeekHumidityMean <- mean(stationWeather[stationWeather$Date >= "2020-10-03",]$Humidity)
    currWeather[currWeather$name == station,]$WeekWindMean <- mean(stationWeather[stationWeather$Date >= "2020-10-03",]$WindSpeed)
    currWeather[currWeather$name == station,]$MonthMaxTempMean <- mean(stationWeather[stationWeather$Date >= "2020-09-10",]$MaxTemp)
    currWeather[currWeather$name == station,]$MonthRainMean <- mean(stationWeather[stationWeather$Date >= "2020-09-10",]$Rainfall)
    currWeather[currWeather$name == station,]$MonthHumidityMean <- mean(stationWeather[stationWeather$Date >= "2020-09-10",]$Humidity)
    currWeather[currWeather$name == station,]$MonthWindMean <- mean(stationWeather[stationWeather$Date >= "2020-09-10",]$WindSpeed)
    currWeather[currWeather$name == station,]$YearMaxTempMean <- mean(stationWeather$MaxTemp[stationWeather$Date >= "2019-10-10"])
    currWeather[currWeather$name == station,]$YearRainMean <- mean(stationWeather$Rainfall[stationWeather$Date >= "2019-10-10"])
    currWeather[currWeather$name == station,]$YearHumidityMean <- mean(stationWeather$Humidity[stationWeather$Date >= "2019-10-10"])
    currWeather[currWeather$name == station,]$YearWindMean <- mean(stationWeather$WindSpeed[stationWeather$Date >= "2019-10-10"])
}

#currWeather <- currWeather[,c(5, 4, 2, 3, 6:17)]
weather <- weather[weather$Date < currDate,]
weather <- rbind(weather, currWeather)
weather <- weather[,c(10, 3:7, 11:22)]

for (i in 0:34) {
    weather[,paste("Prediction", i, sep="")] <- 0
}

library(keras)

model <- load_model_tf("model.h5")
# Make predictions
data <- weather[weather$Date == currDate,3:18]
for (i in 0:34) {
    preds <- predict(model, as.matrix(cbind(rep(i), data)))[,2]
    col_name <- paste("Prediction", i, sep="")
    weather[weather$Date == currDate,col_name] <- preds
}

write.csv(weather, "data/weather2020.csv", row.names = F)
#write.csv(currWeather, "data/currentWweather.csv", row.names=F)
