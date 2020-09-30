weather <- read.csv("Datasets/DoubleCleanWeather.csv")
weather <- weather[,-8]
weather <- weather[,-8]
weather <- weather[,-8]
#weather <- weather[,-1]

stations <- unique(weather$Location)
idxs <- c()
for (i in 1:length(stations)) {
    idxs <- c(idxs, weather$X[weather$Location == stations[i]][1:347])
}

weather <- weather[-idxs,]
weather <- weather[!is.na(weather$WeekMaxTempMean),]

fire_data <- read.csv("Datasets/training_data.csv")
fire_data <- fire_data[,c("acq_date","closest_station","latitude","longitude","confidence","frp","class","vegetation")]

merged <- merge(fire_data, weather, by.x = c("acq_date", "closest_station"), by.y = c("Date", "Location"))
merged <- merged[,-9]

write.csv(merged, "Datasets/training.csv", row.names = FALSE)
