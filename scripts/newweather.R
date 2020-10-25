### Merges fire data and weather data to create the training data for prediction

# awk 'FNR==1 && NR!=1{next;}{print}' *.csv > combined.csv 
# Read weather data
weather <- read.csv("Datasets/DoubleCleanWeather.csv")
weather <- weather[,-8]
weather <- weather[,-8]
weather <- weather[,-8]
#weather <- weather[,-1]

# Remove all rows with inaccurate means
stations <- unique(weather$Location)
idxs <- c()
for (i in 1:length(stations)) {
    idxs <- c(idxs, weather$X[weather$Location == stations[i]][1:347])
}

# Delete rows and nas
weather <- weather[-idxs,]
weather <- weather[!is.na(weather$WeekMaxTempMean),]

# Fire data
fire_data <- read.csv("Datasets/training_data.csv")
fire_data <- fire_data[,c("acq_date","closest_station","latitude","longitude","confidence","frp","class","vegetation")]

# Merge data
merged <- merge(fire_data, weather, by.x = c("acq_date", "closest_station"), by.y = c("Date", "Location"))
merged <- merged[,-9]

# Write data
write.csv(merged, "Datasets/training.csv", row.names = FALSE)
