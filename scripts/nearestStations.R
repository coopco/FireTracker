### Used to compute nearest station for each point in the fire dataset

library(geosphere)

fires = read.csv("Datasets/cleanFire.csv", nrow=17133276) # nrow speeds up reading time
weather = read.csv("Datasets/cleanedWeather.csv")
stations = read.csv("Datasets/stationLocations.csv")
# Only keep fires withing range of weather data
fires_short = fires[fires$acq_date >= min(weather$Date) & fires$acq_date <= max(weather$Date),]

# Only keep needed columns
fires_short = fires[,c("latitude","longitude","acq_date","acq_time","instrument","confidence","frp")]

# Store fire lats and longs
fire_lats = fires_short$latitude
fire_lons = fires_short$longitude

# Store station lats and longs
station_lats = stations$Lat
station_lons = stations$Lon
distances = vector(mode="logical", length=length(fire_lats)) # Initialise vector
closest_stations = vector(mode="character", length=length(fire_lats)) # Initialise vector

# Function to return distance between two points
apply_distm = function(point) {
        return(distm(c(point[3], point[1]), c(point[4], point[2]), fun = distHaversine))
}

# For each fire
# This code is quite slow
for (fire_idx in 1:length(fire_lats)) {
        if (fire_idx %% 500 == 0) {
                # Print progress
                print(fire_idx)
        }
        
        # Creates list of points from fire to each of 112 stations
        fire_lat = rep(fire_lats[fire_idx], length(station_lats))
        fire_lon = rep(fire_lons[fire_idx], length(station_lons))
        points = cbind(fire_lat, station_lats, fire_lon, station_lons)
        
        # Find the station with minimum distance
        distances = apply(points, 1, apply_distm)
        station_idx = which.min(distances)
        dist = min(distances)
        if (dist < 30000) {
                closest_stations[fire_idx] = stations$Location[station_idx]
        } else {
                closest_stations[fire_idx] = NA
                
        }
}

# Add closest station as a column
fires$Location = closest_stations
# Only keep fires that have accurate weather information
new_fires = fires_short[distances < 30000,]

# Write to file
write.csv(new_fires, "fires.csv", row.names=FALSE)