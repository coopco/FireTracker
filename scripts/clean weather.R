### Used for cleaning weatherdata.csv, as downloaded from https://www.kaggle.com/jsphyg/weather-dataset-rattle-package
# CLEAN WEATHER DATA - RHONWYN

#setwd("/Users/ronniebarker/Documents/Uni/3164")
weather <- read.csv('weatherdata.csv') #read in
attach(weather)  #use c names

weather <- weather[,c(1,2,4,5,12,13,14,15)]  #select relevant columns (only max temp, min of humidity, generic wind speed)
weather$Humidity <- with(weather, pmin(weather$Humidity9am, weather$Humidity3pm)) #find min humidity
weather$WindSpeed <- with(weather, pmax(weather$WindSpeed9am, weather$WindSpeed3pm))
weather <-weather[,-c(5,6,7,8)] #remove timed humidity and wind

#Check if any locations have too many na and should be omitted 
library(dplyr)
tmp <- weather %>% group_by(weather$Location) %>% summarise_all(funs(sum(is.na(.))))  #number of na per column grouped bylocation
tmp <-tmp[,-c(2,3)] #remove irrelevant
tmp$Sum <- tmp[2]+tmp[3]+tmp[4] #sum na per location
colnames(tmp)<- c("Location", "MaxTemp", "Rainfall", "Humidity", "Total")
count<- weather %>%
  count(Location) #count of entries per location
colnames(count)  <- c("Location", "Total Obs")
tmp <- merge(tmp, count)  #merge
tmp$Percent <- (tmp[5]/tmp[6])*100 #percent of entries that are na per location 

summary(weather)
weather[!complete.cases(weather),]

weather <- na.omit(weather) #remove na

write.csv(weather, "cleanedWeather.csv")
