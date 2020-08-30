library(ggplot2)
library(dplyr)
library(zoo)

setwd("/Users/ronniebarker/Documents/Uni/3164/bushfire")
fireWeather <- read.csv('fireWeather.csv')
state <- read.csv('stationLocations.csv')

#process state
state <- state[,c(3,10)]  #select relevant columns
colnames(state) <- c('Location', 'State')

#process fire
fireWeather <- fireWeather[,c(1,2,3,4,7,10)]  #select relevant columns
fireWeather$Date <- as.yearmon(fireWeather$Date) #only look at monthly
colnames(fireWeather) <- c('Date','Location','Latitude','Longitude', 'Confidence','Temperature')

#merge
fireWeather <- merge(fireWeather, state, by = 'Location')

#plot
fireSum <- fireWeather %>% group_by(fireWeather$Date) %>% summarise(Count = n()) #number of fires per month
fireSum <- merge(fireWeather, fireSum, by.x = 'Date', by.y = 'fireWeather$Date') #join tables
colnames(fireSum) <- c('Date','Location','Latitude','Longitude', 'Confidence','Temperature','State','nFire')

ggplot(fireSum, aes(x=Date, y=nFire, size = Temperature)) +
  geom_point(alpha=0.7) #all states

stateName <- 'NSW'    #which state
tmp <- fireSum[which(fireSum$State==stateName),]  #just that state
ggplot(tmp, aes(x=Date, y=nFire, size = Temperature)) +
  geom_point(alpha=0.7) + ggtitle(paste("Number of Fires and Temperature Over Time in "), stateName) +
  ylab("Number of Fires") + xlab("Month")   #bubble plot

#colour plot
scatter<- ggplot(tmp, aes(x=Date, y=nFire, color = Temperature)) +
  geom_point(alpha=0.7) + ggtitle(paste("Number of Fires and Temperature Over Time in "), stateName) +
  ylab("Number of Fires") + xlab("Month")
scatter + scale_color_gradient(low="white", high="red")
