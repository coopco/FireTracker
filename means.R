setwd("/Users/ronniebarker/Documents/Uni/3164")
wData <- read.csv("cleanedWeather.csv")
wData <- wData[,-1]
wData$Date <- as.Date(wData$Date, '%d/%m/%Y')

wData$WeekBefore <- wData$Date-7
wData$MonthBefore <- mondate(wData$Date) - 1
wData$YearBefore <- wData$Date-365

#make columns
wData$WeekMaxTempMean <- 0
wData$WeekRainMean <- 0
wData$WeekHumidityMean <- 0
wData$WeekWindMean <- 0

wData$MonthMaxTempMean <- 0
wData$MonthRainMean <- 0
wData$MonthHumidityMean <- 0
wData$WeekWindMean <- 0

wData$YearMaxTempMean <- 0
wData$YearRainMean <- 0
wData$YearHumidityMean <- 0
wData$YearWindMean <- 0

#loop through weekly/monthly/yearly
#loop through temp, rain, wind, humidity
#loop through each row
for (timeCol in 7:9) {
  for (col in 3:6) {
    for (row in 1:nrow(wData)) {
      startD <- wData[row,timeCol]  #beginning of time frame
      endD <- wData[row,1] #end of time frame
      rangeV <- wData[wData$Date<endD & wData$Date>=startD & wData$Location==wData[row,2],col]  #find all days within range at that location
      if(timeCol == 7){
        saveAt <- 10+(col-3)
      }
      else if(timeCol == 8){
        saveAt <- 14+(col-3)
      }
      else{
        saveAt <- 18+(col-3)
      }
      wData[row,saveAt] <- mean(rangeV)  #find mean of that time 
    }
  }
}

wData2 <- na.omit(wData)
write.csv(wData2, "DoubleCleanWeather.csv")
