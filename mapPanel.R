library(lubridate)

weather2020 <- read.csv("data/weather2020.csv")
weather2020$Date <- as.Date(weather2020$Date)
monthlyMeans <- read.csv("data/monthlyMeans.csv")

currDate <- Sys.Date()

# TODO change these to html objects
helpTitle <- "Fire Risk Map"
helpText <- "This is html describing how the map works."

panel <- absolutePanel(id = "control", fixed = TRUE, width = 400, height ="auto",
                      top = 77, left = 16, right = "auto", bottom = 16,
          textOutput("helpTitle"),
          textOutput("helpText"),
          uiOutput("panelTitle"),
          htmlOutput("panelText"),
          plotOutput("panelPlot")
        )

mapPanelServer <- function(input, output) {
  output$helpTitle <- renderText(helpTitle)
  output$helpText <- renderText(helpText)
  
  mapPanelClickEvent <- observeEvent(input$map_shape_click, {
    event <- input$map_shape_click
    newStationName <- shapes$stationNam[event$id]
    shortStationName <- shortStationNames$ShortName[shortStationNames$Name == newStationName]
    # Might be slow, should make faster if need to
    stationWeather <- weather2020[weather2020$name == newStationName & weather2020$Date+7 >= currDate,]
    currWeather <- stationWeather[stationWeather$Date == currDate,]
    
    if (!is.null(event$id)) {
      output$helpTitle <- NULL
      output$helpText <- NULL
      # render stuff
      output$panelTitle <- renderUI({
        tagList(
          fluidRow(
            column(2, p(""), actionButton("back", "", icon=icon("arrow-left"))),
            column(8, h3(shortStationName))
            )
        )
      })
      
      # Weather text
      stationMeans <- monthlyMeans[monthlyMeans$Station == newStationName & monthlyMeans$Month == month(currDate),]
      tempDiff <- round(currWeather$MaxTemp - stationMeans$MaxTemp, digits=1)
      windDiff <- round(currWeather$WindSpeed - stationMeans$WindSpeed, digits=1)
      humidityDiff <- round(currWeather$Humidity - stationMeans$Humidity, digits=1)
      rainDiff <- round(currWeather$MonthRainMean - stationMeans$Rainfall, digits=1)
      
      # Panel text
      # fa-temperature-high fa-wind fa-tint fa-cloud-showers-heavy
      # 
      output$panelText <- renderText(paste(tempDiff, windDiff, humidityDiff, rainDiff))
      output$panelText <- renderUI({
        # TODO add plus signs to positive diffs
        tempText <-     paste("<h3 style='display: inline'>",icon('temperature-high'),paste(currWeather$MaxTemp,'\u00b0',sep=""),"</h3>",'&nbsp;&nbsp;(',tempDiff,')')
        windText <-     paste("<h3 style='display: inline'>",icon('wind'),currWeather$WindSpeed,"</h3>",'km/h &nbsp;&nbsp;(',windDiff,')')
        humidityText <- paste("<h3 style='display: inline'>",icon('tint'),paste(currWeather$Humidity,'%',sep=""),"</h3>",'humidity &nbsp;&nbsp;(',paste(humidityDiff,'%',sep=""),')')
        rainText <-     paste("<h3 style='display: inline'>",icon('cloud-showers-heavy'),currWeather$Rainfall,"</h3>",'mm &nbsp;&nbsp;(',rainDiff,')')
        #str1 <- paste("<h3 style=\"text-align: left\">", tempText, windText, humidityText, rainText, "</h3>", sep="</br>")
        str1 <- paste(paste("</br>&nbsp;&nbsp;&nbsp;&nbsp",tempText), windText, humidityText, rainText, "", sep="</br></br>&nbsp;&nbsp;&nbsp;&nbsp;")
        HTML(paste(str1))
        })
      # Risk plot
      veg <- shapes$vegetation[event$id]
      colName <- paste("Prediction", veg, sep="")
      output$panelPlot <- renderPlot(ggplot(data=stationWeather, aes_string(x="Date", y=colName)) 
                                     + geom_line(color="red") + geom_point() + ylim(0, 1) 
                                     + labs(title = paste("Bushfire Risk", shortStationName), x="Date", y="Risk") 
                                     + theme(plot.title = element_text(hjust=0.5)))
    }
  })
  
  observeEvent(input$back, {
    # Add non-focus psnel stuff
    output$panelTitle <- NULL
    output$panelText <- NULL
    output$panelPlot <- NULL
    output$helpTitle <- renderText(helpTitle)
    output$helpText <- renderText(helpText)
    leafletProxy("map") %>% fitBounds(73, 3.5, 178, -51.5) %>% hideGroup(old_station_name)
  })
}