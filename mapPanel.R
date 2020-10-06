weather2020 <- read.csv("data/weather2020.csv")
weather2020$Date <- as.Date(weather2020$Date)

#currDate <- Sys.Date()
currDate <- "2020-09-29"

# TODO change these to html objects
helpTitle <- "Fire Risk Map"
helpText <- "This is html describing how the map works."

panel <- absolutePanel(id = "control", fixed = TRUE, width = 400, height ="auto",
                      top = 77, left = 16, right = "auto", bottom = 16,
          textOutput("helpTitle"),
          textOutput("helpText"),
          uiOutput("panelTitle"),
          textOutput("panelText"),
          plotOutput("panelPlot")
        )

mapPanelServer <- function(input, output) {
  output$helpTitle <- renderText(helpTitle)
  output$helpText <- renderText(helpText)
  
  mapPanelClickEvent <- observeEvent(input$map_shape_click, {
    event <- input$map_shape_click
    newStationName <- shapes$stationNam[event$id]
    shortStationName <- shortStationNames$ShortName[shortStationNames$Name == newStationName]
    stationWeather <- weather2020[weather2020$name == newStationName & weather2020$Date+7 >= currDate,]
    currWeather <- stationWeather[stationWeather$Date == "2020-09-29",]
    
    if (!is.null(event$id)) {
      output$helpTitle <- NULL
      output$helpText <- NULL
      # render stuff
      output$panelTitle <- renderUI({
        tagList(
          fluidRow(
            column(2, p(""), actionButton("back", "", icon=icon("arrow-left"))),
            column(8, h3(shortStationName))#, style="font-size:4vmax;"))
            )
        )
      })
      
      # Weather text
      print(currWeather)
      tempDiff <- currWeather$MaxTemp - currWeather$YearMaxTempMean
      windDiff <- currWeather$WindGustSpeed - currWeather$YearWindMean 
      humidityDiff <- currWeather$Humidity - currWeather$YearHumidityMean 
      tempDiff <- round(tempDiff, digits=1)
      windDiff <- round(windDiff, digits=1)
      humidityDiff <- round(tempDiff, digits=1)
      
      # Help text
      
      output$panelText <- renderText(paste(tempDiff, windDiff, humidityDiff))
      # Risk plot
      veg <- shapes$vegetation[event$id]
      colName <- paste("Prediction", veg, sep="")
      #output$panelPlot <- renderPlot(ggplot(data=stationWeather, aes_string(x="Date", y=colName)) 
      #                               + geom_line(color="red") + geom_point() + ylim(0, 1) 
      #                               + labs(title = paste("Bushfire Risk", shortStationName), x="Date", y="Risk") 
      #                               + theme(plot.title = element_text(hjust=0.5)))
      # Get avergae vegetation for station
    }
  })
  
  observeEvent(input$back, {
    # Add non-focus psnel stuff
    output$panelTitle <- NULL
    output$panelText <- NULL
    output$helpTitle <- renderText(helpTitle)
    output$helpText <- renderText(helpText)
    leafletProxy("map") %>% fitBounds(73, 3.5, 178, -51.5) %>% hideGroup(old_station_name)
  })
}