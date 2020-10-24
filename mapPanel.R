### Code for creating the panel used on the risk map
### Responds to map input 

library(lubridate)
library(plotly) # Interactive graph

# Read data
weather2020 <- read.csv("data/weather2020.csv")
weather2020$Date <- as.Date(weather2020$Date)
# Used to compare current weather to monthly averages
monthlyMeans <- read.csv("data/monthlyMeans.csv")

currDate <- Sys.Date()

# HTML to display before any station is clicked
helpTitle <- "<h1>&nbsp;Bushfire Risk Map</h1>"
helpText <- "</br>
<p>This map shows current predictions of bushfire risk throughout Australia. Each cell represents a forecast for one station.</p>
<p>Click on a cell to see a more detailed breakdown of bushfire risk in that station's area.</p>
<p><b>DISCLAIMER:</b> This is not supposed to override government advice. Please always consult your local authority before making decisions. Links to each states official government advice can be found on the home page</p>
<p><br><h4>Risk Level</h4><b>Very Low and Low</b></br>Be aware of your bushfire plan and monitor the situation closely. Unlikely to need immediate attention<br><b>Moderate</b></br>Leaving early is always the safest option. Please check government advice.</br><b>High and Very High</b></br>Please immediately seek government advice. Fire risk is high and your bushfire plan should be followed.</p>
"

# Initialise the panel and its outputs
panel <- absolutePanel(id = "control", fixed = TRUE, width = 400, height ="auto",
                      top = 77, left = 16, right = "auto", bottom = 16, style = "overflow-y: auto; overflow-x: hidden",
          htmlOutput("helpTitle"),
          htmlOutput("helpText"),
          uiOutput("panelTitle"),
          htmlOutput("panelText"),
          plotlyOutput("panelPlot")
        )

# Server function for panel
# Sets outputs to null depending on current state of map 
mapPanelServer <- function(input, output) {
  # Simply render the HTML
  output$helpTitle <- renderText(helpTitle)
  output$helpText <- renderText(helpText)
  
  # When a shape on the map is clicked
  mapPanelClickEvent <- observeEvent(input$map_shape_click, {
    event <- input$map_shape_click
    newStationName <- shapes$stationNam[event$id] # Name of clicked station
    # Use short version of station name
    shortStationName <- shortStationNames$ShortName[shortStationNames$Name == newStationName]
    # This might be slow, should make faster if necessary
    stationWeather <- weather2020[weather2020$name == newStationName & weather2020$Date+4 >= currDate,]
    currWeather <- stationWeather[stationWeather$Date == currDate,]
    
    # If the click corresponds to a station, not a grid cell
    if (!is.null(event$id)) {
      output$helpTitle <- NULL
      output$helpText <- NULL
      
      # Render the panel title and the back button
      output$panelTitle <- renderUI({
        tagList(
          fluidRow(
            column(2, p(""), actionButton("back", "", icon=icon("arrow-left"))),
            column(8, h3(shortStationName))
            )
        )
      })
      
      # Get long-term monthly averages
      # Calculate average as weighted combination of current and previous months
      # This is so that, for example, 1st October uses primarily the average for September
      month1 <- monthlyMeans[monthlyMeans$Station == newStationName & monthlyMeans$Month == month(currDate),]
      month2 <- monthlyMeans[monthlyMeans$Station == newStationName & monthlyMeans$Month == month(currDate)-1,]
      currDay <- day(currDate)
      tempDiff <- round(currWeather$MaxTemp - month1$MaxTemp*currDay/30 - month2$MaxTemp*(30-currDay)/30, digits=1)
      windDiff <- round(currWeather$WindSpeed - month1$WindSpeed*currDay/30 - month2$WindSpeed*(30-currDay)/30, digits=1)
      humidityDiff <- round(currWeather$Humidity - month1$Humidity*currDay/30 - month2$Humidity*(30-currDay)/30, digits=1)
      rainDiff <- round(currWeather$MonthRainMean - month1$Rainfall*currDay/30 - month2$Rainfall*(30-currDay)/30, digits=1)
      
      # Panel text
      output$panelText <- renderUI({
        # Create text for the current weather
        # There may be a more compact way to do this
        if (tempDiff >= 0) {
          tempText <- paste(tempDiff, "above average")
        } else {
          tempText <- paste(abs(tempDiff), "below average")
        }
        if (windDiff >= 0) {
          windText <- paste(windDiff, "above average")
        } else {
          windText <- paste(abs(windDiff), "below average")
        }
        if (humidityDiff >= 0) {
          humidityText <- paste(humidityDiff, "above average")
        } else {
          humidityText <- paste(paste(abs(humidityDiff),'%',sep=""), "below average")
        }
        if (rainDiff >= 0) {
          rainText <- paste(rainDiff, "above average")
        } else {
          rainText <- paste(abs(rainDiff), "below average")
        }
        # Create HTML
        tempText <-     paste("<h3 style='display: inline'>",icon('temperature-high'),paste(round(currWeather$MaxTemp,digits=1),'\u00b0',sep=""),"</h3>",'max &nbsp;&nbsp;(',tempText,')**')
        windText <-     paste("<h3 style='display: inline'>",icon('wind'),round(currWeather$WindSpeed,digits=1),"</h3>",'km/h &nbsp;&nbsp;(',windText,')**')
        humidityText <- paste("<h3 style='display: inline'>",icon('tint'),paste(round(currWeather$Humidity,digits=1),'%',sep=""),"</h3>",'humidity &nbsp;&nbsp;(',humidityText,')**')
        rainText <-     paste("<h3 style='display: inline'>",icon('cloud-showers-heavy'),paste(round(currWeather$MonthRainMean,digits=1),'*',sep=""),"</h3>",'mm &nbsp;&nbsp;(',rainText,')**')
        str1 <- paste(paste("</br>&nbsp;&nbsp;&nbsp;&nbsp",tempText), windText, humidityText, rainText, sep="</br></br>&nbsp;&nbsp;&nbsp;&nbsp;")
        str1 <- paste(str1, "</br></br>&nbsp;*&nbsp; Average over last 30 days</br>&nbsp;** Values compared to long-term monthly averages</br></br>")
        HTML(paste(str1))
        })
      
      # Risk plot
      veg <- shapes$vegetation[event$id] # Get risk for stations average vegetation
      colName <- paste("Prediction", veg, sep="")
      
      # Create the prediction plot
      plot <- ggplot(data=stationWeather, aes_string(x="Date", y=colName, # Below necessary for tooltips
                                                     Temperature=stationWeather$MaxTemp, Wind=stationWeather$WindSpeed, 
                                                     Humidity=stationWeather$Humidity, Rainfall=stationWeather$Rainfall,
                                                     Date=stationWeather$Date, Risk=round(stationWeather[,colName],digits=2))) +
              geom_line(color="red") + geom_point() + ylim(0,1) + 
              labs(title = paste("Bushfire Risk:", shortStationName), x="Date", y="Risk") +
              theme(plot.title = element_text(hjust=0.5))
      
      # Convert to Plotly plot
      output$panelPlot <- renderPlotly(ggplotly(plot, tooltip = c("Risk", "Temperature", "Rainfall", "Humidity", "Wind")) %>%
                                       config(displayModeBar=F) %>% # Disable toolbar
                                       layout(xaxis = list(fixedrange=T), yaxis = list(fixedrange=T))) # Force fixed range
    }
  })
  
  # When the back button is clicked
  observeEvent(input$back, {
    # Remove station output
    output$panelTitle <- NULL
    output$panelText <- NULL
    output$panelPlot <- NULL
    
    # Render HTML
    output$helpTitle <- renderText(helpTitle)
    output$helpText <- renderText(helpText)
    
    # Reset zoom and hide grid cells
    leafletProxy("map") %>% fitBounds(73, 3.5, 178, -51.5) %>% hideGroup(old_station_name)
  })
}
