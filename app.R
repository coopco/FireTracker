library(shiny)
library(magrittr)
library(leaflet)
library(maps)
library(RColorBrewer)
library(shinythemes)
library(geojsonio)
library(htmltools)
library(ggplot2)
library(rgdal)
source("map.R", local=TRUE)

nFires <- read.csv("data/CorrelationData.csv")
nFires$Date <- as.Date(nFires$Date)

ui <- bootstrapPage(
  navbarPage(theme=shinytheme("flatly"), collapsible = TRUE, id="nav", title="Fire Tracker",
    mapTab,
    tabPanel("Previous trends",
      #tabs to select data in graph
      selectInput("State", "State:",
                c("All" = "ALL",
                  "Queensland" = "QLD",
                  "Victoria" = "VIC",
                  "New South Wales" = "NSW",
                  "Northern Territory" = "NT",
                  "South Australia" = "SA",
                  "Tasmania" = "TAS")),
      #show correlation plot
      plotOutput("corrPlot", width = "50%")
    )
  )
)

server <- function(input, output) {
  mapServer(input, output)
  
  #correlation plot
  output$corrPlot <- renderPlot({
    stateName <- input$State  #selected data
    selectedData <- nFires
    if(stateName!="ALL"){ #if not all states, take subset of data
      selectedData <- nFires[which(nFires$State==stateName),]  #just that state 
    }
    
    scatter <- ggplot(selectedData, aes(x=Date, y=nFire, color = Temperature)) +
      geom_point(alpha=0.7) + ggtitle(paste("Number of Fires and Temperature Over Time in "), stateName) +
      ylab("Number of Fires") + xlab("Month")
    scatter + scale_color_gradient(low="white", high="red")
    
  })
}

shinyApp(ui, server)
