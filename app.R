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
library(plotly)
source("map.R", local=TRUE)
source("PreviousTrends.R", local=TRUE)
source("home.R", local=TRUE)

ui <- bootstrapPage(
  navbarPage(theme=shinytheme("flatly"), collapsible = TRUE, id="nav", title="Fire Tracker",
    homeTab,
    mapTab,
    TrendsTab
  )
)

server <- function(input, output) {
  homeTabServer(input,output)
  mapServer(input, output)
  TrendsServer(input,output)  
}

shinyApp(ui, server)
