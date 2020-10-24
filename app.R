### Main file
library(shiny)
library(magrittr)
library(leaflet)
library(maps)
library(RColorBrewer)
library(shinythemes)
library(htmltools)
library(ggplot2)
library(rgdal)
library(plotly)

# Source the different tabs
source("map.R", local=TRUE)
source("PreviousTrends.R", local=TRUE)
source("home.R", local=TRUE)

# Basic navbar page
ui <- bootstrapPage(
  navbarPage(theme=shinytheme("flatly"), collapsible = TRUE, id="nav", title="Fire Tracker",
    homeTab,
    mapTab,
    TrendsTab
  )
)

# Server functions
server <- function(input, output) {
  homeTabServer(input,output)
  mapServer(input, output)
  TrendsServer(input,output)  
}

shinyApp(ui, server)
