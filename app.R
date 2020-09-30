library(shiny)
library(magrittr)
library(leaflet)
library(maps)
library(RColorBrewer)
library(shinythemes)
library(geojsonio)
library(htmltools)
library(ggplot2)
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
  output$map <- renderLeaflet({
    basemap
  })
  
  output$text <- renderText ({
    event <- input$map_shape_click
    if (!is.null(event)) {
      sprintf("%s has been selected", shapes$stationNam[event$id])
    } 
  })
  
  observeEvent(input$map_shape_click, {
    event <- input$map_shape_click
    new_station_name <- shapes$stationNam[event$id]
    if (is.null(event$id)){
      # Do nothing
    } else if (event$id %in% clicked_ids) {
      bbox <- shapes[event$id,]@bbox
      leafletProxy("map") %>% hideGroup(old_station_name) %>%
      showGroup(new_station_name) %>%
      fitBounds(bbox[1], bbox[2], bbox[3], bbox[4]) # zoom to shape
    } else {
      clicked_ids <<- c(clicked_ids, event$id)
      cells <- cells_df[cells_df$id == event$id,]
      cell_labels <- sprintf("<strong>%s</strong><br/>%g%% risk", rep(shapes$stationNam[event$id]), cells$spice*100) %>% lapply(htmltools::HTML)
      bbox <- shapes[event$id,]@bbox
      leafletProxy("map", data=cells) %>% addPolygons(
          group = new_station_name,
          fillColor = ~cells_pal(func(spice)), #TODO change
          weight = 2,
          opacity = 1,
          color = "white",
          dashArray = "3",
          fillOpacity=0.7,
          highlight = highlightOptions(
            weight = 5,
            color = "white",
            dashArray = "",
            fillOpacity = 0.7,
            bringToFront = TRUE
          )
        ) %>% 
      hideGroup(old_station_name) %>%
      flyToBounds(bbox[1], bbox[2], bbox[3], bbox[4]) # zoom to shape
    }
    old_station_name <<- new_station_name
  })
  
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
