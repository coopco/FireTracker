library(shiny)
library(magrittr)
library(leaflet)
library(maps)
library(RColorBrewer)
library(shinythemes)
library(geojsonio)
library(htmltools)
library(ggplot2)

nFires <- read.csv("data/CorrelationData.csv")
nFires$Date <- as.Date(nFires$Date)
#pal = colorNumeric("YlOrRd", c(0:72))
#hotspots = geojson_read("data/recenthotspots.json", what="sp")
#hotspotClusterOptions = markerClusterOptions(maxClusterRadius = 40)

library(rgdal)
shapes = readOGR("data/voronoi/voronoi.shp")
pal <- colorNumeric("YlOrRd", domain = shapes$spice)
labels <- sprintf("<strong>%s</strong><br/>%g degrees", shapes$stationNam, shapes$spice) %>% lapply(htmltools::HTML)

basemapOptions = leafletOptions(zoomSnap = 0.25, minZoom = 4.75, zoomControl = FALSE)

basemap = leaflet(shapes, options = basemapOptions) %>%
  addProviderTiles(providers$Esri.WorldGrayCanvas) %>%
  #setView(123, -28, 4.75) %>%
  fitBounds(73, 3.5, 178, -51.5) %>%
  #setMaxBounds(73, 3.5, 178, -51.5) %>%
  # Add markers
  #addCircleMarkers(clusterOptions = hotspotClusterOptions,
  #  #radius = ~ifelse(power == -1, 6, floor(power)),
  #  color = ~pal(hours_since_hotspot),
  #  stroke = FALSE, fillOpacity = 0.8
  #) %>%
  # Add buttons
  addEasyButton(easyButton(position = "topright",
    icon="fa-plus", title="Zoom in",
    onClick=JS("function(btn, map) { map.zoomIn(); }"))) %>%
  addEasyButton(easyButton(position = "topright",
    icon="fa-minus", title="Zoom out",
    onClick=JS("function(btn, map) { map.zoomOut(); }"))) %>%
  addEasyButton(easyButton(position = "topright",
    icon="fa-globe", title="Reset zoom",
    onClick=JS("function(btn, map){ map.flyToBounds(map.fitBounds([[73,3.5],[178,-51.5]])); }"))) %>% # wtf
  addLegend(pal = pal, value = ~spice, opacity = 0.7, title = "Temp", position = "bottomright") %>%
  addPolygons(
    layerId = 1:length(shapes),
    fillColor = ~pal(spice),
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
    ),
    label = labels,
    labelOptions = labelOptions(
      style = list("font-weight" = "normal", padding = "3px 8px"),
      textsize = "15px",
      direction = "auto"
    )
  )

ui <- bootstrapPage(
  navbarPage(theme=shinytheme("flatly"), collapsible = TRUE, id="nav", title="Fire Tracker",
    tabPanel("Risk map",
      div(class="outer",
        tags$head(includeCSS("styles.css")),
        leafletOutput("mymap", width="100%", height="100%"),
        
        absolutePanel(id = "control", fixed = TRUE, width = 400, height ="auto",
                      top = 77, left = 16, right = "auto", bottom = 16,
          #h2("Cool fire stats"),
          h2(""),
          textOutput("mytext")
        )
      )
    ),
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
  output$mymap <- renderLeaflet({
    basemap
  })
  output$mytext <- renderText ({
    event <- input$mymap_shape_click
    test <- ifelse(is.null(event), "Nothing has been selected", 
                   sprintf("%s has been selected", shapes$stationNam[event$id]))
    
  })
  #observeEvent(input$mymap_shape_click, {
  #  event <- input$mymap_shape_click
  #  #leafletproxy
  #})
  
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
