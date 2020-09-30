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

shapes <- readOGR("data/voronoi/voronoi.shp")
pal <- colorNumeric("YlOrRd", domain = shapes$spice)
labels <- sprintf("<strong>%s</strong><br/>%g%% risk", shapes$stationNam, shapes$spice*100) %>% lapply(htmltools::HTML)

weather2020 <- read.csv("data/weather2020.csv")
currWeather <- weather2020[weather2020$Date == "2020-09-29",]

cells_df <- readOGR("data/grid/voronoi.shp")
#cells_df <- readOGR("data/cells/voronoi.shp")
cells_pal <- colorNumeric("YlOrRd", domain = c(0,1))
old_station_name <- "-1"
clicked_ids <- c()
#labels <- sprintf("<strong>%s</strong><br/>%g%% risk", shapes$id, shapes$vegetation*100) %>% lapply(htmltools::HTML)

basemapOptions <- leafletOptions(zoomSnap = 0.25, minZoom = 4.75, zoomControl = FALSE)

basemap <- leaflet(shapes, options = basemapOptions) %>%
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
  addLegend(pal = pal, value = ~spice, opacity = 0.7, title = "Risk", position = "bottomright") %>%
  addPolygons(
    group = "stations",
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


mapTab <- tabPanel("Risk map",
      div(class="outer",
        tags$head(includeCSS("styles.css")),
        leafletOutput("map", width="100%", height="100%"),
        
        absolutePanel(id = "control", fixed = TRUE, width = 400, height ="auto",
                      top = 77, left = 16, right = "auto", bottom = 16,
          h2("Cool fire stats"),
          textOutput("text")
          #includeMarkdown("mapHelp.md")         
        )
      )
    )