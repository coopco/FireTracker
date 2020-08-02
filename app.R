library(shiny)
library(magrittr)
library(leaflet)
library(maps)
library(RColorBrewer)
library(shinythemes)
library(geojsonio)

pal = colorNumeric("YlOrRd", c(0:72))
hotspots = geojson_read("data/recenthotspots.json", what="sp")
hotspotClusterOptions = markerClusterOptions(maxClusterRadius = 40)

basemapOptions = leafletOptions(zoomSnap = 0.25, minZoom = 4.75, zoomControl = FALSE)

basemap = leaflet(hotspots, options = basemapOptions) %>%
  #addProviderTiles(providers$Esri.WorldGrayCanvas) %>%
  addTiles() %>%
  setView(123, -28, 4.75) %>%
  setMaxBounds(79.8, 1.47, 187.13, -56.13) %>%
  # Add markers
  addCircleMarkers(clusterOptions = hotspotClusterOptions,
    #radius = ~ifelse(power == -1, 6, floor(power)),
    color = ~pal(hours_since_hotspot),
    stroke = FALSE, fillOpacity = 0.8
  ) %>%
  # Add buttons
  #addEasyButtonBar(position = "bottomright",
  #  easyButton(position = "topright",
  #    icon="fa-plus", title="Zoom in",
  #    onClick=JS("function(btn, map) { map.zoomIn(); }")
  #  ),
  #  easyButton(position = "topright",
  #    icon="fa-minus", title="Zoom out",
  #    onClick=JS("function(btn, map) { map.zoomOut(); }")
  #  )
  #) %>%
  addEasyButton(easyButton(position = "topright",
    icon="fa-plus", title="Zoom in",
    onClick=JS("function(btn, map) { map.zoomIn(); }"))) %>%
  addEasyButton(easyButton(position = "topright",
    icon="fa-minus", title="Zoom out",
    onClick=JS("function(btn, map) { map.zoomOut(); }"))) %>%
  addEasyButton(easyButton(position = "topright",
    icon="fa-globe", title="Reset zoom",
    onClick=JS("function(btn, map){ map.setView([-28, 133], 4.75); }"))) %>%
  addEasyButton(easyButton(position = "topright",
    icon="fa-crosshairs", title="Locate Me",
    onClick=JS("function(btn, map){ map.locate({setView: true}); }")))

ui <- bootstrapPage(
  navbarPage(theme=shinytheme("flatly"), collapsible = TRUE, id="nav", title="Fire Tracker",
    tabPanel("Risk map",
      div(class="outer",
        tags$head(includeCSS("styles.css")),
        leafletOutput("mymap", width="100%", height="100%"),
        
        absolutePanel(id = "control", fixed = TRUE, width = 400, height ="auto",
                      top = 77, left = 16, right = "auto", bottom = 16,
          h2("Cool fire stats"),
          p("test etest testset")
        )
      )
    )
  )
)

server <- function(input, output) {
  output$mymap <- renderLeaflet({
    basemap
  })
}

shinyApp(ui, server)
