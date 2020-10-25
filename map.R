### Code for risk map

# Create the map panel
source("mapPanel.R", local=TRUE)

# Shortened station name, so that they fit on one line
shortStationNames <- read.csv("data/shortStationNames.csv")
# To map vegetation numbers to descriptions
vegetation_names <- read.csv("data/vegetation_names.csv")

# Breaks for the different risk classes
breaks <- c(0, 0.2, 0.4, 0.6, 0.8, 1)
class_names <- c("Very Low", "Low", "Moderate", "High", "Very high")

# Read station shapefile
shapes <- readOGR("data/voronoi/voronoi.shp")
pal <- colorBin("YlOrRd", c(0,1), bins=5) # Define colour scheme
shapes$class <- cut(shapes$spice, breaks=breaks, labels=class_names) # Sort risks into classes
# Create strings for tooltips
labels <- sprintf("<strong>%s</strong><br/>%s risk (%g)", 
                  shortStationNames$ShortName[match(shapes$stationNam, shortStationNames$Name)], shapes$class, round(shapes$spice,digits=2)) %>% lapply(htmltools::HTML)

# Read grid cells
cells_df <- readOGR("data/grid/voronoi.shp")
cells_df$class <- cut(cells_df$spice, breaks=breaks, labels=class_names) # Sort risks into classes
# List of stations that have already been clicked, to avoid duplicating cell shape objects
clicked_ids <- c()
old_station_name <- "-1" # Placeholder
focused <- FALSE # Is a station currently focused?

# Options for Leaflet
basemapOptions <- leafletOptions(zoomSnap = 0.25, doubleClickZoom = FALSE, zoomControl = FALSE)

# Create leaflet object
basemap <- leaflet(shapes, options = basemapOptions) %>%
  addProviderTiles(providers$Esri.WorldGrayCanvas) %>% # World tiles
  fitBounds(73, -6.4, 178, -45.4) %>% # Rough bounds of Australia
  #setMaxBounds(73, 3.5, 178, -51.5) %>% # This is unreliable at different resolutions
  # Create legend
  addLegend(pal = pal, value = ~spice, opacity = 0.7, title = "Risk", position = "bottomright",
            labFormat=function(type, cuts, p){paste0(class_names)}) %>% # Weird hack to get class names as labels
  # Add station shapes, with styling and tooltips
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

# Create tab, and container for map
mapTab <- tabPanel("Risk map",
      div(class="outer",
        tags$head(includeCSS("styles.css")),
        leafletOutput("map", width="100%", height="100%"),
        panel
      )
    )

# Server function for leaflet map
mapServer <- function(input, output) {
  # Map output
  output$map <- renderLeaflet({
    basemap
  })
  
  # Store name of clicked station
  # TODO Is this necessary
  # TODO comment rest of file
  output$focused <- reactive({input$map_shape_click$group}) 
  
  # When a shape is clicked
  mapClickEvent <- observeEvent(input$map_shape_click, {
    event <- input$map_shape_click
    new_station_name <- shapes$stationNam[event$id]
    focused <<- TRUE
    if (is.null(event$id)){
      # Do nothing
    } else if (event$id %in% clicked_ids) {
      bbox <- shapes[event$id,]@bbox
      leafletProxy("map") %>% hideGroup(old_station_name) %>%
      showGroup(new_station_name) %>%
      flyToBounds(bbox[1]-0.5, bbox[2]-0.5, bbox[3]+0.5, bbox[4]+0.5) # zoom to shape
      old_station_name <<- new_station_name
    } else {
      clicked_ids <<- c(clicked_ids, event$id)
      cells <- cells_df[cells_df$id == event$id,]
      
      cell_labels <- sprintf("<strong>%s</strong><br/>%s risk (%g)", vegetation_names$Name[cells$vegetation+1], cells$class, round(cells$spice,digits=2)) %>% lapply(htmltools::HTML)
      bbox <- shapes[event$id,]@bbox
      
      leafletProxy("map", data=cells) %>% addPolygons(
        group = new_station_name,
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
        label = cell_labels,
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "15px",
          direction = "auto"
        )
      ) %>% 
      hideGroup(old_station_name) %>%
      flyToBounds(bbox[1]-0.5, bbox[2]-0.5, bbox[3]+0.5, bbox[4]+0.5) # zoom to shape
      old_station_name <<- new_station_name
    }
  })
  mapPanelServer(input, output)
}