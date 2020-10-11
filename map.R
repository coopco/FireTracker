source("mapPanel.R", local=TRUE)

vegetation_names <- read.csv("data/vegetation_names.csv")
shortStationNames <- read.csv("data/shortStationNames.csv")

shapes <- readOGR("data/voronoi/voronoi.shp")
pal <- colorNumeric("YlOrRd", domain = shapes$spice)
labels <- sprintf("<strong>%s</strong><br/>%g%% risk", 
                  shortStationNames$ShortName[match(shapes$stationNam, shortStationNames$Name)], shapes$spice*100) %>% lapply(htmltools::HTML)

cells_df <- readOGR("data/grid/voronoi.shp")
cells_pal <- colorNumeric("YlOrRd", domain = c(0,1))
old_station_name <- "-1"
clicked_ids <- c()
#labels <- sprintf("<strong>%s</strong><br/>%g%% risk", shapes$id, shapes$vegetation*100) %>% lapply(htmltools::HTML)

focused <- FALSE

# TODO disable scroll wheel zoom
basemapOptions <- leafletOptions(zoomSnap = 0.25, minZoom = 4.75, doubleClickZoom = FALSE, scrollWheelZoom = FALSE,
                                 zoomControl = FALSE, dragging = FALSE)

basemap <- leaflet(shapes, options = basemapOptions) %>%
  addProviderTiles(providers$Esri.WorldGrayCanvas) %>%
  #setView(123, -28, 4.75) %>%
  fitBounds(73, -6.4, 178, -45.4) %>%
  #setMaxBounds(73, 3.5, 178, -51.5) %>%
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
        panel
      )
    )

mapServer <- function(input, output) {
  output$map <- renderLeaflet({
    basemap
  })
  
  output$text <- renderText ({
    event <- input$map_shape_click
    if (!is.null(event)) {
      sprintf("%s has been selected", shapes$stationNam[event$id])
    } 
  })
  
  output$focused <- reactive({input$map_shape_click$group}) 
  
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
      cell_labels <- sprintf("<strong>%s</strong><br/>%g%% risk", vegetation_names$Name[cells$vegetation+1], cells$spice*100) %>% lapply(htmltools::HTML)
      bbox <- shapes[event$id,]@bbox
      
      cells$spice2 = shapes$spice[event$id] * cells$spice/sum(cells$spice)
      
      leafletProxy("map", data=cells) %>% addPolygons(
        group = new_station_name,
        fillColor = ~cells_pal(spice), #TODO change
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