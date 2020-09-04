setwd('C:/Users/Mehak Minocha/Desktop/bushfiretracker')
library(shiny)
library(shinydashboard)
library(ggplot2)

nFires <- read.csv("CorrelationData.csv")

ui = dashboardPage(
  dashboardHeader(title = "Bushfire Tracker"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Home", tabName = "home", icon=icon("dashboard")),
      menuItem("Previous Trends", tabName = "previoustrends", icon=icon("th")),
      menuItem("Predictions", tabName = "predictions", icon=icon("th")),
      menuItem("Bushfire Alerts", tabName = "alert", icon=icon("th"))
    )
  ),
  dashboardBody(
    tabItems(
      #Home page
      tabItem(tabName = "home",
              tags$image(src="img/home_pic.jpeg")),
      
      #Bushfire Alert Tab
      tabItem(tabName = "previoustrends",
              h2("Previous Trends"), 
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
              plotOutput("corrPlot")),
      tabItem(tabName = "predictions",
              h2("Predictions"),
              infoBoxOutput("output1")),
      tabItem(tabName = "alert",
              h2("Bushfire Alerts"))
      
    ),
  ),
  
  skin="red")


server <- function(input, output){
  #correlation plot
  output$corrPlot <- renderPlot({
    stateName <- input$State  #selected data
    selectedData <- nFires[which(nFires$State==stateName),]  #just that state 
    
    scatter<- ggplot(selectedData, aes(x=Date, y=nFire, color = Temperature)) +
      geom_point(alpha=0.7) + ggtitle(paste("Number of Fires and Temperature Over Time in "), stateName) +
      ylab("Number of Fires") + xlab("Month")
    scatter + scale_color_gradient(low="white", high="red")
    
  })
}

shinyApp(ui,server)
