setwd('C:/Users/Mehak Minocha/Desktop/bushfiretracker')
library(shiny)
library(shinydashboard)
library(ggplot2)

nFires <- read.csv("CorrelationData.csv")
nFires$Date <- as.Date(nFires$Date)

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
    selectedData<- nFires
    if(stateName!="ALL"){ #if not all states, take subset of data
      selectedData <- nFires[which(nFires$State==stateName),]  #just that state 
    }
    else{
      stateName <- 'Australia'  #change so title says aus
    }
    scatter<- ggplot(selectedData, aes(x=Date, y=nFire, color = Temperature)) +
      geom_point(alpha=0.7) + ggtitle(paste("Number of Fires and Temperature Over Time in", stateName)) +
      ylab("Number of Fires") + xlab("Year") #plot
    scatter + scale_color_gradient(low="white", high="red") + theme(plot.background = element_rect(fill = '#ecf0f5', colour = '#ecf0f5'),
                                                                    panel.background = element_rect(fill = '#ecf0f5'),
                                                                    legend.background = element_rect(fill='#ecf0f5'),
                                                                    panel.grid.major = element_blank(), 
                                                                    panel.grid.minor = element_blank(),
                                                                    axis.line.x = element_line(colour = "grey", 
                                                                                               size=0.5, 
                                                                                               lineend = "butt"),
                                                                    axis.line.y = element_line(colour = "grey", 
                                                                                               size=0.5),
                                                                    plot.title = element_text(hjust = 0.5)) #format colour of plots, background, axis and title
  })
}

shinyApp(ui,server)

