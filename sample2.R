setwd('C:/Users/Mehak Minocha/Desktop/bushfiretracker')
library(shinydashboard)
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
              h2("Previous Trends")),
      tabItem(tabName = "predictions",
              h2("Predictions"),
              infoBoxOutput("output1")),
      tabItem(tabName = "alert",
              h2("Bushfire Alerts"))
          
    ),
     ),
  
   skin="red")

server = function(input, output){}

shinyApp(ui,server)
