nFires <- read.csv("data/CorrelationData.csv")
nFires$Date <- as.Date(nFires$Date)

TrendsTab <- tabPanel("Previous Trends", tabName = "previoustrends",
                      div(h2("Previous Trends"), 
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
                          plotlyOutput("corrPlot")))

TrendsServer <- function(input, output){
  #correlation plot
  output$corrPlot <- renderPlotly({
    stateName <- input$State  #selected data
    selectedData<- nFires
    if(stateName!="ALL"){ #if not all states, take subset of data
      selectedData <- nFires[which(nFires$State==stateName),]  #just that state 
    }
    else{
      stateName <- 'Australia'  #change so title says aus
    }
    scatter<- ggplot(selectedData, aes(x=Date, y=Fires, color = Temperature)) +
      geom_point(alpha=0.7) + ggtitle(paste("Number of Fires and Temperature Over Time in", stateName)) +
      ylab("Number of Fires") + xlab("Year") #plot
    ggplotly(scatter + scale_color_gradient(low="white", high="red") + theme(plot.background = element_rect(fill = '#ecf0f5', colour = '#ecf0f5'),
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
             
    )
  })
}
