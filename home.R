homeTab <- tabPanel("Home", tabName = "home",
                      div(h2("Welcome to the Bushfire Tracker"), 
                          h4("The bushfires are calculated based on the following factors wind speed, temperature and humidity. The bushfire tracker focuses on the relationship between temperature change and bushfires across Australia. The Bushfire Tracker provides bushfire predictions based on historic bushfire data from NASA and temperature data acquired from Kaggle. The Alert tab has a map of Australia which indicates bushfire danger.  Users are able to click on a location on the map for more information of the selected location. The Prediction tab shows the correlation between temperature and bushfires against time, which includes future predictions. ", align="center")
                          , tags$br(),
                          h4("FAQ"),
                          h5("When clicking a location on the map how precise is the location?"),
                          h6("The location clicked on, on the map gives results based on the nearest station. ")
                          ,h5("Where was the historical data retrieved from?")
                          , h6("The temperature data was found from Kaggle. The historic bushfire data was acquired from NASA")
                          , h5("Can specific locations be chosen?")
                          , h6("The locations are specific to a station not every location in Australia. ") 
                          , h5("Which locations can be viewed in the prediction tab?")
                          , h6("New South Wales, Victoria, Northern Territory, Western Australia, Tasmania, Victoria and Queensland")
                          , h5("What does the temperature and bushfire against time graph show?")
                          , h6("This graph shows the number of fires across the year in a particular state with a darker coloured circle representing higher temperatures")
                          ))                                 
                          