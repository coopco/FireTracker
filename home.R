homeTab <- tabPanel("Home", tabName = "home",
                      div(h2("Welcome to the Bushfire Tracker"), 
                          h4("Bushfire are a significant issue that plagues the Australian public; this website aims to provide clear information and predictions about fire risk across the country.<br>
                              Firstly, you can look at the relationship between temperature and bushfires over time on our ‘Previous Trends’ page. This provides 8 years of historical data in order to help identify trends. <br>
                              Additionally, our ‘Risk Map’ page predicts the risk of a fire in a given area. Our predictions are based off rainfall, wind speed, humidity and temperature from weather stations across the country. This is similar to the Forest Fire Danger Index used by the government. <br>
                              <b>DISCLAIMER:</b> This service is aimed to provide clear information to the public, but is in no way supposed to override government advice. Always consult your official local fire rating and following evacuation guidance. Links to individual state emergency websites can be found below", align="left")
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
                          , h6("This graph shows the number of fires from 2008 to 2016 in a particular state with a darker coloured circle representing higher temperatures")
                          , tags$br(),
                          h4("Links to Government Fire Warnings:"),
                          h5("<a href="http://emergency.vic.gov.au/respond/">Victoria</a>"),
                          h5("<a href="https://www.rfs.nsw.gov.au/fire-information/fires-near-me">New South Wales</a>"),
                          h5("<a href="https://www.cfs.sa.gov.au/site/warnings_and_incidents.jsp">South Australia</a>"),
                          h5("<a href="https://www.emergency.wa.gov.au/">Western Australia</a>"),
                          h5("<a href="http://www.fire.tas.gov.au/map">Tasmania</a>"),
                          h5("<a href="https://www.ruralfire.qld.gov.au/map/Pages/default.aspx">Queensland</a>"),
                          h5("<a href="https://pfes.nt.gov.au/fire-and-rescue-service/fire-incident-map">Northern Territory</a>")
                          ))                                 
                          
