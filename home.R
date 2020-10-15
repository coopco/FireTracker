welcomeText <- "<h1><center>Welcome to Bushfire Tracker</center></h1><br>"

introText <- "<p>Bushfire are a significant issue that plagues the Australian public; this website aims to provide clear information and predictions about fire risk across the country.<br><br>
  Firstly, you can look at the relationship between temperature and bushfires over time on our ‘Previous Trends’ page. This provides 8 years of historical data in order to help identify trends. <br><br>
  Additionally, our ‘Risk Map’ page predicts the risk of a fire in a given area. Our predictions are based off rainfall, wind speed, humidity and temperature from weather stations across the country. This is similar to the Forest Fire Danger Index used by the government. <br><br>
  <b>DISCLAIMER:</b> This service is aimed to provide clear information to the public, but is in no way supposed to override government advice. Always consult your official local fire rating and following evacuation guidance. Links to individual state emergency websites can be found below.<br><br></p>"

faqText <- '<p><h4><b>FAQ</b></h4>
            <b>When clicking a location on the map how precise is the location?</b></br>
            The location clicked on, on the map gives results based on the nearest station.</br>
            <b>Where was the historical data retrieved from?</b></br>
            The temperature data was found from <a href="https://www.kaggle.com/jsphyg/weather-dataset-rattle-package">Kaggle</a>. The historic bushfire data was acquired from <a href="https://firms.modaps.eosdis.nasa.gov/">NASA.</a></br>
            <b>Can specific locations be chosen?</b></br>
            The locations are specific to a station not every location in Australia. </br>
            <b>Which locations can be viewed in the prediction tab?</b></br>
            New South Wales, Victoria, Northern Territory, Western Australia, Tasmania, Victoria and Queensland.</br>
            <b>What does the temperature and bushfire against time graph show?</b></br>
            This graph shows the number of fires from 2008 to 2016 in a particular state with a darker coloured circle representing higher temperatures.</p>'

linkText <- '<br><h4><b>Links to Government Fire Warnings</b></h4>
            <a href="http://emergency.vic.gov.au/respond/">Victoria</a></br>
            <a href="https://www.rfs.nsw.gov.au/fire-information/fires-near-me">New South Wales</a></br>
            <a href="https://www.cfs.sa.gov.au/site/warnings_and_incidents.jsp">South Australia</a></br>
            <a href="https://www.emergency.wa.gov.au/">Western Australia</a></br>
            <a href="http://www.fire.tas.gov.au/map">Tasmania</a></br>
            <a href="https://www.ruralfire.qld.gov.au/map/Pages/default.aspx">Queensland</a></br>
            <a href="https://pfes.nt.gov.au/fire-and-rescue-service/fire-incident-map">Northern Territory</a><br>'

homeTab <- tabPanel("Home", tabName = "home",
                      htmlOutput("welcomeText"),
                          htmlOutput("introText"),
                          htmlOutput("faqText"),
                          htmlOutput("linkText")
                          )    

homeTabServer <- function(input, output) {
  output$welcomeText <- renderText(welcomeText)
  output$introText <- renderText(introText)
  output$faqText <- renderText(faqText)
  output$linkText <- renderText(linkText)
}
                          
