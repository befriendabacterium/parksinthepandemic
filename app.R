# Set the root directory as the working directory

# Libraries
# ---------
library(rgdal)
#library(leaflet)
library(data.table)
library(tidyverse)
library(shinydashboard)
library(shinythemes)
library(shiny)
#library(shinyWidgets)
#library(leaflet.extras)
library(dashboardthemes)
library(htmlwidgets)
library(htmltools)
library(remotes)
library(osfr)
library(here)
library(conflicted)
library(randomForest)
conflict_prefer("box", "shinydashboard")

# Import functions from repo
# --------------------------

source("code/plot.googlemobilitydistricts.R")
source("code/read.googlemobility.R")
source("code/getRnumbers.R")
source("code/plot.parkvisits.R")

# Read data 
# ----------

googleandmetoffice_england<-read.csv('data/temporal/googleandmetoffice_england.csv')
RF_model<-readRDS('data/model/RF_model.RDS')  
forecast_england<-read.csv('data/model/forecasts_england.csv')
  
# make the map
#bedford <- readOGR(dsn="data/spatial", layer="TL_GreenspaceSite")
#shapeData <- spTransform(bedford, CRS("+init=epsg:4326"))
#UK_latlon <- readRDS("data/UK_dat_ggplot.RDS")
#UK_Mobility <- readRDS("data/UK_Mobility.RDS")

shapeData <- readOGR(dsn="data/spatial", layer="googleboundaries_WGS84")
#shapeData$NAME <- gsub( " *\\(.*?\\) *", "", shapeData$NAME)
#shapeData$NAME <- gsub( "City of ", "", shapeData$NAME)
#shapeData$NAME <- gsub( "The Brighton and Hove", "Brighton and Hove", shapeData$NAME)

#Rnumbers <- getRnumbers()

# Widgets
# -------

# this layout is very tidy 
text.box <- tabBox(id = "tabset1", height = "350px", width = 20,
                  tabPanel('Intro', span("Since the start of the COVID-19 pandemic in the UK, visits to outdoor parks and spaces have become even more valuable to the public. Getting outdoors allows us to maintain good physical and mental health in otherwise isolated times. However, the lack of alternative recreational opportunities during the pandemic means that parks are becoming busier - potentially making them hotspots for COVID-19 transmission. In order to help you understand the benefits and risks of visiting parks during the pandemic, we have created this app. It allows you to visualise how busy parks in your area have been over the course of the pandemic (Google Community Mobility Reports data; https://www.google.com/covid19/mobility/), and experimentally forecasts how busy parks will be over the coming days, using weather data (Met Office) and data on park visiting trends (Natural England) and access to greenspace (e.g. public parks, private gardens; Ordnance Survey).", style = "font-size:17px", width = 12), 
                           span("This is purely experimental - we do NOT take responsibility for any decisions made as a result of using this experimental app. See Disclaimer for more details.", style = "font-size:17px;font-weight:bold;", width = 12)),
                  tabPanel('Bar graph', span("Below you can see a graph of showing the relative busyness of parks on a day of the week (default current day of the week) in a particular district of England (default is Bedford) during the course of the pandemic. On the x axis is the date, on the y axis is the number of visitors to parks - Google data (https://www.google.com/covid19/mobility/) defined in terms of the number of visitors relative the the average (median) number of visitors on that day of the week in the winter period before the start of the pandemic (Jan 3 - Feb 6, 2020). For example, 2x as busy means parks are double as busy as the winter period, 0.5x means they are half as busy.", style = "font-size:17px;"),
                  span("Increases relative to the winter period are in green (e.g. 2x as busy).", style = "color:green; font-weight:bold; font-size:17px;"),
                  span("Decreases relative to the winter period are in grey (e.g. 0.5x as busy).", style = "color:grey; font-weight:bold; font-size:17px;"),
                  span("In white is the forecasted busyness for the next day of the week selected.", style = "color:black; font-weight:bold; font-size:17px;"),
                  span("The pink line is a scaled mean temperature change relative to the winter period.", style = "color:palevioletred; font-weight:bold; font-size:17px;"),
                  span("The blue line is a scaled mean rainfall change relative to winter (Met Office data used to inform the forecast).", style = "color:royalblue; font-weight:bold;font-size:17px;", width = 12)),
                  
                  tabPanel('Map', span("On the right, we see the same historical data (minus the forecast) displayed on a map of all of the Google regions.", style = "font-size:17px;"),
                  span("Darker shades of green represent areas where parks are the most busy compared to the winter period,", style = "color:green; font-weight:bold; font-size:17px;"), 
                  span("white regions represent areas where they are about as busy as winter,", style = "color:black; font-weight:bold; font-size:17px;"), 
                  span("darker shades of grey represent areas where parks are the least busy compared to the winter period.", style = "color:grey; font-weight:bold; font-size:17px;"),
                  span("This map allows you to compare the busyness of your region to others. On the sidebar on the left, you can also select the time period to view on the right, and it will change both the map and the timeline. You can also select the region or day of the week to view on the graph (or all of the data for a region). Enjoy (parks safely)!", style = "font-size:17px;", width = 12)),
                  tabPanel('Data', 'The primary data on park visits displayed in the app is Google data from the Google Community Mobility Reports - Google LLC Google COVID-19 Community Mobility Reports. https://www.google.com/covid19/mobility/
                  The map of the boundaries of the Google Community Mobility Reports UK districts are located at Open Science Framework (https://osf.io/c7kg4/). These shapefiles (named googleboundaries__XXXX) contain OS data (c) Crown copyright and database right 2020, derived from derived from OSopen boundary-line (https://www.ordnancesurvey.co.uk/business-government/products/boundaryline). 
                  Historical weather data plotted on the bar graph and used to inform the forecast model is supplied by the Met Office (c) Crown Copyright, 2020 as part of their their COVID-19 response https://metdatasa.blob.core.windows.net/covid19-response/index.html. This data is released under the open government license version 3.0. Contains Met Office data licensed under the Open Government Licence v3.0TM. For licence details details see: http://www.nationalarchives.gov.uk/doc/open-government-licence/version/3/
                  Natural England data used to inform the forecast model is the Visits data ( We http://publications.naturalengland.org.uk/file/6746346730291200) from Natural Englands  Monitor of Engagement with the Natural Environment. The national survey on people and the natural environment. Technical Report to the 2009-2019 surveys. This data is released under the open government license version 3.0. Contains Natural England data licensed under the Open Government Licence v3.0TM. For licence details details see: http://www.nationalarchives.gov.uk/doc/open-government-licence/version/3/
                  Greenspace data used to inform the forecast model contains OS data (c) Crown copyright and database right 2020 Contains Royal Mail data (c) Royal Mail copyright and Database right 2020 Contains National Statistics data  Crown copyright and database right 2020. This data is released under the open government license version 3.0. Contains OS, Royal Mail and National Statistics data licensed under the Open Government Licence v3.0TM. For licence details details see: http://www.nationalarchives.gov.uk/doc/open-government-licence/version/3/Available at https://www.ons.gov.uk/economy/environmentalaccounts/datasets/accesstogardensandpublicgreenspaceingreatbritain
                  Weather forecast data is from the OpenWeather 5-day/3 hour forecast weather API (https://openweathermap.org/api) obtained under a Free pricing plan.', style = "font-size:12px;", width = 12),
                  tabPanel('Team', 'Scarlett Kynoch (University of Exeter/back-end R programming); Dr. Matt Grainger (Norsk institutt for naturforsknin/front-end RShiny programming); Anna Bush (University of Exeter/front-end RShiny programming & Github); Dr. Lewis Elliot (University of Exeter/back-end R programming & OS greenspace/Natural England MENE data); Dr. Matt Lloyd Jones (University of Exeter/Project Manager & back-end R programming). We also thank Katherine Burgess, Beth Brockett & Rose ONeill from Natural England for their advice and support throughout.', style = "font-size:17px;", width = 12),
                  tabPanel('Disclaimer', 'This is an experimental app.  The historical data on park visits and the experimental forecast especially may not reflect the true conditions observed. Google states that their Community Mobility dataset is intended to help remediate the impact of COVID-19. It shouldnt be used for medical diagnostic, prognostic, or treatment purposes. It also isnt intended to be used for guidance on personal travel plans. (https://www.google.com/covid19/mobility/data_documentation.html?hl=en). We agree with this. We can therefore not take responsibility for an decisions made as a result of using this purely experimental app.', style = "font-size:17px;", width = 12)
)

date.box <- dateRangeInput("daterange1", "Date range:", start="2020-01-01", end="2021-01-01")

day.box<-selectInput("dayOfTheWeek", "Choose a day of the week", choices=c("Sunday"='1',"Monday"='2',"Tuesday"='3', "Wednesday"='4', "Thursday"='5', "Friday"='6', "Saturday"='7'),
                     selected=6, multiple = FALSE, selectize = TRUE, width=NULL, size=NULL)

place.box<-selectInput("place", "Choose a region", choices=unique(shapeData$Mblty_n)
                       , selected = "Bedford", multiple = FALSE, selectize = TRUE, width = NULL, size = NULL)

#baseline.check<-selectInput("custom_base", "Do you want a custom baseline?", choices=c("No", "Yes"), selected = "No")
plot.week<-selectInput("plot_week", "What do you want to plot?", choices=c("Per day of the week with forecast", "All historical data"), selected = "Per day of the week with forecast")

#baseline.box<-dateInput("basedate", "Date:", value = "2020-02-29")

graph <- plotOutput("plot1")
graph2<-plotOutput("plot2")

map <- plotOutput("map1", height=700*1.5, width=400*1.5)

#info.box <-infoBox("R value", value = paste0("England's R number is ", Rnumbers$Rnumbers.R_med[1]), subtitle = NULL,
   #     icon = shiny::icon("bar-chart"), color = "aqua", width = 4,
    #    href = TRUE, fill = FALSE)

#map <- leafletOutput("map1", height = 600)

# UI.R code
# ---------

header <- dashboardHeader(title="Parkcast", titleWidth = 250)


sidebar <- dashboardSidebar(date.box,place.box, day.box,
                            #baseline.check, 
                            plot.week
                            #,
                            #conditionalPanel("input.custom_base=='Yes'", 
                                       #      dateInput("basedate", "Date:", value = "2020-02-29")),
                            #width = 250
                            )

body <- dashboardBody(
  
  tags$head(tags$style(HTML('
                                /* main sidebar */
                                .skin-blue .main-sidebar {
                                background-color: #308759;
                                }
                                /* logo */
                                .skin-blue .main-header .logo {
                                background-color: #308759;
                                }
                                /* navbar (rest of the header) */
                                .skin-blue .main-header .navbar {
                                background-color: #308759;
                                }        

                                /* body */
                                .content-wrapper, .right-side {
                                background-color:	#FFFFFF ;
                                }

                                '))),
  fluidRow(
    column(
      6,
      text.box,
      conditionalPanel("input.plot_week=='All historical data'", 
      box(width=12, graph)),
      conditionalPanel("input.plot_week=='Per day of the week with forecast'", 
      box(width=12, graph2))
      #,
      #info.box
    ),
    column(6, box(width=12, map))
  )
)

# server
# ------

server <- function(input, output) {
  google_react<-reactive({
    googleandmetoffice_england %>% 
      dplyr::filter(dplyr::between(as.Date(date),min(as.Date(input$daterange1)), max(as.Date(input$daterange1)))) %>% 
      select(sub_region_1, date, parks_percent_change_from_baseline) %>% 
      group_by(sub_region_1) %>% 
      dplyr::summarise(mn=mean(parks_percent_change_from_baseline, na.rm=TRUE)) %>% 
      dplyr::mutate(Mblty_n=sub_region_1)
  })
  google_react2<-reactive({
    
    
    
    googleandmetoffice_england %>% 
      dplyr::filter(dplyr::between(as.Date(date),min(as.Date(input$daterange1)), max(as.Date(input$daterange1)))) %>% 
      dplyr::filter(sub_region_1==input$place)
  })
  
  shapeData2<-reactive({
    shapeData2 <- shapeData[shapeData$Mblty_n==input$place,]  
  })
  
  
  google_shp_merge<-reactive({merge(shapeData, google_react())})
  #map
  output$map1<-renderPlot({
    par(mar=c(3, 0, 3, 0))
    myscale<-grDevices::colorRampPalette(colors = c("darkgrey", "white", "darkgreen"))
    mycolours<-myscale(8)
    mybreaks <- c(-60,-40,-20,0, 20,40,60)
    #cut(google_shp_merge$mn, mybreaks)
    mycolourscheme <- mycolours[findInterval(google_shp_merge()$mn, vec = mybreaks)]
    plot(google_shp_merge(), xlim=c(-5.5,1.5), ylim=c(50,54.1), col = mycolourscheme)
    plot(shapeData2(), xlim=c(-5.5,1.5), ylim=c(50,54.1), add=TRUE, lwd=2, border='purple')
    
  })

  #plot
  
  
  
  output$plot1<-renderPlot({
    print(plot.googlemobilitydistricts(google_react2(), "parks", print(input$place)))})
  
  output$plot2<-renderPlot({
    print(plot.parkvisits(googleandmetoffice = googleandmetoffice_england, model=RF_model, forecast = forecast_england, 
                          district = input$place, dayofweek =as.numeric(input$dayOfTheWeek)))
    
    #plot.parkvisits(googleandmetoffice = googleandmetoffice, model=model, forecast = forecast, district = district,dayofweek = dayofweek)
    
  })

  #example plot function - MATT YOU WILL NEED TO EDIT IT TO MAKE IT REACTIVE TO DISTRICTS AND WEEKDAY
  #plot.parkvisits(googleandmetoffice = googleandmetoffice_england,
  #model = RF_model,
  #forecast = forecast_england)
  
  
}

# Run
# ---

shinyApp(
  ui = dashboardPage(header, sidebar, body),
  server = server
)
