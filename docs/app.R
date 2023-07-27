#This based on the tutorial found here: https://programminghistorian.org/en/lessons/shiny-leaflet-newspaper-map-tutorial#setting-up-your-coding-environment-and-creating-the-shiny-application

#install.packages('shiny')
#install.packages('leaflet')
#install.packages('tidyverse')
#install.packages('sf')

#if trouble occurs in installing 'sf' package, install gdal by opening a terminal window and entering the following commands (may occur on a Mac)
#brew install pkg-config
#brew install gdal

#Create an Empty Shiny Application

#1. Set up an application folder with all of the necessary files for that application in their own folder.

#2. Make the app.R file (this one)

#3. Load relevant libraries
#library(tidyverse)
#library(shiny)
#library(sf)
#library(leaflet)
#library(shinydashboard)

#Packages commented out to avoid reinstallation everytime app is run.

#4. Load the datasets
occurrences = read_csv('occurrences.csv')
ascidians = read_csv('species.csv')
sites = read_csv('sites.csv')

occurrences %>%
  left_join(sites, by = c("SITEID" = "SITEID")) %>%
  left_join(ascidians, by = c("SpeciesID" = "SpeciesID"))

#Create UI element

ui = bootstrapPage(
  tags$style(type = "text/class", "html, body {width:100%;height:100%}"),
  leafletOutput("map", width = "100%", height = "100%"),
  absolutePanel(top = 10, right = 10,
                sliderInput('years', 'Years Observed', min = 1990, max=2020, value = c(2000, 2010), sep = ""),
                selectInput('Genus', 'Species', choices = c("Ascidiella aspersa" = 'Ascidiella', 
                                                            "Botrylloides violaceous" = 'Botrylloides', 
                                                            "Didemnum vexillum" = 'Didemnum',
                                                            "Styela clava" = 'Styela')),
                selectInput('State', 'State', choices = c("Connecticut", "Maine", "Massachusetts", "New Hampshire", "New York", "Rhode Island")))
)                


server = function(input, output, session){
  #  #Reactive expression for the data subsetted to what the user selected
  #  filteredData <- reactive({
  #  occurrences[occurrences$Year >= input$years[1] & occurrences$Year <= input$years[1],]
  # })
  #  
  #  #Reactive expression that represents the selection function which changes 
  #  Locationinput <- reactive({
  #    occurrences[occurrences$Species == input$Genus,]
  #  })
  #  
  map_df = reactive({
    occurrences %>%
      filter(Year > input$years[1] & Year < input$years[2]) %>%
      filter(Genus == input$Genus) %>%
      filter(State == input$State) %>%
      count(SITEID, name = 'locations') %>%
      left_join(sites, by = c("SITEID" = "SITEID")) %>%
      left_join(ascidians, by =c("SpeciesID" = "SpeciesID")) %>%
      filter(!is.na(lat) & !is.na(lng)) %>%
      st_as_sf(coords = c('lng', 'lat')) %>%
      st_set_crs(4326)

  })
# 
  output$map = renderLeaflet({

    leaflet() %>%
      addTiles() %>%
      #setView(lng = -70, lat = 43, zoom = 6) %>%
    #fitBounds(~min(lng), ~min(lat), ~max(lng), ~max(lat))
     addCircleMarkers(data = map_df(), radius = ~sqrt(locations))
    })
#   
#   observe({
# 
#   })
}

shinyApp(ui, server)
# titlePanel("Ascidian Biofoulers in Northeastern United States (Long Island - Maine)"),
# sidebarLayout(
#   sidebarPanel = sidebarPanel(),
#   mainPanel = mainPanel(leafletOutput(outputId = 'map'))
# )

# ui = fluidPage(
#   header <-dashboardHeader(
#     title = "Ascidian Biofoulers"
#   ),
#   
# body <- dashboardBody(
#   fluidRow(
#     column(width = 9,
#            box(width = NULL, solidHeader = TRUE,
#                leafletOutput("busmap", height = 500)
#                ),
#            box(width = NULL,
#                uiOutput("numOccTable")
#                )
#            ),
#     column(width = 3,
#            box(width = NULL, status = "warning",
#                uiOutput("SpeciesSelect"),
#                checkboxGroupInput("Species", "Show",
#                                   choices = c(
#                                     Ascidiella = 1,
#                                     Botrylloides = 2,
#                                     Didemnum = 3,
#                                     Styela = 4
#                                   ),
#                                   selected = c(1,2,3,4)
#                                   ),
#                p(
#                  class = "text-muted",
#                  paste("Note: Those occurrences shown for each species only indicate known presences. Users should not assume that areas not showing occurrences indicate absence of the species.")
#                )
#                ),
#            actionButton("zoomButton", "Zoom to fit occurrences")
#            ),
#     box(width = NULL, status = "warning",
#         selectInput("State", "State where occurrences were observed",
#                     choices = c(
#                       "Connecticut",
#                       "Maine",
#                       "Massachusetts",
#                       "New Hampshire",
#                       "New York",
#                       "Rhode Island"
#                     ),
#       )
#     )
#   )
# ),
# 
# dashboardPage(
#   header,
#   dashboardSidebar(disable = TRUE),
#   body
# )
# )
# 
# #SpeciesColors <- c("1" = "#595490","2" = "#527525", "3" = "#A93F35", "4" = "#BA48AA")
# 
# server = function(input, output, session){
#   # Species select input box
#   output$SpeciesSelect <- renderUI({
#     
#     Sps <-
#       sort(unique(as.numeric(
#         realtime_locations(gtfs = gtfs)$Route
#       )))
#     
#     # Add names, so that we can add all=0
#     names(Sps) <- Sps
#     Sps <- c(All = 0, Sps)
#     selectInput("Sps", "Species", choices = Sps, selected = Sps[2])
#   })
#   # Data frame of Species for a particular state
#   SpeciesState <- reactive({
#     if (is.null(input$Sps))
#       return()
#     
#     locations <- vehicleLocations()
#     
#     if (as.numeric(input$routeNum) == 0)
#       return(locations)
#     
#     locations[locations$Route == input$routeNum, ]
#   })
# }
