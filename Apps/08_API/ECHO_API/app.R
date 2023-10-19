# EPA R User Shiny Workshop - App #1

# Load libraries
library(shiny)
library(leaflet)
library(tidyverse)
library(pins)

# Load echo download function
source("functions/functions.R")

# Color palette for map
pal <- colorFactor(
  palette = c('#49d15b', '#2052a8', '#eb9e3b', '#3acfcc'),
  domain = c("CAA","CWA","RCRA","SDWA")
)


# Define User Interface (UI) for application
ui <- fluidPage(
  
  # Application title
  titlePanel("Extracting ECHO Data from Bounding Box"),
  
  # Sidebar with an action button to initiate data download. 
  sidebarLayout(
    sidebarPanel(
      actionButton("getECHO", "Get ECHO Facilities from map extent"),
      checkboxGroupInput("program",label = "Program",
                         choiceValues = c("CAA","CWA","RCRA","SDWA","Other"),
                         choiceNames = c("Clean Air Act","Clean Water Act","Resource Conservation & Recovery Act","Safe Drinking Water Act","Other"),
                         selected = c("CAA","CWA","RCRA","SDWA","Other")),
      checkboxGroupInput("compliance",label = "Compliance Status",
                         choices =c("Inactive","No Violation Identified","Significant Violation","Unknown","Violation","Violation Identified"),
                         selected = c("Inactive","No Violation Identified","Significant Violation","Unknown","Violation","Violation Identified")),
      actionButton("filter", "Apply Filters")
    ),
    
    # Show a map
    mainPanel(
      leafletOutput('simpleMap')
    )
  )
)

# Define server logic to download echo data and redraw map.
server <- function(input, output) {
  
  # Create a Simple Map which will show up when the app launches.
  output$simpleMap = renderLeaflet({
    leaflet() %>% 
      addTiles() %>% 
      setView(
        lng = -84.5,lat = 39.1, zoom = 14)
  })
  
  
  # Download ECHO Facility Data
  echo <- eventReactive(input$getECHO,{
    
    # Get bounding box coordinates from current map view
    extent <- as.numeric(input$simpleMap_bounds)
    
    # Create an sf object from the map extent
    mapcoords.df <- data.frame(lat = c(extent[1],extent[3]),lon = c(extent[2],extent[4]))
    poly <- mapcoords.df %>% 
      st_as_sf(coords = c("lon", "lat"), 
               crs = 4326) %>% 
      st_bbox()%>% 
      st_as_sfc()%>%
      st_sf()
    
    # Fetch ECHO Facilities
    return(echo_get_facilities(poly, drop_unknown_program = TRUE))})
  
  observe({
    sf.filtered <- echo()%>%
      filter(Program %in% input$program & RCRAComplianceStatus %in% input$compliance)
    
    output$simpleMap = renderLeaflet({
      leaflet(sf.filtered)%>% 
        addTiles()%>%
        addCircleMarkers(radius = 5, weight = 1,
                         color = "black", fillColor = ~pal(Program), fillOpacity = 1,
                         popup = ~paste(Name,"<br>",
                                        "Registry ID:",RegistryID,"<br>",
                                        "Compliance Status:",ComplianceStatus,"<br>",
                                        "Last Inspected:",DateLastInspection))%>%
        addLegend("bottomright", pal = pal, values = ~Program,
                  title = "EPA Rule",
                  opacity = 1
        )
    })
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)