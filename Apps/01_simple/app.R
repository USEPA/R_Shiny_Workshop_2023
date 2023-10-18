# EPA R User Workshop 2023
# A Simple Shiny Application

# Set working directory while working
#setwd("01_simple")


# Step 1: load packages and data you will need
library(shiny)
library(sf)
library(tidyverse)
library(leaflet)

sf <- st_read("data/Hurricane.gpkg", layer = "Ian")

# Define UI for application that draws a histogram
ui <- fluidPage(
  selectInput("fCast","Select a Forecast", sf$fLabel, sf$fLabel[1]),
  leafletOutput("map")
)

# Define server logic required to render a map
server <- function(input, output) {

  # Filter the hurricane tracks
  sf.filt <- reactive(
    sf%>%
      filter(fLabel == input$fCast)
  )
  
  # Render the map
  output$map <- renderLeaflet(
    leaflet(sf.filt())%>%
      addTiles()%>%
      addPolygons())
}

# Run the application 
shinyApp(ui = ui, server = server)
