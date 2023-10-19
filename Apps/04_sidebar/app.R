# Exercise 4: layouts
library(shiny)
library(sf)
library(tidyverse)
library(leaflet)

# Load some data
#setwd("Apps/04_sidebar/")
sf <- st_read("data/spatial.gpkg", layer = "counties")

# Define UI for application that draws a histogram
ui <- fluidPage(
  titlePanel("Adding a Sidebar Layout"),
  sidebarPanel(
  selectInput("state","Select a State",
              choices = unique(sf$State), selected = "Ohio"),
  selectInput("x","Choose X Variable",choices = colnames(sf)[4:12], selected = "Med_House_Val"),
  selectInput("y","Choose y variable",choices = colnames(sf)[4:12], selected = "Population")
  ),
  mainPanel(
    plotOutput("plot"),
    leafletOutput("map")
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  # Filter our spatial file
  sf.filt <- reactive({
    sf%>%
      filter(State == input$state)
  })
  
  # Create a map
  output$map <- renderLeaflet(
    leaflet(sf.filt())%>%
      addTiles()%>%
      addPolygons()
  )
  
  # Create a plot
  output$plot <- renderPlot(
    ggplot(sf.filt())+
      geom_point(aes_string(x = input$x, y = input$y))
  )
}

# Run the application 
shinyApp(ui = ui, server = server)

