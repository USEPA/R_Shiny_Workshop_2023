# Exercise 5: Brushing
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
    hr(),
    h3("Plot Options"),
    selectInput("x","Choose X Variable",choices = colnames(sf)[4:12], selected = "Med_House_Val"),
    selectInput("y","Choose y variable",choices = colnames(sf)[4:12], selected = "Population"),
    h4("Index: "),textOutput("index"),
    h4("Columns: "),textOutput("colnames"),
    hr(),
    h3("Map Options"),
    selectInput("color","Color Map By:",choices = colnames(sf)[4:12], selected = "Med_House_Val")
  ),
  mainPanel(
    fluidRow(
      column(5,
             plotOutput("plot",
                        brush = brushOpts(
                          id = "plot_brush"))),
      column(7,
             leafletOutput("map"))
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  # Filter our spatial file
  sf.filt <- reactive({
    sf%>%
      filter(State == input$state)
  })
  
  
  # Create subset of points to map
  sf.brushed <- reactive({
    brushedPoints(sf.filt(), input$plot_brush, xvar = input$x,yvar = input$y)
  })
  
  # Create a map and color by a selected column
  observe({
    # Get column we have selected to use as the color
    col.color <- st_drop_geometry(sf.brushed())[, paste(input$color)]
    
    # generate a color palette
    pal <- colorNumeric(
      palette = "Blues",
      domain = col.color)
    
    # Put color into the sf object
    sf.map <- sf.brushed()
    sf.map$color <- pal(col.color)
    
    # Create Map
    output$map <- renderLeaflet(
      leaflet(sf.map)%>%
        addTiles()%>%
        addPolygons(color = "#3c3d3c",stroke = TRUE,weight = 2,
                    fillColor = sf.map$color, fillOpacity = 1,
                    popup = paste0("<strong>",sf.map$County,"</strong><br>",
                                  "Population:",format(sf.map$Population,big.mark = ","),"<br>",
                                  "Median Home Value: $",format(sf.map$Med_House_Val,big.mark = ",")))%>%
        addLegend(pal = pal, values = col.color, title = input$color)
    )
  })
  
  
  # Create a plot
  output$plot <- renderPlot(
    ggplot(sf.filt())+
      geom_point(aes_string(x = input$x, y = input$y))
  )
}

# Run the application 
shinyApp(ui = ui, server = server)

