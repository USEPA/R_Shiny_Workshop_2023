# Exercise 6: Crosstalk
library(shiny)
library(sf)
library(tidyverse)
library(leaflet)
library(crosstalk)
library(d3scatter)

# Load some data
#setwd("Apps/04_sidebar/")
sf <- st_read("data/spatial.gpkg", layer = "counties")
df <- st_drop_geometry(sf)

# Define UI for application that draws a histogram
ui <- fluidPage(
  titlePanel("Adding a Sidebar Layout"),
  sidebarPanel(
    selectInput("state","Select a State",
                choices = unique(sf$State), selected = "Ohio"),
    hr(),
    h3("Left Plot Options"),
    selectInput("x","Choose X Variable",choices = colnames(sf)[4:12], selected = "Med_House_Val"),
    selectInput("y","Choose y variable",choices = colnames(sf)[4:12], selected = "Population"),
    hr(),
    h3("Right Plot Options"),
    selectInput("x2","Choose X Variable",choices = colnames(sf)[4:12], selected = "White_alone"),
    selectInput("y2","Choose y variable",choices = colnames(sf)[4:12], selected = "Black_African_American"),
    h4("Index: "),textOutput("index"),
    h4("Columns: "),textOutput("colnames")
  ),
  mainPanel(
    fluidRow(
      column(6,
             d3scatterOutput("lplot")),
      column(6,
             d3scatterOutput("rplot"))
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  # Get column numbers
  col.nums <- reactive(match(c(input$x,input$y,input$x2,input$y2),names(df)))
  # create a new data frame with selected variables
  df.sel <- reactive(
    df%>%
      select(col.nums())%>%
      setNames(c("X","Y","X2","Y2"))
  )
  
  # Filter our spatial file
  shared.df <- SharedData$new(df.sel)
  
  # left Plot
  output$lplot <- renderD3scatter({
    d3scatter(shared.df, ~X,~Y, width = "100%")
  })
  

  # Right Plot
  output$rplot <- renderD3scatter({
    d3scatter(shared.df, ~X2,~Y2, width = "100%")
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

