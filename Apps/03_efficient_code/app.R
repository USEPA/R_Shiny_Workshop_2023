# Exercise 3 (R Shiny Workshop)
library(shiny)
library(tigris)
library(tidyverse)
library(leaflet)

# Create data frame of state fips codes
fips <- select(fips_codes,state_name,state_code)%>%
  distinct()

# Define UI for application that draws a histogram
ui <- fluidPage(
  selectInput("state","Select a State",
              choices = fips$state_name, selected = "Ohio"),
  plotOutput("plot"),
  leafletOutput("map")
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  # Get the state fips code from the state input
  st.fips <- reactive({
    fips[fips$state_name == input$state,2]
  })
  
  # observer
  observe({
    
    # Download County data for selected state
    cnty.sf <- counties(st.fips())
    
    # Render a map
    output$map <- renderLeaflet(
      leaflet(cnty.sf)%>%
        addTiles()%>%
        addPolygons()
    )
    
    # Render a plot
    output$plot <- renderPlot({
      ggplot(cnty.sf)+
        geom_histogram(aes(x = ALAND))
    })
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
