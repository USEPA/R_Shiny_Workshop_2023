
library(shiny)
library(tidycensus)
library(tigris)
library(tidyverse)
library(leaflet)

# Create data frame of state fips codes
fips <- select(fips_codes,state_name,state_code)%>%
  distinct()

cnty.sf <- counties()

# Define UI for application that draws a histogram
ui <- fluidPage(
  selectInput("state","Select a State",
              choices = fips$state_name, selected = "Ohio"),
  plotOutput("plot"),
  leafletOutput("map")
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  st.fips <- reactive({
    fips[fips$state_name == input$state,2]
  })
  cnty.filt <- reactive(
    cnty.sf%>%
      filter(STATEFP == st.fips())
  )
  output$map <- renderLeaflet(
    leaflet(cnty.filt())%>%
      addTiles()%>%
      addPolygons()
  )
  output$plot <- renderPlot(
    ggplot(cnty.filt())+
      geom_histogram(aes(x = ALAND))
  )
}

# Run the application 
shinyApp(ui = ui, server = server)
