library(shiny)
library(ggplot2)
library(dplyr)

# Load some data
data(iris)

# Define UI for application that draws a histogram
ui <- fluidPage(
  checkboxGroupInput("species.check",
              "Select Species",
              unique(iris$Species),
              unique(iris$Species)),
  actionButton("execute","Apply Filter"),
  # Show a plot of the generated distribution
  plotOutput("plot")

)

# Define server logic required to draw a histogram
server <- function(input, output) {

  
#######################  
# REACTIVE EXPRESSION #
#######################
  
  # Trying to use a reactive expression
    output$plot <- reactive(
      renderPlot(
        df.filt <- iris%>%
          filter(Species %in% input$species.check)%>%
          ggplot(df.filt)+
          geom_point(aes(x = Sepal.Length, y = Petal.Length, color = Species))
      )
    )
    
############    
# OBSERVER #
############
    
    # Using an observer
    # observe({
    #   df.filt <- iris%>%
    #     filter(Species %in% input$species.check)
    # 
    #   output$plot <- renderPlot(
    #     ggplot(df.filt)+
    #       geom_point(aes(x = Sepal.Length, y = Petal.Length, color = Species))
    #   )
    # })
    
#################    
# OBSERVE EVENT #
#################
    
    # Using observeEvent
    # observeEvent(input$execute,{
    #   df.filt <- iris%>%
    #     filter(Species %in% input$species.check)
    #   
    #   output$plot <- renderPlot(
    #     ggplot(df.filt)+
    #       geom_point(aes(x = Sepal.Length, y = Petal.Length, color = Species))
    #   )
    # })
}

# Run the application 
shinyApp(ui = ui, server = server)
