#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(plotly)
library(leaflet)
library(DT)
library(bslib)
library(vroom)
library(shinyWidgets)

# Import Data

releases <- vroom("./data/Release_BlkGrp_Relate.csv")%>%
    mutate(Reported_Date = lubridate::ymd(Reported_Date))

ej <- vroom::vroom("./data/UST_Counts_by_Block_Group_STATE_EJ_Round.csv")%>%
    select(!c(GISJOIN))%>%
    mutate(N_P_OZONE = as.numeric(N_P_OZONE),
           N_P_PM25 = as.numeric(N_P_PM25),
           N_P_PRMP = round(N_P_PRMP),
           N_P_PTSDF= round(N_P_PTSDF),
           N_P_OZONE = round(N_P_OZONE),
           N_P_PM25 = round(N_P_PM25))

labels <- data.frame(Var = colnames(ej),label = c("Minority Population","% Minority","Low-Income Population","% Low-Income",
                                                  "Population Less Than High School","% Less Than High School","Linguistically Isolate Population",
                                                  "% Linguistically Isolated","Under 5 Population","% Under 5","65+ Population","% 65+","Homes Built Pre-1960",
                                                  "% Pre-1960 Homes","Minority & Low-Income Population","% Minority & Low-Income","Percentile Minority (Nat'l)",
                                                  "Percentile Low-Income (Nat'l)","Percentile Less Than High School (Nat'l)","Percentile Linguistically Isolated (Nat'l)",
                                                  "Percentile Under 5 (Nat'l)","Percentile 65+ (Nat'l)","Percentile Lead Paint (Nat'l)","Percentile Minority & Low-Income (Nat'l)",
                                                  "Percentile Diesel Particulate Matter (Nat'l)","Percentile Cancer (Nat'l)","Percentile Air Toxics (Nat'l)","Percentile Traffic (Nat'l)",
                                                  "Percentile Major Direct Discharge to Water (Nat'l)","Percentile Proximity to NPL Sites (Nat'l)","Percentile Proximity to RMP Sites (Nat'l)",
                                                  "Percentile Proximity to TSDF Sites (Nat'l)","Percentile O-Zone (Nat'l)","Percentile Particulate Matter 2.5 (Nat'l)","Land Area [m2]","ID",
                                                  "State","EPA Region","Population","Percentile Minority (State)",
                                                  "Percentile Low-Income (State)","Percentile Less Than High School (State)","Percentile Linguistically Isolated (State)",
                                                  "Percentile Under 5 (State)","Percentile 65+ (State)","Percentile Lead Paint (State)","Percentile Minority & Low-Income (State)",
                                                  "Percentile Diesel Particulate Matter (State)","Percentile Cancer (State)","Percentile Air Toxics (State)","Percentile Traffic (State)",
                                                  "Percentile Major Direct Discharge to Water (State)","Percentile Proximity to NPL Sites (State)","Percentile Proximity to RMP Sites (State)",
                                                  "Percentile Proximity to TSDF Sites (State)","Percentile O-Zone (State)","Percentile Particulate Matter 2.5 (State)","# Facilities",
                                                  "# Releases","# Tanks","Capacity (Gallons)","Median Tank Age","Latitude","Longitude","Releases to Tanks","Releases to Facilities",
                                                  "Tanks / km2","Facilities / km2","Releases / km2","Sate Ej Indexes > 80","State Ej Indexes > 95","National EJ Indexes > 80","National EJ Indexes > 95"))



# Define UI for application
ui <- fluidPage(
    list(tags$head(HTML('<link rel="icon", href="epa_seal_NoRing_RGB.png", 
                                   type="image/png" />'))),
    div(style="padding: 1px 0px; width: '100%'",
        titlePanel(
            title="", windowTitle="My Window Title"
        )
    ),
    
    navbarPage(
        theme = bs_theme(primary = "#62c342", font_scale = NULL, 
                         bootswatch = "united"),
        title = div(img(src="epa_seal_NoRing_RGB.png", height = 50), "UST EJ Cleanup Prioritization")
    ),
    
    fluidRow(
        column(3,
             wellPanel(
        pickerInput("state","Choose State", choices=unique(ej$STATE_NAME), options = list(`actions-box` = TRUE),
                    multiple = T, selected = "Ohio"),
                 
        #selectInput('state','Choose a State', unique(ej$STATE_NAME),"Ohio"),
        
        # Restrict choices, for all choices, use labels$label
        selectInput('xcol','X Variable', labels$label,"Percentile Minority (State)"),
        
        
        selectInput('ycol','Y Variable', labels$label,"Releases / km2")
        )),
        
        column(3,
               wellPanel(
                   selectInput('col',"Color Plot by:", labels$label,"National EJ Indexes > 80"),
                   sliderInput('sEJ',"State EJ Index Percentiles >",min = 0, max = 100,value = 0),
                   sliderInput('nEJ',"National EJ Index Percentiles >",min = 0, max = 100,value = 0)
               )),
        
        column(6,
               leafletOutput("map"))
    ),
    
    # Show a plot of the generated distribution
   
    
    fluidRow(
        column(6,
               "Selected Communities",
               DTOutput("table1"),
               downloadButton("report", "Generate report")),
        
        column(6,
               
               plotOutput("plot",
                          click = "plot1_click",
                          brush = brushOpts(
                              id = "plot1_brush"
                          )
               )
        )
    ),
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    # Turn on theming
    #bs_themer()
    
    # Activate state selection
    observe({
        print(input$locInput)
    })
    
    
    
    xVar <- reactive({
        labels%>%
            filter(label == input$xcol)%>%
            select(Var)%>%
            as.character()
    })
    
    xLab <- reactive({
        labels%>%
            filter(label == input$xcol)%>%
            select(label)%>%
            as.character()
    })
    
    yVar <- reactive({
        labels%>%
            filter(label == input$ycol)%>%
            select(Var)%>%
            as.character()
    })
    
    yLab <- reactive({
        labels%>%
            filter(label == input$ycol)%>%
            select(label)%>%
            as.character()
    })
    
    df <- reactive({
        ej%>%
            filter(STATE_NAME == input$state)%>%
            filter(N_P_DSLPM > input$nEJ |N_P_CANCR > input$nEJ |N_P_RESP > input$nEJ |N_P_PTRAF > input$nEJ |
                       N_P_PWDIS > input$nEJ |N_P_PNPL > input$nEJ |N_P_PRMP > input$nEJ |N_P_PTSDF > input$nEJ |
                       N_P_OZONE > input$nEJ |N_P_PM25 > input$nEJ |ST_P_DSLPM > input$sEJ |ST_P_CANCR > input$sEJ |ST_P_RESP > input$sEJ |ST_P_PTRAF > input$sEJ |
                       ST_P_PWDIS > input$sEJ |ST_P_PNPL > input$sEJ |ST_P_PRMP > input$sEJ |ST_P_PTSDF > input$sEJ |
                       ST_P_OZONE > input$sEJ |ST_P_PM25 > input$sEJ)%>%
             select(xVar(), yVar(),Latitude,Longitude,ID,ACSTOTPOP,MINORPCT,LOWINCPCT,N_Releases,
                    N_P_LDPNT,N_P_VULEOPCT,N_P_DSLPM,N_P_CANCR,N_P_RESP,N_P_PTRAF,N_P_PWDIS,N_P_PNPL,
                    N_P_PRMP,N_P_PTSDF,N_P_OZONE,N_P_PM25,pcol())%>%
            as.data.frame()
    })
    
    pcol <- reactive({
        labels%>%
            filter(label == input$col)%>%
            select(Var)%>%
            as.character()
    })
    
    pcolLab <- reactive({
        labels%>%
            filter(label == input$col)%>%
            select(label)%>%
            as.character()
    })
    
    output$plot <- renderPlot({
        ggplot(df())+
            geom_point(aes(x = as.numeric(df()[,xVar()]),y = as.numeric(df()[,yVar()]), colour = as.numeric(df()[,pcol()])), size = 2)+
            scale_colour_continuous(type = "viridis")+
            labs(title = paste0(input$state," Block Groups (2020)"),x = xLab(), y = yLab(), colour = pcolLab())+
            theme(text = element_text(size=14))
    })
    
    
    ## POINT CLICK / BRUSH INFO
    
    output$brush_info <- renderPrint({
        brushedPoints(df(), input$plot1_brush, xvar = colnames(df())[1],yvar = colnames(df())[2])
    })
    
    # Create subset of points to map
    points <- reactive({
        brushedPoints(df(), input$plot1_brush, xvar = colnames(df())[1],yvar = colnames(df())[2])
    })
    
    ## Data table for selected communities
    output$table1 <- renderDT({
        datatable(select(points(),ID,ACSTOTPOP,MINORPCT,LOWINCPCT,N_Releases),
                  options = list(pageLength = 5),
                  colnames = c("Block Group","Population","% Minority","% Low Income","# Active Releases"))%>%
            formatPercentage(c("MINORPCT","LOWINCPCT"), 0)
    })
    
    ## Use selection from datatable to generate a report
    rowSelect <- reactive({as.data.frame(input$table1_rows_selected)})
    
    # Subset ej data by selected rows in data table to generate report
     reportRows <- reactive({
         ej%>%
             filter(ID %in% points()[rowSelect()[,1],][,"ID"])
     })
     
     # Releases included in report
     reportReleases <- reactive({
         releases%>%
             filter(GEOID %in% points()[rowSelect()[,1],][,"ID"])
     })
    
    
    # Map
    
    # Set View
    lat <- reactive({mean(df()[,3], na.rm = TRUE)})
    lon <- reactive({mean(df()[,4],na.rm = TRUE)})
    
    
    output$map <- renderLeaflet({
        leaflet(points())%>%
            addTiles()%>%
            addWMSTiles(
                "https://tigerweb.geo.census.gov/arcgis/services/Census2020/tigerWMS_Census2010/MapServer/WMSServer",
                layers = "Census_Block_Groups59805",
                options = WMSTileOptions(format = "image/png", transparent = TRUE),
                attribution = "U.S. Census Bureau - 2020 Tiger"
            )%>%
            addCircleMarkers(
                radius = 12,
                color = "#b52121",
                stroke = FALSE, fillOpacity = 0.5,
                clusterOptions = markerClusterOptions(),
                popup =  ~paste0(
                    "Block Group ID: <b>",ID,"</b><br>",
                    "Population: <b>",ACSTOTPOP,"</b><br>",
                    "% Minority: <b>",MINORPCT*100,"%</b><br>",
                    "% Low Income: <b>",LOWINCPCT*100,"%</b><br>",
                    "# Active Releases: <b>",N_Releases,"</b><br><br>",
                    "<b>National EJ Indexes (Percentile):</b><br>",
                    "Lead Paint: <b>",round(N_P_LDPNT),"</b><br>",
                    "Diesel Particulate Matter: <b>",round(N_P_DSLPM),"</b><br>",
                    "Cancer: <b>",round(N_P_CANCR),"</b><br>",
                    "Air Toxics: <b>",round(N_P_RESP),"</b><br>",
                    "Traffic: <b>",round(N_P_PTRAF),"</b><br>",
                    "Direct Discharge to Water: <b>",round(N_P_PWDIS),"</b><br>",
                    "Proximity to NPL Sites: <b>",round(N_P_PNPL),"</b><br>",
                    "Proximity to RMP Facilities: <b>",round(N_P_PRMP),"</b><br>",
                    "Proximity to TSDF Facilities: <b>",round(N_P_PTSDF),"</b><br>",
                    "O-Zone: <b>",round(N_P_OZONE),"</b><br>",
                    "PM 2.5: <b>",round(N_P_PM25),"</b>"))%>%
            setView(lon(),lat(),zoom = 6)
    })
    
    #Generate Report for Selected Communities
    output$report <- downloadHandler(
        # For PDF output, change this to "report.pdf"
        filename = "report.html",
        content = function(file) {
            # Copy the report file to a temporary directory before processing it, in
            # case we don't have write permissions to the current working dir (which
            # can happen when deployed).
            tempReport <- file.path(tempdir(), "report.Rmd")
            file.copy("report.Rmd", tempReport, overwrite = TRUE)

            # Knit the document, passing in the `params` list, and eval it in a
            # child of the global environment (this isolates the code in the document
            # from the code in this app).
            rmarkdown::render(tempReport, output_file = file,
                              params = list(IDs = reportRows(),Lusts = reportReleases()),
                              envir = new.env(parent = globalenv())
            )
        }
    )
    
}

# Run the application 
shinyApp(ui = ui, server = server)
