
source('helper.R')
today = today_pts(az_time) 
basemap  <-  basemap(today)

ui <- dashboardPage(
     dashboardHeader(title = "USGS Groundwater"),
     dashboardSidebar(disable = TRUE),
     dashboardBody(
         fluidRow(
             column(width =6,
                                   # autocomplete_input("auto", "Search for a County:",
                                     #                    value = "",
                                     #                    max_options = 5,
                                     #                    structure(today$county, names = today$name)),
                                     box(width = NULL, height = 600, solidHeader = TRUE, 
                                                            leafletOutput("groundMap")),
                                   # box(width = NULL, status = "primary",
                                     #     title = "SECTOR WITHDRAWALS",
                                     #     solidHeader = TRUE,
                                     #     plotlyOutput('groundPlot'))
                                     # tabBox(width = NULL, height = 700,
                                     #        tabPanel("DEPTH TO WATER", dygraphOutput('groundGraph')),
                                     #        # tabPanel("WITHDRAWALS BY SECTOR", plotlyOutput('groundPlot') ),
                                     #        tabPanel("CHART", DTOutput('groundTable'))),
                                     valueBoxOutput("depthValue"),
                                   valueBoxOutput("minValue"),
                                   valueBoxOutput("maxValue")
                                   # box(width = NULL,
                                     #     title = "Statistics",
                                     #     status = "primary",
                                     #     solidHeader = TRUE,
                                     #     DTOutput('groundTable'))
                              #        
                            ),
       
               column(width =5,
                                     box(width = NULL, status = "primary",
                                                            title = "WITHDRAWALS BY SECTOR",
                                                            solidHeader = TRUE,
                                                            plotlyOutput('groundPlot')),
                                     tabBox(width = NULL,
                                                             tabPanel("DEPTH TO WATER", dygraphOutput('groundGraph')),
                                                             # tabPanel("WITHDRAWALS BY SECTOR", plotlyOutput('groundPlot')),
                                                               tabPanel("TABLE", DTOutput('groundTable'))),
                              
                                       # ),
                            
                                #       
                                #        # infoBox("Total cases", icon = icon("credit-card"), fill = TRUE),
                                # valueBoxOutput("depthValue"), 
                                # valueBoxOutput("minValue"),
                                # valueBoxOutput("maxValue")
                                    
                                )
           )
       )
   )
server <- function(input, output, session) {
     # Global variables initialized
       wellid <- today$wellid[which.max(today$dtw)]
       v   <- reactiveValues(msg = "Arizona Dept. of Water Resources")
       
         output$groundMap     <- renderLeaflet({ basemap })
         output$groundGraph   <- renderDygraph({ make_graph(az_time, wellid) })
         output$groundGraph2  <- renderDygraph({ make_graph2(az_time, wellid) })
         output$groundPlot    <- renderPlotly({ withdrawals_plot(az_time, sector_total, wellid) })
         output$groundTable <- renderDT({ make_table(today, az_time, wellid) })
         output$groundMessage <- renderText(v$msg)
         output$depthValue <- renderValueBox({
             valueBox(
                 paste0((well_stats(az_time, wellid)[3])),
                 subtitle = "DEPTH",
                 icon = icon("water"),
                 color = "blue") })
         output$minValue <- renderValueBox({
             valueBox(
                 paste0((well_stats(az_time, wellid)[4])),
                 subtitle = "MINMUMUM DEPTH",
                 icon = icon("arrow-up"),
                 color = "green") })
         output$maxValue <- renderValueBox({
             valueBox(
                 paste0((well_stats(az_time, wellid)[5])),
                 subtitle = "MAXIMUM DEPTH",
                 icon = icon("arrow-down"),
                 color = "red") })
         observeEvent(input$groundMap_marker_mouseout, {
             v$msg <- "Mouse is over: "
           })
         
           observeEvent(input$groundMap_marker_click, {
               wellid <<- input$groundMap_marker_click$id
               print(wellid)
               output$groundGraph <- renderDygraph({ make_graph(az_time, wellid) })
               
                 # output$groundTable   <- renderDT({ make_table(az_time, wellid) })
                 output$groundGraph2  <- renderDygraph({ make_graph2(az_time, wellid) })
                 output$groundPlot    <- renderPlotly({ withdrawals_plot(az_time, sector_total, wellid) })
                 output$groundTable <- renderDT({ make_table(today, az_time,  wellid) })
                 output$groundMessage <- renderText(v$msg)
                 output$depthValue <- renderValueBox({
                     valueBox(
                         paste0((well_stats(az_time, wellid)[3])),
                         subtitle = "DEPTH",
                         icon = icon("user"),
                         color = "blue") })
                 leafletProxy('groundMap')
               })
       }

shinyApp(ui, server)








