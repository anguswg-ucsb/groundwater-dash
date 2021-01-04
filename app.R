
source('helper.R')
today = today_pts(az_time) 
basemap  <-  basemap(today)


ui = fluidPage(
  titlePanel('Arizona Groundwater Dashboard'),
  
  sidebarPanel(
    textOutput("groundMessage", container = h4),
    
    DTOutput("groundTable")
    
  ),
  
  mainPanel(
    leafletOutput('groundMap'),
    dygraphOutput('groundGraph')
  )
)


# Server logic ----
server <- function(input, output, session) { 
   
 wellid <- today$wellid[which.max(today$dtw)]
 v   <- reactiveValues(msg = "Arizona Dept. of Water Resources")
 
 output$groundMap     <- renderLeaflet({ basemap })
 output$groundGraph   <-  renderDygraph({ make_graph(az_time, wellid) })
 output$groundMessage <- renderText(v$msg)

 observeEvent(input$groundMap_marker_mouseout, {
   v$msg <- "Mouse is over: "
 })
 
 observeEvent(input$groundMap_marker_click, {
   wellid <<- input$groundMap_marker_click$id
   print(wellid)
   output$groundGraph <- renderDygraph({ make_graph(az_time, wellid) })

   leafletProxy('groundMap')
   output$groundTable   <- renderDT({ make_table(az_time, wellid) })
   output$groundMessage <- renderText(v$msg)
 })
}


shinyApp(ui, server)












