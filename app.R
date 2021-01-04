
source('helper.R')
today = today_pts(az_time) 
basemap  <-  basemap(today)
# 
# pal <- colorNumeric('YlOrRd', domain = today$dtw, n = 100)
# 
# map = leaflet(data = today) %>%
#   addProviderTiles(providers$CartoDB.Positron) %>%
#   addScaleBar("bottomleft") %>%
#   addCircleMarkers(radius = 4,
#                    fillColor = ~pal(dtw), 
#                    fillOpacity = .8, 
#                    color = 'black',
#                    # color = ifelse(r1$source == 'USGS', 'black', NA),
#                    opacity = .8, 
#                    weight = .5,
#                    stroke = T,
#                    label = ~wellid,
#                    layerId = ~wellid)


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
 wellid <- reactiveValues(wellid)
 v   <- reactiveValues(msg = "Arizona Dept. of Water Resources")
 
 output$groundMap     <- renderLeaflet({ basemap })
 output$groundGraph = renderDygraph({ make_graph(az_time, wellid) })
 output$groundMessage <- renderText(v$msg)
 
 # observeEvent(input$groundMap_marker_mouseover, {
 #   txt = filter(today, wellid == input$groundMap_marker_mouseover$wellid)
 #   v$msg <- paste0("Mouse is over: ", txt$wellid)
 #   # v$msg <- "Mouse is over: "
 # })

 observeEvent(input$groundMap_marker_mouseout, {
   v$msg <- "Mouse is over: "
 })
 
 observeEvent(input$groundMap_marker_click, {
   # wellid <<- subset(az_time,wellid == input$groundMap_marker_click$wellid)
   wellid <<- input$groundMap_marker_click$wellid
   output$groundGraph <- renderDygraph({ make_graph(az_time, wellid) })
   # output$groundGraph <- renderDygraph({
   #   subset = filter(az_time, wellid == wellid) %>%
   #   ungroup() %>%
   #   as.data.table()
   # 
   #   subset$wellid <- as.numeric(subset$wellid)
   # 
   #   subset = subset %>% select(date, wellid, dtw)
   # 
   #   subset = as.xts.data.table(subset)
   # 
   # 
   # dygraph(data = subset$dtw,
   #         main = paste0("Well ", subset$wellid[1]),
   #         ylab = 'Depth to water (ft)',
   #         xlab = 'Date') %>%
   #   dyHighlight(highlightCircleSize = 4,
   #               highlightSeriesBackgroundAlpha = 0.6,
   #               highlightSeriesOpts = list(strokeWidth = 2.5)) %>%
   #   dyOptions(stackedGraph = TRUE,
   #             colors = c("navy")) })
   leafletProxy('groundMap')
   output$groundTable   <- renderDT({ make_table(az_time, wellid) })
   output$groundMessage <- renderText(v$msg)
 })
}

# subset = subset %>% select(date, wellid, source, dtw:lng)
# x = xts::as.xts(data.table::as.data.table(subset))

shinyApp(ui, server)












