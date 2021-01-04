# Angus Watters
# Groundwater dashboard
# 1/1/2021
# Data Manipulation

library(dplyr)    # data.frames
library(sf)       # Spatial

# Interactive Data Viz
library(leaflet)  # Maps
library(dygraphs) # Charts
library(plotly)
library(DT)       # tables
library(rvest)    # webscraping


# Shiny
library(dqshiny)    # auto complete
library(shiny)       # Starting Reactivity
library(shinythemes) # themes


source('helper.R')
today = today_pts(az_time) 
basemap  <-  basemap(today)

pal <- colorNumeric('YlOrRd', domain = today$dtw, n = 100)

map = leaflet(data = today) %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addScaleBar("bottomleft") %>%
  addCircleMarkers(radius = 4,
                   fillColor = ~pal(dtw), 
                   fillOpacity = .8, 
                   color = 'black',
                   # color = ifelse(r1$source == 'USGS', 'black', NA),
                   opacity = .8, 
                   weight = .5,
                   stroke = T,
                   label = ~wellid,
                   layerId = ~wellid)


ui = fluidPage(
  titlePanel('Arizona Groundwater Dashboard'),
  
  sidebarPanel(
    textOutput("groundMessage", container = h4)
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
  
  output$groundGraph = renderDygraph({ make_graph(az_time, wellid) })
  
  output$groundMessage <- renderText(v$msg)
  
  observeEvent(input$groundmap_marker_mouseover, {
    txt = filter(today, wellid == input$groundMap_marker_mouseover$wellid)
    v$msg <- paste0("Mouse is over: ", txt$wellid)
    # v$msg <- "Mouse is over: "
  })
  # observeEvent(input$groundMap_marker_mouseover, {
  # txt = filter(today, wellid == input$groundMap_marker_mouseover$wellid)
  # v$msg <- paste0("Mouse is over: ", txt$wellid)
  # })
  # 
  observeEvent(input$groundMap_marker_mouseout, {
    v$msg <- "Mouse is over: "
  })
  
  observeEvent(input$groundMap_marker_click, {
    wellid <<- subset(az_time,wellid == input$groundMap_marker_click$wellid)
    # wellid <<- input$groundMap_marker_click$wellid
    output$groundGraph <- renderDygraph({ make_graph(az_time, wellid) })
    leafletProxy('groundMap')
  })
}

shinyApp(ui, server)

# 
# #####################################################
# 
# pal <- colorQuantile("YlGn", NULL, n = 5)
# 
# leaf_mexico <- leaflet(data = mexico) %>%
#   addTiles() %>%
#   addPolygons(fillColor = ~pal(gdp08), 
#               fillOpacity = 0.8, 
#               color = "#BDBDC3", 
#               weight = 1,
#               layerId = ~id)
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
# 
# 
# 
# check = dplyr::bind_rows(crime_mexico)
# 
# ui <- fluidPage(
#   leafletOutput("map1"),
#   dygraphOutput("dygraph1",height = 200),
#   textOutput("message", container = h3)
# )
# 
# server <- function(input, output, session) {
#   v <- reactiveValues(msg = "")
#   
#   output$map1 <- renderLeaflet({
#     map
#   })
#   
#   output$dygraph1 <- renderDygraph({
#     # start dygraph with all the states
# 
#       dygraph(data = subset,
#               main = paste0("Well ", az_time$wellid[1]),
#               ylab = 'Depth to water (ft)', 
#               xlab = 'Date') %>% 
#         dyHighlight(highlightCircleSize = 4, 
#                     highlightSeriesBackgroundAlpha = 0.6,
#                     highlightSeriesOpts = list(strokeWidth = 2.5)) %>% 
#         dyOptions(stackedGraph = TRUE,
#                   colors = c("navy"))
#     )
#   })
#   
#   observeEvent(input$map1_shape_mouseover, {
#     v$msg <- paste("Mouse is over shape", input$map1_shape_mouseover$wellid)
#   })
#   observeEvent(input$map1_shape_mouseout, {
#     v$msg <- ""
#   })
#   observeEvent(input$map1_shape_click, {
#     v$msg <- paste("Clicked shape", input$map1_shape_click$wellid)
#     #  on our click let's update the dygraph to only show
#     #    the time series for the clicked
#     subset = filter(az_time, wellid == 100) %>%
#       ungroup() %>%
#       data.frame()
#     
#     subset2 = subset[!duplicated(subset$date), ]
#     
#     rownames(subset2) <- subset2$date
#     subset2 = subset2 %>% select(dtw)
#     subset <- subset(az_time$hd, wellid == input$map1_shape_click$wellid)
#     rownames(subset) <- as.Date(subset$date)
#     output$dygraph1 <- renderDygraph({
#       dygraph(
#         xts::as.xts(state_crime_data[,"rate",drop=F]),
#         ylab = paste0(
#           "homicide rate ",
#           as.character(mexico$state[input$map1_shape_click$id])
#         )
#       )
#     })
#   })
#   observeEvent(input$map1_zoom, {
#     v$msg <- paste("Zoom changed to", input$map1_zoom)
#   })
#   observeEvent(input$map1_bounds, {
#     v$msg <- paste("Bounds changed to", paste(input$map1_bounds, collapse = ", "))
#   })
#   
#   output$message <- renderText(v$msg)
# }
# 
# shinyApp(ui, server)
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# tmp1 = today %>% filter(wellid == 5301)
# 
# tmp2 = az_time %>% filter(wellid == tmp1$wellid) %>% 
#   arrange(desc(date)) 
# 
# # Make an interactive Table!
# datatable(tmp2, 
#           # add a title to the table
#           caption = paste0('Well statistics: ', tmp1$date)) 
# 
# ## Build time series table
# make_table = function(today, well, az_time){
#   tmp1 = today %>% filter(wellid == well)
#   
#   # Filter todays data to wellid time series record
#   tmp2 = az_time %>% filter(wellid == tmp1$wellid) %>% 
#     arrange(desc(date)) %>% 
#     select(wellid:date_max, measurement_dist)
# 
#   # Interactive Data table
#   datatable(tmp2, caption = paste0('Well statistics: ', tmp1$date), options = list(paging = FALSE, searching = TRUE))
# }
# make_table(today, 100, az_time)

# today = today %>% filter(source == 'AZ')

# test = az_time %>% filter(!wellid %in% today$wellid)
# sub = filter(az_time, wellid == 5301) %>% ungroup() %>% data.frame()
# sub2 = sub %>% select(date, wellid, source, dtw:lng) %>% unique(by = 'date')
# rownames(sub2)<- sub2$date 
#
# dygraph(data = select(sub2, dtw),
#         main = paste0("Well ", sub2$wellid[1]),
#         ylab = 'Depth to water (ft)', 
#         xlab = 'Date') %>% 
#   dyHighlight(highlightCircleSize = 4, 
#               highlightSeriesBackgroundAlpha = 0.6,
#               highlightSeriesOpts = list(strokeWidth = 2.5)) %>% 
#   dyOptions(stackedGraph = TRUE,
#             colors = c("navy"))

# plot_ly(sub, x = ~date, y = ~dtw, type = 'scatter', mode = 'lines') %>%
#   layout(title = paste0('Well ', sub$wellid[1]), 
#          xaxis = list(title = "Date"),
#          yaxis = list (title = "Depth to water (ft)")) 
# 
# 
# font = list(
#   family = 'Courier',
#   size = 15,
#   color = 'white'
# )
# label = list(
#   bgcolor = '#232F34',
#   bordercolor = 'transparent',
#   font = font
# )
# 
# 
# # COLOR PALLETES 
# #RColorBrewer::display.brewer.all(n=4, exact.n=FALSE)
# 
# nb.cols = 10
# col1 = RColorBrewer::brewer.pal(9,"Blues")
# # col2 = brewer.pal(9,"YlOrRd")
# col2 = colorRampPalette(RColorBrewer::brewer.pal(9,"YlOrRd"))(nb.cols)
# col3 = RColorBrewer::brewer.pal(9,"YlGnBu")
# col4 = RColorBrewer::brewer.pal(9,"Spectral")
# col5 = RColorBrewer::brewer.pal(9,"Greys")
# col6 = palette(c('black', 'white'))
# 
# pals1 = colorFactor('cyan', domain = x$dtw)
# pals2 = colorNumeric(col2, domain = c(0, 1500))
# pals3 = colorBin(col3, domain = 1:8)
# pals4 = colorFactor(col4, domain = aquifer2$AQ_NAME)
# pals5 = colorFactor(col6, domain = x$source)
# 
# 
# pal <- colorNumeric('YlOrRd', domain = today$dtw, n = 100)
# pal2 <- colorNumeric("YlOrRd", domain = today$dtw, n = 50) 
# 
# map = leaflet(data = today) %>%
#   addProviderTiles(providers$CartoDB.Positron) %>%
#   addScaleBar("bottomleft") %>%
#   addCircleMarkers(radius = 4,
#                     fillColor = ~pal(dtw), 
#                     fillOpacity = .8, 
#                     color = 'black',
#                     # color = ifelse(r1$source == 'USGS', 'black', NA),
#                     opacity = .8, 
#                     weight = .5,
#                     stroke = T,
#                    label = ~wellid,
#                    layerId = ~wellid)
#   # addLegend("bottomright",
#   #           pal = pal,
#   #           values = ~dtw,
#   #           title = 'Depth to water ',
#   #           opacity = 1)
#   # addLegend(pal = pal,
#   #           values = c(0, 1500),
#   #           opacity = .9,
#   #           title = 'Depth to water (ft)', # Title
#   #           position = "bottomright",
#   #           labFormat = function(type, cuts, p) {
#   #             paste0(labels)}) 
#   
# zoom_to_well = function(map, today, well){
#   # Filter the counties to the input FIP code
#   shp = filter(today, wellid == 100) %>% st_buffer(.001)
#   # Build a buffered bounding box to center the map on:
#   bounds = shp %>% 
#     # make bounding box
#     st_bbox() %>% 
#     # Make spatial
#     st_as_sfc() %>% 
#     # make new bounding box
#     st_bbox() %>% 
#     # extract coordinates as vector
#     as.vector()
#   # Clear all current shapes (remember county centroids are currently markers!)
#   clearShapes(map) %>% 
#     # Add the county shape making the outline color red and the fill an opaque white
#     addPolygons(data = shp,
#                 color = "transparent",
#                 fillColor  = "white") %>% 
#     # Fly the leaflet map to the buffered boundary
#     flyToBounds(bounds[1], bounds[2], bounds[3], bounds[4])
# }
#  
# # plot_ly(sub, x = ~date, y = ~dtw, type = 'scatter', mode = 'lines') %>%
# #   layout(title = paste0('Well ', sub$wellid[1]), 
# #          xaxis = list(title = "Date"),
# #          yaxis = list (title = "Depth to water (ft)")) 
# 
# 
# 
# 
# 
# 
# 
# 
# 




