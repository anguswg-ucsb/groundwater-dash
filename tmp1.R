# Angus Watters
# Groundwater dashboard
# 1/1/2021
# Data Manipulation
library(tidyr)
library(dplyr)    # data.frames
library(sf)       # Spatial
library(climateR)

library(AOI)
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

az_time = readRDS('data/az/az-join-time.rds') %>%
  # filter(source == 'AZ') %>%
  select(!measure_period)

az_spatial = az_time %>%
  group_by(wellid) %>%
  arrange(desc(date)) %>%
  slice(n = 1) %>%
  st_as_sf(coords = c('lng', 'lat'), crs = 5070)

well = az_spatial %>% filter(wellid == 5301)
buffer = st_buffer(az_spatial[well, ], 5000)
neighbors =st_intersection(buffer, az_spatial)         

az = USAboundaries::us_states() %>% 
  filter(name == 'Arizona') %>% 
  st_as_sf() %>% 
  st_transform(5070)
ggplot() + 
  geom_sf(data = az) +
  geom_sf(data = well) +
  geom_sf(data = buffer) + 
  geom_sf(data = st_intersection(az_spatial, buffer), col = "darkred", size = .5) 








tmp = join_time %>% filter(wellid == c('6001', '5998', '814', '4261')) 
ggplot(data = temp2) +
  geom_pointrange(
    mapping = aes(x = wellid, y = dtw),
    stat = "summary",
    fun.min = min,
    fun.max = max,
    fun = median
  )



pop = readxl::read_xls('data/pop-estimates.xls') %>% 
  select(fips = FIPStxt, state = State, county = Area_Name, POP_ESTIMATE_2010:POP_ESTIMATE_2019) %>% 
  janitor::clean_names() %>% 
  filter(state == 'AZ') %>% 
  mutate(fips = as.numeric(fips))

pop = pop[2:16,]

counties = readRDS('data/counties.rds')

tmp1 = left_join(select(pop, fips, county, pop = pop_estimate_2019), counties, by = 'fips')

tmp1 = tmp1 %>%
  st_as_sf() %>% 
  st_transform(4326)

az_spatial = az_time %>%
  group_by(wellid) %>%
  arrange(desc(date)) %>% 
  slice(n = 1) %>% 
  st_as_sf(coords = c('lng', 'lat'), crs = 4326) %>% 
  na.omit() %>% 
  select(wellid, date, dtw)

well_pops = st_join(az_spatial, tmp1) 

well_pops = well_pops %>% 
  group_by()









srad = getGridMET(AOI::geocode("Arizona", pt = TRUE),
                  param = "srad",
                  startDate = "2020-01-01",
                  endDate = "2020-01-05")

sites = read.csv('data/example.csv') %>%
  st_as_sf(coords = c("long", "lat"), crs = 4326)

az_rain = getTerraClim(AOI = az_counties,
                           param = "srad",
                           startDate = "2018-01-01",
                           endDate = "2018-01-05")
ggplot(data = az_rain,
       aes(x = date, y = srad, col = srad)) +
  geom_line() +
  labs(title = "Solar Radiation @ UCSB",
       x = "Date", y = "Solar Radiation (W/m^2)") +
  stat_smooth(col = "red") +
  theme_linedraw() +
  scale_color_viridis_c()





#################


############## STEP 2 ##############

# *** STEP 2.1 ***

water_use <-  readxl::read_xlsx('data/usco2015v2.0.xlsx', 1, skip = 1) %>%
  janitor::clean_names() %>%
  rename(population = tp_tot_pop)

# convert chr columns to numerics
water_use <- water_use %>%
  mutate(across(c(8:141), as.numeric))


withdrawal_total = water_use %>% 
  select(state, county, do_wsw_fr, do_wgw_fr,
         contains('wsw_to'), contains('wgw_to'),
         ir_wsw_fr, ir_wgw_fr,
         li_wsw_fr, li_wgw_fr)
withdrawal_total <- withdrawal_total %>%
  filter(state == "AZ") %>% 
  group_by(county) %>% 
  mutate(dom_surface = ps_wsw_to + do_wsw_fr,
         dom_groundwater = ps_wgw_to + do_wgw_fr,
         industrial_surface = in_wsw_to,
         industrial_groundwater = in_wgw_to,
         agr_surface = ir_wsw_fr + li_wsw_fr + aq_wsw_to,
         agr_groundwater = ir_wgw_fr + li_wgw_fr + aq_wgw_to,
         mining_surface = mi_wsw_to,
         mining_groundwater = mi_wgw_to,
         thermo_surface = pt_wsw_to,
         thermo_groundwater = pt_wgw_to) %>% 
  select(1:2, 25:34)

sectors_total <- withdrawal_total %>%   pivot_longer(cols = c(3:12), names_to = "sector", values_to = "withdrawal")

sectors_total$source <- ifelse(grepl("surface", sectors_total$sector, ignore.case = T), "surface_water", 
                  ifelse(grepl("groundwater", sectors_total$sector, ignore.case = T), "groundwater", "Other"))

sectors_total$sector <- ifelse(grepl("industrial", sectors_total$sector, ignore.case = T), "Industrial", 
                      ifelse(grepl("dom", sectors_total$sector, ignore.case = T), "Domestic",
                      ifelse(grepl("agr", sectors_total$sector, ignore.case = T), "Agriculture",
                      ifelse(grepl("mining", sectors_total$sector, ignore.case = T), "Mining",
                    ifelse(grepl("thermo", sectors_total$sector, ignore.case = T), "Thermoelectric", "Other"))))) 
sectors_total <- sectors_total %>% 
  select(state, county, sector, source, withdrawal)
sectors_total$county = gsub(" County", '', sectors_total$county)

county_well <- sectors_total %>% 
  filter(county == "Apache")
ggplot(county_well, aes(x = sector, y = withdrawal)) +
  geom_col(aes(fill = source), alpha = 0.7)  +
  labs(title = "SECTOR WITHDRAWALS",
       x = "SECTOR",
       y = "WITHDRAWALS",
       fill = "") +
  scale_fill_manual(values = c('green4', 'dodgerblue3')) +
  theme_bw() +
  theme(plot.title =element_text(size = 16, hjust = 0.5, vjust = 2),
        axis.text = element_text(face = "bold", size =10),
        axis.title = element_text(size = 12),
        legend.title = element_text(size = 12),
        plot.caption = element_text(hjust = 0, face = "bold", size = 12))



# aggregating columns into 5 sectors and total, then selecting only those cols & state
sectors <-  freshwater_totals %>%
  mutate(Domestic = ps_w_fr_to + do_w_fr_to,
         Industrial = in_w_fr_to,
         Agriculture = ir_w_fr_to + li_w_fr_to + aq_w_fr_to,
         Mining = mi_w_fr_to,
         Thermoelectric = pt_w_fr_to,
         Total = Domestic + Industrial + Agriculture + Mining + Thermoelectric) %>%
  select(1, 15:20)

# *** STEP 3.2 ***
# summarize across columns then df pivot longer
sectors <- sectors %>%
  # filter(state %in% c("CA", "NY")) %>%
  group_by(state) %>%
  summarise(across(Domestic:Total, sum)) %>%
  pivot_longer(cols = c(2:7), names_to = "sector", values_to = "withdrawal")

# ADD COUNTY COLUMN TO EACH WELL
counties = USAboundaries::us_counties() %>%
  filter(state_name == "Arizona")

az_spatial = az_time %>%
  group_by(wellid) %>%
  arrange(desc(date)) %>% 
  slice(n = 1) %>% 
  st_as_sf(coords = c('lng', 'lat'), crs = 4326) %>% 
  na.omit() %>% 
  select(wellid, date, dtw)

az_spatial = st_join(az_spatial, counties)
az_time = left_join(az_time, select(az_spatial, wellid, name), by = "wellid") %>%
  select(!geometry)

subset = well_counties %>% select(year, wellid, dtw, date, county, pop)
# subset = dplyr::filter(az_time, wellid == !!wellid) %>% 
#   select(date, wellid, dtw) %>% 
#   data.frame()
# 
# subset = subset[!duplicated(subset$date),]
install.packages('WDI')
df <- WDI(country = c("US"), indicator = "TX.QTY.MRCH.XD.WD", start = 1980, end = 2013, extra = FALSE)
x = WDI::WDIsearch()
df$exports <- df$TX.QTY.MRCH.XD.WD

df1 <- df %>%
  select(country, year, exports) %>%
  mutate(country = gsub("United States", "USA", df$country)) %>%
  spread(key = country, value = exports) %>%
  mutate(date = as.Date(as.character(year), format = "%Y")) %>%
  select(-year) 

# stackedGraph = TRUE

# well_counties$name <- gsub("^.{0,13}", "", well_counties$name)
# well_counties = well_counties %>% 
#   mutate(name = as.numeric(name))
# 
# well_counties = well_counties %>% rename(year = 'name', pop = 'value')

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




