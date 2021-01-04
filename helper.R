
library(dplyr)    # data.frames
library(sf)       # Spatial
library(data.table)


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
  filter(source == 'AZ') %>% 
  select(!measure_period)


# Join County Data with 
today_pts = function(time_data){
  today = time_data %>%
    group_by(wellid) %>% 
    arrange(desc(date)) %>% 
    slice(n =1)
  today = today %>%
    st_as_sf(coords = c('lng', 'lat'), crs = 4326) %>% 
    na.omit()
}



make_graph  = function(az_time, wellid){
  
  subset = dplyr::filter(az_time, wellid == !!wellid) %>% 
    select(date, wellid, dtw) %>% 
    data.frame()
  
  subset = subset[!duplicated(subset$date),]

  rownames(subset) = subset$date
      
  dygraph(data = dplyr::select(subset,dtw),
            main = paste0("Well ", subset$wellid[1]),
            ylab = 'Depth to water (ft)',
            xlab = 'Date') %>%
      dyHighlight(highlightCircleSize = 4,
                  highlightSeriesBackgroundAlpha = 0.6,
                  highlightSeriesOpts = list(strokeWidth = 2.5)) %>%
      dyOptions(stackedGraph = TRUE,
                colors = c("navy"))

}


basemap = function(az_time){
  pal <- colorNumeric('YlOrRd', domain = today$dtw, n = 100)
  
  # I had this taking input "today" from today_pts but I was trouble shooting and thought maybe I should use only full dataset so each point has all time rows for each well
  
  az_spatial = az_time %>%
    group_by(wellid) %>% 
    st_as_sf(coords = c('lng', 'lat'), crs = 4326) %>% 
    na.omit()
  
  leaflet(data = az_spatial) %>%
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
}

zoom_to_well = function(map, today, well){
  # Filter the counties to the input FIP code
  shp = filter(today, wellid == 100) %>% st_buffer(.001)
  # Build a buffered bounding box to center the map on:
  bounds = shp %>% 
    # make bounding box
    st_bbox() %>% 
    # Make spatial
    st_as_sfc() %>% 
    # make new bounding box
    st_bbox() %>% 
    # extract coordinates as vector
    as.vector()
  # Clear all current shapes (remember county centroids are currently markers!)
  clearShapes(map) %>% 
    # Add the county shape making the outline color red and the fill an opaque white
    addPolygons(data = shp,
                color = "transparent",
                fillColor  = "white") %>% 
    # Fly the leaflet map to the buffered boundary
    flyToBounds(bounds[1], bounds[2], bounds[3], bounds[4])
}

## Build time series table
make_table = function(az_time, wellid){
  # tmp1 = today %>% filter(wellid == well)
  tmp1 = az_time %>% filter(wellid == wellid)
  # Filter todays data to wellid time series record
  # tmp2 = az_time %>% filter(wellid == tmp1$wellid) %>% 
  #   arrange(desc(date)) %>% 
  #   select(wellid:date_max, measurement_dist)
  
  # Interactive Data table
  datatable(tmp1)
            # , caption = paste0('Well statistics: ', tmp1$date), options = list(paging = FALSE, searching = TRUE))
}


