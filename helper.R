
library(tidyverse)    # data.frames
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
library(shinydashboard)

az_time = readRDS('data/az-join-time.rds') 
az_time$dtw_neg = az_time$dtw*(-1)

sector_total <- readRDS('data/sector_total.rds')

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
            main = paste0("WELL ", subset$wellid[1]),
            ylab = 'DEPTH TO WATER (ft)',
            xlab = 'DATE') %>%
    dyAxis('y', valueRange = c(100 + max(subset$dtw), 0)) %>% 
      dyHighlight(highlightCircleSize = 4,
                  highlightSeriesBackgroundAlpha = 0.6,
                  highlightSeriesOpts = list(strokeWidth = 4)) %>%
      dyOptions(includeZero = TRUE,
                colors = c("navy"), strokeWidth = 3)
       
                 # stackedGraph = TRUE

}


basemap = function(az_time){
  pal <- colorNumeric('YlOrRd', domain = az_time$dtw, n = 100)
  
  # I had this taking input "today" from today_pts but I was trouble shooting and thought maybe I should use only full dataset so each point has all time rows for each well
  
  az_spatial = az_time %>%
    group_by(wellid) %>% 
    st_as_sf(coords = c('lng', 'lat'), crs = 4326) %>% 
    na.omit()
  
  # Arizona state shape (CRS = 5070)
  az = USAboundaries::us_states() %>%
    filter(state_name == 'Arizona') %>%
    st_transform(4326)
  
  # read in AMA shapefiles
  ama = read_sf('az-ama-shps/Act_Man_Areas.shp') %>% 
    st_transform(4326) %>% 
    st_cast("MULTIPOLYGON")
  # ama = ama %>% st_transform(4326)
  
  col3 = RColorBrewer::brewer.pal(9,"YlGnBu")
  pals3 = colorBin(col3, domain = 1:8)
  
  leaflet(data = az_spatial) %>%
    addProviderTiles(providers$CartoDB.Positron) %>%
    addMapPane("wells", zIndex = 420) %>% 
    addMapPane("AMAs", zIndex = 410) %>% 
    addPolygons(data = az,
                fillColor = 'transparent', 
                opacity = 7,
                weight = 2,
                color = 'black') %>% 
    addScaleBar("bottomleft") %>%
    addCircleMarkers(radius = 4,
                     fillColor = ~pal(dtw), 
                     fillOpacity = .8, 
                     color = 'black',
                     # color = ifelse(r1$source == 'USGS', 'black', NA),
                     opacity = .8, 
                     weight = .5,
                     stroke = T,
                     popup = leafpop::popupTable(select(st_drop_geometry(az_spatial), wellid, dtw, source, date),
                                                 feature.id = FALSE,
                                                 row.numbers = FALSE),
                     label = ~wellid,
                     layerId = ~wellid,
                     options = pathOptions(pane = "wells"),
                     group = "WELLS") %>% 
    addPolygons(data = ama,
                fillColor  = ~pals3(OBJECTID),
                fillOpacity = .4,
                opacity = .7,
                color = 'black',
                weight = 3,
                label = ~MAP_LABEL,
                options = pathOptions(pane = "AMAs"),
                group = 'AMA') %>% 
    addLegend("bottomright",
              pal = pal,
              values = ~dtw,
              title = 'Depth to water (ft)',
              opacity = 1) %>% 
    addLayersControl(overlayGroups = c("WELLS", 'AMA'),
                     options = layersControlOptions(collapsed = FALSE)) 
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
make_table = function(today, az_time, wellid){
  tmp1 = today %>% filter(wellid == !!wellid)
  # tmp1 = az_time %>% filter(wellid == wellid)
  # Filter todays data to wellid time series record
  tmp2 = az_time %>% filter(wellid == tmp1$wellid) %>%
    arrange(desc(date), desc(measurement_dist)) %>%
    select(wellid:dtw)
  tmp3 = tmp2[!duplicated(tmp2$date),]
  
  # Interactive Data table
  datatable(tmp3, caption = paste0('WELL ', tmp1$wellid, ' -----  AVERAGE DTW: ', round(tmp1$avg_dtw),
                                   'ft -----  MINIMUM: ',
                                   round(tmp1$min_dtw),
                                   'ft -----  MAXIMUM: ', round(tmp1$max_dtw), 'ft'),
            options = list(paging = FALSE, searching = TRUE))
}


plotMultipleWells = function(df_time) {
  font = list(
    family = 'Courier',
    size = 15,
    color = 'white')
  label = list(
    bgcolor = '#232F34',
    bordercolor = 'transparent',
    font = font)
  
  gg = ggplot(data = df_time, aes(x = date, y = dtw)) +
    geom_line(aes(y = dtw, col = wellid), size = 1) +
    ylim(max(df_time$dtw) + 50, 0) +
    labs(x = 'Year',
         y = 'DTW (ft)',
         col = 'Well') +  
    theme_bw() +
    theme(plot.title = element_text(face = 'bold',color = 'black', size = 18, hjust = 0.5), 
          axis.text.x = element_text(color="black", size=14), 
          axis.text.y = element_text(color="black", size=14), 
          axis.title.x = element_text(face="bold", color="black", size=16), 
          axis.title.y = element_text(face="bold", color="black", size=16), 
          panel.grid.major = element_line(colour = "#808080"),
          panel.grid.minor = element_line(colour = "#808080", size = 1))
  plot = ggplotly(gg, tooltip = c('x', 'y', 'wellid')) %>%
    style(hoverlabel = label) %>% 
    layout(font = font, 
           yaxis = list(fixedrange = TRUE)) %>% 
    config(displayModeBar = FALSE)
  plot
}

well_stats = function(az_time, wellid) {
  stats = az_time %>% 
    filter(wellid == !!wellid) %>% 
    arrange(desc(date)) %>% 
    slice(n = 1) %>% 
    select(wellid, source, dtw, min_dtw, max_dtw, measurement_dist)
}


withdrawals_plot <- function(az_time, sector_total, wellid){
  # ADD WELL ID TO SECTOR TOTALS DF
  tmp1 <- az_time %>%
    filter(wellid == !!wellid) %>%
    select(county, wellid)
  
  subset4 <- sector_total %>%  
    filter(county == tmp1$county[1])
  
  subset4$wellid <- tmp1$wellid[match(subset4$county, tmp1$county)]
  
  gg1 = ggplot(subset4, aes(x = sector, y = withdrawal)) +
    geom_col(aes(fill = source), col = "black", width= 0.5, alpha = 0.8) +
    labs(x = "SECTOR",
         y = "WITHDRAWALS (Mgal/day)",
         fill = "") +
    scale_fill_manual(values = c('green4', 'dodgerblue3')) +
    theme_classic() +
    theme(plot.title =element_text(size = 16, hjust = 0.5, vjust = 2),
          axis.text = element_text(size =10),
          axis.title = element_text(size = 10, vjust = 1),
          legend.text = element_text(size = 12),
          legend.position = "top",
          plot.caption = element_text(hjust = 0, face = "bold", size = 12)) 
  ggplotly(gg1, tooltip = c('y')) %>%
    layout(legend = list(
      orientation = "h",
      x = 0.4, y = 1.1,
      itemclick = "toggleothers"))
  # style(hoverlabel = label) %>% 
  # layout(font = font, 
  #        yaxis = list(fixedrange = TRUE)) %>% 
  # config(displayModeBar = FALSE))
}

# withdrawals_plot(az_time, sector_total, 5301)







