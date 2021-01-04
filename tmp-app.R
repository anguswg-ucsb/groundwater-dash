# one piece of an answer to this StackOverflow question
#  http://stackoverflow.com/questions/31814037/integrating-time-series-graphs-and-leaflet-maps-using-r-shiny

# for this we'll use Kyle Walker's rpubs example
#   http://rpubs.com/walkerke/leaflet_choropleth
# combined with data from Diego Valle's crime in Mexico project
#   https://github.com/diegovalle/mxmortalitydb

# we'll also build on the shiny example included in leaflet
#  https://github.com/rstudio/leaflet/blob/master/inst/examples/shiny.R

library(shiny)
library(leaflet)
library(dygraphs)
library(rgdal)

# let's build this in advance so we don't download the
#    data every time
tmp <- tempdir()

url <- "http://personal.tcu.edu/kylewalker/data/mexico.zip"

file <- basename(url)

download.file(url, file)

unzip(file, exdir = tmp)

mexico <- readOGR(dsn = tmp, layer = "mexico", encoding = "UTF-8")

}
pal <- colorQuantile("YlGn", NULL, n = 5)

leaf_mexico <- leaflet(data = mexico) %>%
  addTiles() %>%
  addPolygons(fillColor = ~pal(gdp08), 
              fillOpacity = 0.8, 
              color = "#BDBDC3", 
              weight = 1,
              layerId = ~id)



# now let's get our time series data from Diego Valle
crime_mexico <- jsonlite::fromJSON(
  "https://rawgit.com/diegovalle/crimenmexico.diegovalle.net/master/assets/json/states.json"
)

check = dplyr::bind_rows(crime_mexico)

ui <- fluidPage(
  leafletOutput("map1"),
  dygraphOutput("dygraph1",height = 200),
  textOutput("message", container = h3)
)

server <- function(input, output, session) {
  v <- reactiveValues(msg = "")
  
  output$map1 <- renderLeaflet({
    leaf_mexico
  })
  
  output$dygraph1 <- renderDygraph({
    # start dygraph with all the states
    crime_wide <- reshape(
      crime_mexico$hd[,c("date","rate","state_code"),drop=F],
      v.names="rate",
      idvar = "date",
      timevar="state_code",
      direction="wide"
    )
    colnames(crime_wide) <- c("date",as.character(mexico$state))
    rownames(crime_wide) <- as.Date(crime_wide$date)
    dygraph(
      crime_wide[,-1]
    )
  })
  
  observeEvent(input$map1_shape_mouseover, {
    v$msg <- paste("Mouse is over shape", input$map1_shape_mouseover$id)
  })
  observeEvent(input$map1_shape_mouseout, {
    v$msg <- ""
  })
  observeEvent(input$map1_shape_click, {
    v$msg <- paste("Clicked shape", input$map1_shape_click$id)
    #  on our click let's update the dygraph to only show
    #    the time series for the clicked
    state_crime_data <- subset(crime_mexico$hd,state_code == input$map1_shape_click$id)
    rownames(state_crime_data) <- as.Date(state_crime_data$date)
    output$dygraph1 <- renderDygraph({
      dygraph(
        xts::as.xts(state_crime_data[,"rate",drop=F]),
        ylab = paste0(
          "homicide rate ",
          as.character(mexico$state[input$map1_shape_click$id])
        )
      )
    })
  })
  observeEvent(input$map1_zoom, {
    v$msg <- paste("Zoom changed to", input$map1_zoom)
  })
  observeEvent(input$map1_bounds, {
    v$msg <- paste("Bounds changed to", paste(input$map1_bounds, collapse = ", "))
  })
  
  output$message <- renderText(v$msg)
}

shinyApp(ui, server)
