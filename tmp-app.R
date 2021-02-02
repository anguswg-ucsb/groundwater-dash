# one piece of an answer to this StackOverflow question
#  http://stackoverflow.com/questions/31814037/integrating-time-series-graphs-and-leaflet-maps-using-r-shiny

# for this we'll use Kyle Walker's rpubs example
#   http://rpubs.com/walkerke/leaflet_choropleth
# combined with data from Diego Valle's crime in Mexico project
#   https://github.com/diegovalle/mxmortalitydb

# we'll also build on the shiny example included in leaflet
#  https://github.com/rstudio/leaflet/blob/master/inst/examples/shiny.R

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

gg1 = ggplot(subset4, aes(x = sector, y = withdrawal, fill = source)) +
  geom_col(position = "fill", col = "black", alpha = 0.7)  +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "SECTOR WITHDRAWALS",
       x = "SECTOR",
       y = "PERCENTAGE OF WITHDRAWALS",
       fill = "") +
  scale_fill_manual(values = c('green4', 'dodgerblue3')) +
  theme_bw() +
  theme(plot.title =element_text(size = 16, hjust = 0.5, vjust = 2),
        axis.text = element_text(size =10),
        axis.title = element_text(size = 10, vjust = 1),
        legend.title = element_text(size = 12),
        plot.caption = element_text(hjust = 0, face = "bold", size = 12)) 
ggplotly(gg1, tooltip = c('y'))
  # style(hoverlabel = label) %>% 
  # layout(font = font, 
  #        yaxis = list(fixedrange = TRUE)) %>% 
  # config(displayModeBar = FALSE))
}
subset4[which(subset4$withdrawal>0.00),]
withdrawals_plot(az_time, sector_total, 5301)

county_well <- sectors_total %>% 
  filter(county == "Apache")
ggplot(subset4, aes(x = sector, y = withdrawal)) +
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

stats = well_stats(az_time, 100)
stats[3]

source('helper.R')
today = today_pts(az_time) 
basemap  <-  basemap(today)

ui <- dashboardPage(
  dashboardHeader(title = "ARIZONA GROUNDWATER WELLS"),
  dashboardSidebar(disable = TRUE),
  dashboardBody(
    fluidRow(
      column(width = 7,
             autocomplete_input("auto", "Search for a County:",
                                value = "",
                                max_options = 5,
                                structure(today$fips, names = today$name)),
             box(width = NULL, solidHeader = TRUE,
                 leafletOutput("groundMap", height = 650)),
             box(width = NULL, title = "Statistics",
                 solidHeader = TRUE,
                 DTOutput('groundTable'))
             
      ),
      
      column(width = 4,
             box(width = NULL, status = "primary",
                 title = "DEPTH TO WATER",
                 solidHeader = TRUE,
                 dygraphOutput('groundGraph')),
             # infoBox("Total cases", icon = icon("credit-card"), fill = TRUE),
             box(width = NULL, status = "primary",
                 title = "SECTOR WITHDRAWALS",
                 solidHeader = TRUE,
                 plotlyOutput('groundPlot')),
             valueBoxOutput("depthValue"), 
             valueBoxOutput("minValue"),
             valueBoxOutput("maxValue"),
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
  output$groundTable <- renderDT({ make_table(az_time, wellid) })
  output$groundMessage <- renderText(v$msg)
  output$depthValue <- renderValueBox({
    valueBox(
      paste0((well_stats(az_time, wellid)[3])),
      subtitle = "DEPTH",
      icon = icon("user"),
      color = "blue") })
  output$minValue <- renderValueBox({
    valueBox(
      paste0((well_stats(az_time, wellid)[4])),
      subtitle = "MINMUMUM DEPTH",
      icon = icon("user"),
      color = "green") })
  output$maxValue <- renderValueBox({
    valueBox(
      paste0((well_stats(az_time, wellid)[5])),
      subtitle = "MAXIMUM DEPTH",
      icon = icon("user"),
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
    output$groundTable <- renderDT({ make_table(az_time, wellid) })
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



shinyApp( ui, server )





