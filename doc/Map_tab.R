#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#
# load("../output/all_locations.RData")

library(shiny)
library(leaflet)
library(data.table)
library(plotly)
library(shinyWidgets)
library(googleVis)
library(geosphere)
library(leaflet.extras)
library(ggmap)

server <- function(input, output) {
  
  locations_within <- function(r,long,lat){
    return(all_locations[distCosine(c(long,lat),all_locations[,c("Longitude","Latitude")])<=r,])
    }
  
  
  output$mapMarker <- renderLeaflet({
    leaflet(options = leafletOptions(minZoom = 8, maxZoom = 18)) %>%
      addTiles('http://{s}.basemaps.cartocdn.com/light_all/{z}/{x}/{y}.png') %>%
      setView(lng = -73.95, lat = 40.72, zoom = 12)
     
  })
  
  observeEvent(input$mapMarker_click, {
    #if(!input$click_multi) 
    leafletProxy("mapMarker") %>% clearGroup(c("circles","centroids", 'locations'))
    click <- input$mapMarker_click
    clat <- click$lat
    clong <- click$lng
    radius <- input$click_radius
    
    #output info
    #output$click_coord <- renderText(paste("Latitude:",round(clat,7),", Longitude:",round(clong,7)))
    locations_within_range <- locations_within(input$click_radius, clong, clat)
    
    leafletProxy("mapMarker") %>%
      setView(lng = clong, lat = clat, zoom = 15)
    
    leafletProxy('mapMarker') %>%
      addCircles(lng = clong, lat = clat, group = 'circles',
                 stroke = TRUE, radius = radius,
                 color = 'black', weight = 1
                 ,fillOpacity = 0.5)%>%
      addCircles(lng = clong, lat = clat, group = 'centroids', radius = 1, weight = 2,
                 color = 'black',fillColor = 'black',fillOpacity = 1)
    
    leafletProxy('mapMarker', data = locations_within_range) %>%
      addCircles(~Longitude,~Latitude, group = 'locations', stroke = F,
                 radius = 12, fillOpacity = 0.8,fillColor='red', label = as.character(locations_within_range$V1))
    
    #crimes_within_range <- merge(crimes_within_range,crime_type,by = c("LAW_CAT_CD","LAW_CAT_CD"), all.y = F)
    
    
    locations_within_range$Address <- paste(locations_within_range$BUILDING, " ", locations_within_range$STREET, 
                               locations_within_range$BORO, ", New York", locations_within_range$ZIPCODE)
    locations_within_range$Location <- locations_within_range$V1
    output$table <- renderTable(locations_within_range[,c("Location", "Address")])
    
  })

  
}


#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(leaflet)
library(data.table)
library(plotly)
library(shinythemes)
library(shinyWidgets)
library(shinydashboard)

# Define UI for application that draws a histogram
ui<-shinyUI(dashboardPage(skin = "yellow",
                      dashboardHeader(title = "Eating in NYC"),
                      dashboardSidebar(sidebarMenu(
                        menuItem("Background", tabName = "Background", icon = icon("home")),
                        menuItem("Map", tabName = "Map", icon = icon("map")),
                        menuItem("Restaurant Comparison", tabName = "Restaurant Comparison", icon = icon("carrot")),
                        menuItem("Report", tabName = "Report", icon = icon("industry")),
                        menuItem("Menus", tabName = "Menus", icon = icon("receipt"))
                      )),
                      
                      dashboardBody(
                        tabItems(
                          
                          
                          #home
                          tabItem(tabName = "Background",
                                  fluidPage(
                                    fluidRow(
                                      box(width = 15, title = "Introduction", status = "warning",
                                          solidHeader = TRUE,
                                          h4("By ...."),
                                          h5("..."),
                                          h5("..."), 
                                          h5("https://github.com/TZstatsADS/Spring2020-Project2-group-8"))),
                                    
                                    fluidRow(box(width = 15, title = "User Guide", status = "warning",
                                                 solidHeader = TRUE,
                                                 tags$div(tags$ul(
                                                   tags$li("Ranking: ..."),
                                                   tags$li("Map: ...."),
                                                   tags$li("Report: ..."),
                                                   tags$li("Source: ...")
                                                 ))))
                                  )),
                          
                          
                          #mapMarkers
                          tabItem(tabName = "Map",
                                  fluidPage(
                                    fluidRow(column(4, offset=4,
                                                    sliderInput("click_radius", "Radius of area around  the selected address", min=200, max=2000, value=250, step=10))),
                                    
                                    
                                    fluidRow(column(12, 
                                                    leafletOutput("mapMarker", height = "600px"))),
                                    
                                    fluidRow(column(6, offset=4,
                                                    tableOutput("table"))))),
                          
                          
                          #comp
                          tabItem(tabName = "Restaurant Comparison",
                                  fluidPage(
                                    fluidRow(column(3,
                                                    selectInput("rest1", 'Choose a Restaurant',
                                                                choices = levels(all_locations$V1))),
                                             column(3,
                                                    selectInput("rest2", 'Choose a Restaurant',
                                                                choices = levels(all_locations$V1))),
                                             column(3,
                                                    selectInput("rest3", 'Choose a Restaurant',
                                                                choices = levels(all_locations$V1))),
                                             column(3,
                                                    selectInput("rest4", 'Choose a Restaurant',
                                                                choices = levels(all_locations$V1)))))),   
                          
                          
                          #overview
                          tabItem(tabName = "Report",
                                  fluidPage(
                                    fluidRow(column(12,
                                                    h3("Interactive Dashboard"),
                                                    "By default, the bar chart shows the sum of segments by year as the height of each bar, and pie chart shows the percentage of total crime shootings in each borough.",
                                                    tags$div(tags$ul(
                                                      tags$li("Hover the mouse over a year bar in histogram will modify the pie chart and legend."),
                                                      tags$li("Hover the mouse over pie slice should change the histogram.")
                                                    )),
                                                    #htmlOutput("d3"))),
                                                    includeHTML("http://bl.ocks.org/wb2326/raw/d2e92fc05d7b437a7a3a56664e3e49ec/"))),
                                    fluidRow(column(width =  12, title = "Shooting Counts from 2006-2018 by year", 
                                                    plotlyOutput("years"))),
                                    fluidRow(column(width =  12, title = "Shooting Counts by season/year", 
                                                    plotlyOutput("seasons"))),
                                    fluidRow(column(width =  12, title = "Shootings Counts by weekday/year", 
                                                    plotlyOutput("weeks"))),
                                    fluidRow(column(width =  12, title = "Shootings Counts by Victims' sex/year", 
                                                    plotlyOutput("sexs"))),
                                    fluidRow(column(width =  12, title = "Shootings Counts by Victims' age/year", 
                                                    plotlyOutput("ages"))),
                                    fluidRow(column(width =  12, title = "Shootings Counts by Victims' race/year", 
                                                    plotlyOutput("races"))),
                                    fluidRow(column(width =  12, title = "Shootings Counts by boro/year", 
                                                    plotlyOutput("boros"))),
                                    fluidRow(column(width =  12, title = "Shootings Counts by murder/year", 
                                                    plotlyOutput("murders"))))),
                          
                          
                          #source
                          tabItem(tabName = "Menus",
                                  fluidPage(
                                    fluidRow(box(width = 15, title = "Data Source", status = "warning",
                                                 solidHeader = TRUE, "The source data for this project is from", 
                                                 tags$a(href = "https://data.cityofnewyork.us/Health/DOHMH-MenuStat/qgc5-ecnb", 
                                                        "NYC open data"), ".")),
                                    fluidRow(box(width = 15, title = "Project Code", status = "warning",
                                                 solidHeader = TRUE, "The codes for this project are shared at",
                                                 tags$a(href = "https://github.com/TZstatsADS/Spring2020-Project2-group-8",
                                                        "Github"), "."))))
                        ))))



shinyApp(ui = ui, server = server)
