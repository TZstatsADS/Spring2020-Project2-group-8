#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#
load("../output/all_locations.RData")

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
