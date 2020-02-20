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
shinyUI(dashboardPage(skin = "yellow",
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
        