#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(tidyverse)
library(shiny)
library(leaflet)
library(data.table)
library(plotly)
library(shinythemes)
library(shinyWidgets)



shinyUI(
    div(id="canvas",
        
        navbarPage(strong("what to eat",style="color: white;"), 
                   theme=shinytheme("cerulean"),
                   #theme = "bootstrap.min.css",
                   #theme="styles.css",
                   
                   tabPanel("Intro",
                            sidebarPanel(
                                selectInput("restaurants",
                                            h4("choose some restaurants："),
                                            as.list(data_comparison$restaurant%>%unique()),
                                            multiple = T),
                                conditionalPanel('input.restaurants != ""', 
                                                 checkboxGroupInput("category_check", label = h4("Food Category"), 
                                                                    choices = as.list(food_category),
                                                                    selected = 1,
                                                                    inline = F)
                                                 ),
                                conditionalPanel('input.restaurants != ""', 
                                                 selectInput("arrange1",
                                                             label = h4("choose first nuitrition："),
                                                             choice=nutrition),
                                                 conditionalPanel('input.arrange1 != "NA"',
                                                                  checkboxInput("desc1",
                                                                                label = "lower",
                                                                                value = F)
                                                                  )
                                                 ),
                                conditionalPanel('input.arrange1 != "NA"', 
                                                 selectInput("arrange2",
                                                             h4("choose second nuitrition："),
                                                             choice=nutrition),
                                                 conditionalPanel('input.arrange2 != "NA"', 
                                                                  checkboxInput("desc2",
                                                                                label = "lower",
                                                                                value = F)
                                                                  )
                                                 ), 
                                conditionalPanel('input.arrange2 != "NA"', 
                                                 selectInput("arrange3",
                                                             h4("choose third nuitrition："),
                                                             choice=nutrition),
                                                 conditionalPanel('input.arrange3 != "NA"', 
                                                                  checkboxInput("desc3",
                                                                                label = "lower",
                                                                                value = F)
                                                                  )
                                                 )
                                ),
                            mainPanel(
                                conditionalPanel('input.restaurants.length>0 &&(input.arrange1 != "NA"||input.arrange2 != "NA"||input.arrange3 != "NA")', 
                                                 column(12,
                                                        tableOutput ('res1')
                                                        )
                                                 ),
                                conditionalPanel('input.restaurants.length>1 &&(input.arrange1 != "NA"||input.arrange2 != "NA"||input.arrange3 != "NA")', 
                                                 column(12,
                                                        tableOutput ('res2')
                                                 )
                                ),
                                conditionalPanel('input.restaurants.length>2 &&(input.arrange1 != "NA"||input.arrange2 != "NA"||input.arrange3 != "NA")', 
                                                 column(12,
                                                        tableOutput ('res3')
                                                 )
                                ),
                                )
                            ),
                   tabPanel("Intro")

                   #定义选择框
                   #默认状态

        )
        
    )
)   

