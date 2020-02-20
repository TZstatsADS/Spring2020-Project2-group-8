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
# library(leaflet)
# library(data.table)
# library(plotly)
library(shinythemes)
library(shinyWidgets)



shinyUI(
    div(id="canvas",
        navbarPage(strong("what to eat",style="color: white;"),
                   theme=shinytheme("cerulean"),
                   #theme = "bootstrap.min.css",
                   #theme="styles.css
                   tabPanel("Intro",
                            sidebarPanel(
                                selectInput("restaurants",
                                            h4("choose some restaurants:"),
                                            as.list(data_comparison$restaurant%>%unique()),
                                            multiple = T),
                                conditionalPanel('input.restaurants != ""', 
                                                 checkboxGroupInput("category_check", label = h4("Food Category"), 
                                                                    choices = as.list(food_category),
                                                                    selected = as.list(food_category),
                                                                    inline = F)
                                                 ),
                                conditionalPanel('input.restaurants != ""', 
                                                 selectInput("arrange1",
                                                             label = h4("choose first nuitrition:"),
                                                             choice=nutrition),
                                                 conditionalPanel('input.arrange1 != "NA"',
                                                                  checkboxInput("desc1",
                                                                                label = "lower",
                                                                                value = F)
                                                                  )
                                                 ),
                                conditionalPanel('input.arrange1 != "NA"', 
                                                 selectInput("arrange2",
                                                             h4("choose second nuitrition:"),
                                                             choice=nutrition),
                                                 conditionalPanel('input.arrange2 != "NA"', 
                                                                  checkboxInput("desc2",
                                                                                label = "lower",
                                                                                value = F)
                                                                  )
                                                 ), 
                                conditionalPanel('input.arrange2 != "NA"', 
                                                 selectInput("arrange3",
                                                             h4("choose third nuitrition:"),
                                                             choice=nutrition),
                                                 conditionalPanel('input.arrange3 != "NA"', 
                                                                  checkboxInput("desc3",
                                                                                label = "lower",
                                                                                value = F)
                                                                  )
                                                 ),
                                conditionalPanel('input.restaurants.length>0 &&(input.arrange1 != "NA"||input.arrange2 != "NA"||input.arrange3 != "NA")',
                                                 checkboxGroupInput("menuid1", label = h4("Choose menu from restaurant 1"), 
                                                                    choices = 1:10,
                                                                    inline = T)
                                                 ),
                                conditionalPanel('input.restaurants.length>1 &&(input.arrange1 != "NA"||input.arrange2 != "NA"||input.arrange3 != "NA")',
                                                 checkboxGroupInput("menuid2", label = h4("Choose menu from restaurant 2"), 
                                                                    choices = 1:10,
                                                                    inline = T)
                                                 ),
                                conditionalPanel('input.restaurants.length>2 &&(input.arrange1 != "NA"||input.arrange2 != "NA"||input.arrange3 != "NA")',
                                                 checkboxGroupInput("menuid3", label = h4("Choose menu from restaurant 3"), 
                                                                    choices = 1:10,
                                                                    inline = T)
                                                 )
                                ),
                            mainPanel(
                                conditionalPanel('input.restaurants.length>0 &&(input.arrange1 != "NA"||input.arrange2 != "NA"||input.arrange3 != "NA")', 
                                                 column(12,
                                                        textOutput('res1_name'),
                                                        tableOutput ('res1_table')
                                                        ),conditionalPanel('input.menuid1!=""',
                                                                           column(9, plotOutput ('res1_plot'))
                                                        
                                                        )
                                                 ),
                                conditionalPanel('input.restaurants.length>1 &&(input.arrange1 != "NA"||input.arrange2 != "NA"||input.arrange3 != "NA")', 
                                                 column(12,
                                                        textOutput('res2_name'),
                                                        tableOutput ('res2_table')
                                                        ),conditionalPanel('input.menuid2!=""',
                                                                           column(9, plotOutput ('res2_plot'))
                                                        )
                                                 ),
                                conditionalPanel('input.restaurants.length>2 &&(input.arrange1 != "NA"||input.arrange2 != "NA"||input.arrange3 != "NA")', 
                                                 column(12,
                                                        textOutput('res3_name'),
                                                        tableOutput ('res3_table')
                                                        ),conditionalPanel('input.menuid3!=""',
                                                                           column(9, plotOutput ('res3_plot'))
                                                        )
                                                 ),
                                )
                            ),
                   tabPanel("Intro"
                            )
                   )
        )
    )   

