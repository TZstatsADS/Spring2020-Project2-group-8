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


fluidPage(
# shinyUI(
    div(id="canvas",
        navbarPage(strong("what to eat",style="color: white;"),
                   theme=shinytheme("cerulean"),
                   #theme = "bootstrap.min.css",
                   #theme="styles.css
                   tabPanel("Intro",
                            column(2,
                                column(12,
                                       selectInput("restaurants",
                                                   h4("choose some restaurants:"),
                                                   as.list(data_comparison$restaurant%>%unique()),
                                                   multiple = T)
                                       ),
                                column(12,
                                       conditionalPanel('input.restaurants != ""', 
                                                        checkboxGroupInput("category_check", 
                                                                           label = h4("Food Category"), 
                                                                           choices = as.list(food_category),
                                                                           selected = as.list(food_category),
                                                                           inline = F)
                                                        )
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
                                ),
                            column(10,
                                   conditionalPanel('input.restaurants.length>0 &&(input.arrange1 != "NA"||input.arrange2 != "NA"||input.arrange3 != "NA")',
                                                    column(10,checkboxGroupInput("nutrition_show", 
                                                                                label = "Columns in table to show", 
                                                                                choices = as.list(nutrition[-1]%>%names()),
                                                                                selected = as.list(nutrition[-1]%>%names()),
                                                                                inline = T)),
                                                    column(2,numericInput("topn", label = "Number of menu", value = 5),)
                                                    ),
                                   conditionalPanel('input.restaurants.length>0 &&(input.arrange1 != "NA"||input.arrange2 != "NA"||input.arrange3 != "NA")',
                                                    column(12,
                                                           textOutput('res1_name'),
                                                           dataTableOutput ('res1_table')
                                                           ),conditionalPanel('input.res1_table_rows_selected!=""',
                                                                              column(9, 
                                                                                     plotOutput ('res1_plot')
                                                                                     )
                                                                              )
                                                    ),
                                   conditionalPanel('input.restaurants.length>1 &&(input.arrange1 != "NA"||input.arrange2 != "NA"||input.arrange3 != "NA")', 
                                                    column(12,
                                                           textOutput('res2_name'),
                                                           dataTableOutput ('res2_table')
                                                           ),conditionalPanel('input.res2_table_rows_selected!=""',
                                                                              column(9, 
                                                                                     plotOutput ('res2_plot')
                                                                                     )
                                                                              )
                                                    ),
                                   conditionalPanel('input.restaurants.length>2 &&(input.arrange1 != "NA"||input.arrange2 != "NA"||input.arrange3 != "NA")', 
                                                    column(12,
                                                           textOutput('res3_name'),
                                                           dataTableOutput ('res3_table')
                                                           ),conditionalPanel('input.res3_table_rows_selected!=""',
                                                                              column(9, 
                                                                                     plotOutput ('res3_plot')
                                                                                     )
                                                                              )
                                                    ),
                                   )
                            ),
                   tabPanel("Intro"
                            )
                   )
        )
    )   

