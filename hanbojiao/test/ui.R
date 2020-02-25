shinyUI(
  div(id="canvas",
      navbarPage(strong("Eating Healthy in NYC",style="color: grey;"),
                 theme=shinytheme("cerulean"),
                 ## theme = "bootstrap.min.css",
                 ## theme="styles.css
                 tabPanel("Intro",
                          mainPanel(width=12,
                                    h1(strong("Project: Open Data NYC - an RShiny app development project"),
                                       style = "color:black; "),
                                    br(),
                                    br(),
                                    p(strong("Do you know on average each year there are 134 millions fire incidents happening in the New York, sixty three hundred firefighter's injuries and death recorded, with direct property damages over 14 billions?",
                                             style = "color:black; font-size:16pt"),
                                      br(),
                                      br(),
                                      p(strong("Indeed, fire loss is devastating!"),
                                        style = "color:black; font-size:16pt"),
                                      br(),
                                      br(),
                                      p(strong("In this project, we have developed an App using R Shiny to visualize NYC fire incident data. This App can not only help the government and FDNY to have better policy-makings, provide useful information for insurance companies to design more profitable quotes regarding the property insurance and guide the residents to get access to those fire incidents in NYC.",
                                               style = "color:black; font-size:16pt")),
                                      br(),br(),br(), br(),br(),br(),
                                      br(),br(),br(), br(),
                                      br(),br(),br(),
                                      p(em(a("Github link",href="https://github.com/TZstatsADS/Spring2020-Project2-Group8",style = "color:black")))
                                    )
                          )
                 ),
                 tabPanel("Map",
                          fluidRow(
                            column(4,
                                   offset=4,
                                   sliderInput("click_radius",
                                               "Radius of area around  the selected address",
                                               min=500, max=2000, value=500, step=20)
                                   )
                            ),
                          fluidRow(
                            column(12,
                                   leafletOutput("mapMarker",
                                                 height = "600px")
                                   )
                            ),
                          fluidRow(
                            column(6,
                                   offset=4,
                                   tableOutput("table")
                                   )
                            )
                          ),
                 tabPanel("Comparison",
                          column(2,
                                 column(12,
                                        selectizeInput("restaurants",
                                                       h4("Choose Restaurant(s):"),
                                                       as.list(data_comparison$restaurant%>%unique()),
                                                       multiple = T,options=list(minItems=1,maxItems=3))
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
                                                              label = h4("Choose First Nutrition Fact:"),
                                                              choice=c("Select",nutrition)),
                                                  conditionalPanel('input.arrange1 != "Select"',
                                                                   checkboxInput("desc1",
                                                                                 label = "lower",
                                                                                 value = F)
                                                  )
                                 ),
                                 conditionalPanel('input.arrange1 != "Select"&&input.restaurants != ""',
                                                  selectInput("arrange2",
                                                              h4("Choose Second Nutrition Fact:"),
                                                              choice=c("Select",nutrition)),
                                                  conditionalPanel('input.arrange2 != "Select"',
                                                                   checkboxInput("desc2",
                                                                                 label = "lower",
                                                                                 value = F)
                                                  )
                                 ),
                                 conditionalPanel('input.arrange2 != "Select"&&input.arrange1 != "Select"&&input.restaurants != ""',
                                                  selectInput("arrange3",
                                                              h4("Choose Third Nutrition Fact:"),
                                                              choice=c("Select",nutrition)),
                                                  conditionalPanel('input.arrange3 != "Select"',
                                                                   checkboxInput("desc3",
                                                                                 label = "lower",
                                                                                 value = F)
                                                  )
                                 ),
                          ),
                          column(10,
                                 conditionalPanel('input.restaurants.length>0 &&(input.arrange1 != "Select")',
                                                  column(10,checkboxGroupInput("nutrition_show",
                                                                               label = "Choose Table Columns To Show",
                                                                               choices = as.list(nutrition[-1]%>%names()),
                                                                               selected = as.list(nutrition[-1]%>%names()),
                                                                               inline = T)),
                                                  column(2,numericInput("topn", label = "Number of menu items", value = 5),)
                                 ),
                                 conditionalPanel('input.restaurants.length>0 &&input.arrange1 != "Select"',
                                                  column(12,
                                                         textOutput('res1_name'),
                                                         dataTableOutput ('res1_table')
                                                  ),conditionalPanel('input.res1_table_rows_selected!=""',
                                                                     column(12,
                                                                            plotlyOutput ('res1_plot')
                                                                     )
                                                  )
                                 ),
                                 conditionalPanel('input.restaurants.length>1 &&input.arrange1 != "Select"',
                                                  column(12,
                                                         textOutput('res2_name'),
                                                         dataTableOutput ('res2_table')
                                                  ),conditionalPanel('input.res2_table_rows_selected!=""',
                                                                     column(12,
                                                                            plotlyOutput ('res2_plot')
                                                                     )
                                                  )
                                 ),
                                 conditionalPanel('input.restaurants.length>2 &&input.arrange1 != "Select"',
                                                  column(12,
                                                         textOutput('res3_name'),
                                                         dataTableOutput ('res3_table')
                                                  ),conditionalPanel('input.res3_table_rows_selected!=""',
                                                                     column(12,
                                                                            plotlyOutput ('res3_plot')
                                                                     )
                                                  )
                                 ),
                          )
                 ),
                 tabPanel("Statistic Analysis",
                          h2("Summary Statistics"),
                          wellPanel(style = "overflow-y:scroll; height: 850px; max-height: 750px;  background-color: #ffffff;",
                                    tabsetPanel(type="tabs",
                                                tabPanel(title = "Calories",
                                                         br(),
                                                         div(plotlyOutput("p1"),
                                                             align = "center")
                                                         ),
                                                tabPanel(title = "Protein",
                                                           br(),
                                                         div(plotlyOutput("p2"),
                                                             align = "center")
                                                         ),
                                                tabPanel(title = "Total Fat",
                                                         br(),
                                                         div(plotlyOutput("p3"),
                                                             align = "center")
                                                         ),
                                                tabPanel(title = "Carbohydrates",
                                                         div(width = 15,
                                                             h1("Restaurants with Low Carbohydrates content"),
                                                             br(),
                                                             plotlyOutput('p7'),
                                                             h1("Restaurants with High Carbohydrates content"),
                                                             br(),
                                                             plotlyOutput('p8')
                                                             )
                                                         ),
                                                tabPanel(title = "Sodium",
                                                         div(width = 15,
                                                             h1("Sodium"),
                                                             plotlyOutput("p9"),
                                                             h1("Restaurants with High Sodium content"),
                                                             br(),
                                                             plotlyOutput("p10")
                                                             )
                                                         ), 
                                                tabPanel(title = "Sugar",
                                                         div(width = 15,
                                                             h1("Sugar"),
                                                             br(),
                                                             plotlyOutput("p11"),
                                                             h1("Restaurants with High Sugar content"),
                                                             br(),
                                                             plotlyOutput("p12"),
                                                             h1("Restaurants with Low Sugar content"),
                                                             br(),
                                                             plotlyOutput("plow")
                                                             )
                                                         ),
                                                tabPanel(title = "Dietary_fiber",
                                                         div(width = 15,
                                                             h1("Dietary_fiber"),
                                                             br(),
                                                             plotlyOutput("p13"),
                                                             h1("Restaurants with High Dietary Fiber content"),
                                                             br(),
                                                             plotlyOutput("p14")  
                                                             )
                                                         ),
                                                tabPanel(title = "Cholesterol",
                                                         div(width = 15,
                                                             h1("High Cholesterol"),
                                                             br(),
                                                             plotlyOutput('p5'),
                                                             h1("Low Cholesterol"),
                                                             br(),
                                                             plotlyOutput('p6')
                                                             )
                                                         )
                                                )
                                    )
                          ),
                 tabPanel("Data Search",
                          tabsetPanel(type="tabs",
                                      tabPanel("menu", dataTableOutput ('search_menu')),
                                      tabPanel("location",
                                               column(12,
                                                      column(3,
                                                             selectInput("restaurants_search_menu",
                                                                         h4("Restaurants:"),
                                                                         as.list(data_search_location$restaurant%>%as.character()%>%unique()%>%sort()),
                                                                         multiple = T)),
                                                      column(3,selectInput("BORO_search_menu",
                                                                           h4("BORO:"),
                                                                           as.list(data_search_location$BORO%>%as.character()%>%unique()%>%sort()),
                                                                           multiple = T)),
                                                      column(3,selectInput("cuisine_search_menu",
                                                                           h4("Cuisine:"),
                                                                           as.list(data_search_location$`CUISINE DESCRIPTION`%>%as.character()%>%unique()%>%sort()),
                                                                           multiple = T)),
                                                      column(3,selectInput("grade_search_menu",
                                                                           h4("Grade:"),
                                                                           as.list(data_search_location$GRADE%>%as.character()%>%unique()%>%sort()),
                                                                           multiple = T)),),
                                               column(12,
                                                      dataTableOutput ('search_location')))
                          )
                 )
      )
  )
)   



