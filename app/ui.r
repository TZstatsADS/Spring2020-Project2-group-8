shinyUI(
  div(id="canvas",
      navbarPage("Eating Healthy in NYC",
        # strong("Eating Healthy in NYC",style="color: white;"),
                 theme=shinytheme("united"),
                 ## theme = "bootstrap.min.css",
                 ## theme="styles.css
                 
                 tabPanel("Intro",
                          mainPanel(width=12,
                                    h1(strong("Eating Healthy in NYC - an RShiny app project"),
                                       style = "color:black; ",align="center"),
                                    br(),
                                    br(),
                                    p(em("Do you know what are the nutrients of menu items from national restaurant chains? 
                                             Nowadays, customers care more and more about the nutritional value of the foods they’re eating. 
                                             Although some restaurants already include calorie information and other nutrition information in their menus, 
                                             many customers want to see more details and compare similar items in different restaurants. ",
                                             style = "color:black; font-size:13pt"),
                                      br(),
                                      br(),
                                      h3(strong("Our Goal:"),
                                        style = "color:black; font-size:16pt", align="center"),
                                      p("Inspired by how nutritional values affect human health, we aim to develop an App using R shiny to visualize the most common nutrients and the menu information of the top national restaurant chains, 
                                               following with these restaurants’ location in New York City. This app does not only give users insight into the nutritional values of top restaurants,
                                               but also provides a useful tool for finding and comparing nearby restaurants.",
                                               style = "color:black; font-size:13pt"),
                                      br(),br(),br(), 
                                      
                                      h3(strong("User Guide:"),
                                        style = "color:black; font-size:16pt", align="center"),
                                      p("Map: This part contains a map of NYC. The user can click a location on the map and view the restaurants in that area.",  style = "color:black; font-size:13pt"),
                                      p("Comparison: Choose the restaurants, food types, and nurtitional facts you want to explore/compare. Click a food item to see a breakdown of its contents.",  style = "color:black; font-size:13pt"),
                                      p("Statistics Analysis: This part contains some interactive graphs and bar charts that help users to better understand of all main nutritional factors and provide the list of low or high content of each specific nutrient per each restaurant. ",  style = "color:black; font-size:13pt"),
                                      p("Data Search: Search the menu data or restaurant location data used to develop our app.",  style = "color:black; font-size:13pt"),
                                      br(), br(), br(),
                                      
                                      h3(strong("Limitations:"),
                                        style = "color:black; font-size:16pt", align="center"),
                                      p("The dataset does not include all restaurants. We eliminated the restaurants that do not have enough information to analyze.",  style = "color:black; font-size:13pt"),
                                      p("This dataset contains mostly fast food restaurant chains; thus, the menu items may tend to be unhealthier.",  style = "color:black; font-size:13pt"),
                                      br(),
                                      
                                      br(),br(),
                                      p(em(a("Github link",href="https://github.com/TZstatsADS/Spring2020-Project2-group-8",style = "color:black")))
                                    )
                          )
                 ),
                 tabPanel("Map",
                          fluidRow(
                            column(4,
                                   offset=4,
                                   sliderInput("click_radius",
                                               "Radius (in meters) of area around the selected location",
                                               min=500, max=2000, value=750, step=10)
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
                 tabPanel("Nutrition Comparison",
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
                 tabPanel("Statistics Analysis",
                          h2("Summary Statistics"),
                          wellPanel(style = "overflow-y:scroll; height: 850px; max-height: 750px;  background-color: #ffffff;",
                                    tabsetPanel(type="tabs",
                                                tabPanel("Nutrients Contributing to Calories",
                                                         plotOutput(outputId="plotgraph", width="800px",height="600px")),
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
                                                             h1("Restaurant with Low Carbohydrates content"),
                                                             br(),
                                                             plotlyOutput('p7'),
                                                             h1("Restaurant with High Carbohydrates content"),
                                                             br(),
                                                             plotlyOutput('p8')
                                                             )
                                                         ),
                                                tabPanel(title = "Sodium",
                                                         div(width = 15,
                                                             h1("Sodium"),
                                                             plotlyOutput("p9"),
                                                             h1("Restaurant with High Sodium content"),
                                                             br(),
                                                             plotlyOutput("p10")
                                                             )
                                                         ), 
                                                tabPanel(title = "Sugar",
                                                         div(width = 15,
                                                             h1("Sugar"),
                                                             br(),
                                                             plotlyOutput("p11"),
                                                             h1("Restaurant with High Sugar content"),
                                                             br(),
                                                             plotlyOutput("p12"),
                                                             h1("Restaurant with Low Sugar content"),
                                                             br(),
                                                             plotlyOutput("plow")
                                                             )
                                                         ),
                                                tabPanel(title = "Dietary_fiber",
                                                         div(width = 15,
                                                             h1("Dietary_fiber"),
                                                             br(),
                                                             plotlyOutput("p13"),
                                                             h1("Restaurant with High Dietary Fiber content"),
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



