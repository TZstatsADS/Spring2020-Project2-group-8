
packages.used=c("DT","tidyverse", "shiny","shinythemes","shinythemes","datasets","shinyWidgets","plotly")
packages.needed=setdiff(packages.used,
                        intersect(installed.packages()[,1],
                                  packages.used))

if(length(packages.needed)>0){
  install.packages(packages.needed, dependencies = TRUE)
}
library(tidyverse)
library(shiny)
library(shinythemes)
library(DT)
library(datasets)
library(plotly)
library(shinyWidgets)

#Statistics Analysis Global Enviroment 

#Loading the required data:

load("data_comparison.RData")
load("data_search.RData")

data_comparison<-
  data_comparison%>%mutate(Calories_percent=percent_rank(Calories)%>%round(2),
                         Total_Fat_percent=percent_rank(Total_Fat)%>%round(2),
                         Saturated_Fat_percent=percent_rank(Saturated_Fat)%>%round(2),
                         Trans_Fat_percent=percent_rank(Trans_Fat)%>%round(2),
                         Cholesterol_percent=percent_rank(Cholesterol)%>%round(2),
                         Sodium_percent=percent_rank(Sodium)%>%round(2),
                         Carbohydrates_percent=percent_rank(Carbohydrates)%>%round(2),
                         Protein_percent=percent_rank(Protein)%>%round(2),
                         Sugar_percent=percent_rank(Sugar)%>%round(2),
                         Dietary_Fiber_percent=percent_rank(Dietary_Fiber)%>%round(2))

nutrition<-c("Calories"="Calories_percent", "Total_Fat" ="Total_Fat_percent", "Saturated_Fat"="Saturated_Fat_percent", "Trans_Fat" ="Trans_Fat_percent", "Cholesterol"="Cholesterol_percent","Sodium"="Sodium_percent","Carbohydrates" ="Carbohydrates_percent","Protein" ="Protein_percent", "Sugar"="Sugar_percent","Dietary_Fiber"="Dietary_Fiber_percent" )



###########


