
packages.used=c("tidyverse","shiny", "shinythemes","DT","datasets","plotly","shinyWidgets","data.table","shinydashboard","googleVis","geosphere",
                "leaflet.extras","ggmap","leaflet","dplyr","RColorBrewer","viridis","gridExtra")
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
library(data.table)
library(shinydashboard)
library(googleVis)
library(geosphere)
library(leaflet.extras)
library(ggmap)
library(leaflet)
library(dplyr)
library(RColorBrewer)
library(viridis)
library(gridExtra)
#Statistics Analysis Global Enviroment 

#Loading the required data:

load("data_comparison.RData")
load("data_search.RData")
load("all_locations.RData")


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



############ stat data prepare
load("menu_nutrition_location_catagory.RData")



menu_clean <- menu_nutrition_location_catagory


# 
# 
# ##Catogories and its Nutritional Value {.tabset .tabset-fade}  
# 
# subset_nutrition <-menu_clean %>%select(restaurant,menu)%>%unnest(menu)%>%
#   select(restaurant,Item_Name,Item_Description,Food_Category,Calories:Dietary_Fiber,Shareable)%>%
#   filter(Shareable==0)%>%select(-Potassium,-Shareable)%>%group_by(restaurant)%>%arrange(restaurant)%>%nest()%>%ungroup%>%mutate(restaurant_id=row_number())%>%group_by(restaurant)%>%unnest()%>%mutate(menu_id=row_number())%>%
#   select(restaurant_id,restaurant,menu_id,Item_Name,Item_Description,Food_Category,everything())
# 
# #colname<-subset_nutrition%>%colnames()
# #nutrition<-colname[9:18]
# #food_category<-subset_nutrition$Food_Category%>%unique()
# 
# 
# ### Replacing some NA values:
# 
# subset_nutrition$Calories[which(is.na(subset_nutrition$Calories))] = 0
# subset_nutrition$Total_Fat[which(is.na(subset_nutrition$Total_Fat))] = 0
# subset_nutrition$Saturated_Fat[which(is.na(subset_nutrition$Saturated_Fat))] = 0 
# subset_nutrition$Trans_Fat[which(is.na(subset_nutrition$Trans_Fat))] = 0
# subset_nutrition$Cholesterol[which(is.na(subset_nutrition$Cholesterol))] = 0
# subset_nutrition$Sodium[which(is.na(subset_nutrition$Sodium))] = 0
# subset_nutrition$Carbohydrates[which(is.na(subset_nutrition$Carbohydrates))] = 0
# subset_nutrition$Protein[which(is.na(subset_nutrition$Protein))] = 0
# subset_nutrition$Sugar[which(is.na(subset_nutrition$Sugar))] = 0
# subset_nutrition$Dietary_Fiber[which(is.na(subset_nutrition$Dietary_Fiber))] = 0
# 
# 
# 
# 
# ### Calories  
# 
# 
# new_col<-c("grey50", "blue","hotpink","Magenta","orange", "red","seagreen","violet","brown","maroon","navyblue", "yellow")
# p1 <- plot_ly(x = subset_nutrition$Food_Category,  y = subset_nutrition$Calories,color = subset_nutrition$Food_Category,colors = new_col, type = "box")%>%
#   layout(title = "Calories",
#          xaxis = list(title = ""),
#          yaxis = list(title = "Calories"),
#          showlegend=FALSE,
#          autosize = T)
# 
#   
# 
# ### Protein
# 
# 
# protein_content<-subset_nutrition%>% select(restaurant,Food_Category,Protein)%>% group_by(Food_Category)%>%summarise(tprotein = mean(Protein))
# 
# p2 <-  plot_ly(
#   x = protein_content$tprotein,
#   y = protein_content$Food_Category,
#   marker = list(color = new_col),
#   type = "bar"
# )
# 
# 
# 
# ### Total_fat
# 
# 
# p3 <- plot_ly(x = subset_nutrition$Food_Category, y=subset_nutrition$Total_Fat,color = subset_nutrition$Food_Category,colors =new_col , type = "box") %>% 
#   layout(title = "Total Fat",
#          xaxis = list(title = ""),
#          yaxis = list(title = ""),
#          showlegend=FALSE,
#          autosize = T)
# 
# 
# 
# ### Cholesterol
# 
# 
# 
# cholesterol_content<-subset_nutrition%>% select(restaurant,Food_Category,Cholesterol)%>% group_by(Food_Category)%>%summarise(tcholesterol = mean(Cholesterol))
# 
# high_ch <- cholesterol_content%>%arrange(desc(tcholesterol))%>% head(6)
# 
# p5<-plot_ly(x= high_ch$Food_Category,y=high_ch$tcholesterol,
#             color = high_ch$food_category,type="bar")%>%layout(title="High Cholesterol Category",height=400)
# 
# 
# low_ch <-cholesterol_content%>%arrange(desc(tcholesterol))%>% tail(6)
# p6 <-plot_ly(x= low_ch$Food_Category,y=low_ch$tcholesterol,
#              color = low_ch$food_category,type="bar")%>%layout(title="Low Cholesterol Category",height=400)
# 
# 
# 
# 
# 
# 
# ### Carbohydrates
# 
# carb_content<-subset_nutrition%>% select(restaurant,Food_Category,Carbohydrates)%>% group_by(restaurant)%>%summarise(tcarb = mean(Carbohydrates))
# 
# low_carb <- carb_content%>%arrange(desc(tcarb))%>%tail(26)
# 
# p7<-plot_ly(x= low_carb$restaurant,y=low_carb$tcarb,
#             color = low_carb$restaurant,type="bar")%>%layout(title="Carbohydrates content by Restaurant",height=400)
# 
# 
# 
# 
# 
# 
# 
# high_carb <- carb_content%>%arrange(desc(tcarb))%>%head(26)
# 
# p8<-plot_ly(x= high_carb$restaurant,y=high_carb$tcarb,
#             color = high_carb$restaurant,type="bar")%>%layout(title="Carbohydrates content by Restaurant",height=400)
# 
# 
# 
# 
# ### Sodium
# 
# 
# sodium_content<-subset_nutrition%>% select(restaurant,Food_Category,Sodium)%>% group_by(restaurant)%>%summarise(tsodium = mean(Sodium))
# 
# low_sodium <- sodium_content%>%arrange(desc(tsodium))%>%head(26)
# 
# p9<-plot_ly(x= low_sodium$restaurant,y=low_sodium$tsodium,
#             color = low_sodium$restaurant,type="bar")%>%layout(title="Top low sodium Restaurants",height=400)
# 
# 
# 
# 
# 
# 
# 
# sodium_content1<-subset_nutrition%>% select(restaurant,Food_Category,Sodium)%>% group_by(restaurant)%>%summarise(tsodium1 = mean(Sodium))
# 
# high_sodium1 <- sodium_content1%>%arrange(desc(tsodium1))%>%head(26)
# 
# p10<-plot_ly(x= high_sodium1$restaurant,y=high_sodium1$tsodium1,
#              color = high_sodium1$restaurant,type="bar")%>%layout(title="Top High Sodium Restaurants",height=400)
# 
# 
# 
# 
# 
# 
# 
# ### Sugar
# 
# 
# 
# 
# sugar_content<-subset_nutrition%>% select(restaurant,Food_Category,Sugar)%>% group_by(Food_Category)%>%summarise(tsugar = mean(Sugar))
# 
# 
# p11 <- plot_ly(
#   x = sugar_content$tsugar,
#   y = sugar_content$Food_Category,
#   marker = list(color = new_col),
#   type = "bar"
# )
# 
# 
# 
# 
# 
# 
# sugar_content<-subset_nutrition%>% select(restaurant,Food_Category,Sugar)%>% group_by(restaurant)%>%summarise(tsugar = mean(Sugar))
# 
# high_sugar <- sugar_content%>%arrange(desc(tsugar))%>%head(26)
# 
# p12<-plot_ly(x= high_sugar$restaurant,y=high_sugar$tsugar,
#              color = high_sugar$restaurant,type="bar")%>%layout(title="Top High Sugar Restaurants",height=400)
# 
# 
# low_sugar <- sugar_content%>%arrange(desc(tsugar))%>%tail(26)
# 
# plow<-plot_ly(x= low_sugar$restaurant,y=low_sugar$tsugar,
#               color = low_sugar$restaurant,type="bar")%>%layout(title="Top low Sugar Restaurants",height=400)
# 
# 
# 
# 
# ### Dietary_fiber
# 
# 
# 
# p13 <- plot_ly(x = subset_nutrition$Food_Category, y=subset_nutrition$Dietary_Fiber,color = subset_nutrition$Food_Category,colors =new_col , type = "bar") %>% layout(
#   xaxis = list(title = ""),
#   yaxis = list(title = ""),
#   showlegend=FALSE,
#   autosize = T)
# 
# 
# 
# 
# 
# 
# 
# df_content<-subset_nutrition%>% select(restaurant,Food_Category,Dietary_Fiber)%>% group_by(restaurant)%>%summarise(tdf = mean(Dietary_Fiber))
# 
# 
# high_df <- df_content%>%arrange(desc(tdf))%>%head(26)
# 
# p14<-plot_ly(x= high_df$restaurant,y=high_df$tdf,
#              color = high_df$restaurant,type="bar")%>%layout(title="Top High Dietary Fiber Restaurants",height=400)
