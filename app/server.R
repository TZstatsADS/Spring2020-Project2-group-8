source("global.r")



##Catogories and its Nutritional Value {.tabset .tabset-fade}  

subset_nutrition <-menu_clean %>%select(restaurant,menu)%>%unnest(menu)%>%
  select(restaurant,Item_Name,Item_Description,Food_Category,Calories:Dietary_Fiber,Shareable)%>%
  filter(Shareable==0)%>%select(-Potassium,-Shareable)%>%group_by(restaurant)%>%arrange(restaurant)%>%nest()%>%ungroup%>%mutate(restaurant_id=row_number())%>%group_by(restaurant)%>%unnest()%>%mutate(menu_id=row_number())%>%
  select(restaurant_id,restaurant,menu_id,Item_Name,Item_Description,Food_Category,everything())

#colname<-subset_nutrition%>%colnames()
#nutrition<-colname[9:18]
#food_category<-subset_nutrition$Food_Category%>%unique()


### Replacing some NA values:

subset_nutrition$Calories[which(is.na(subset_nutrition$Calories))] = 0
subset_nutrition$Total_Fat[which(is.na(subset_nutrition$Total_Fat))] = 0
subset_nutrition$Saturated_Fat[which(is.na(subset_nutrition$Saturated_Fat))] = 0 
subset_nutrition$Trans_Fat[which(is.na(subset_nutrition$Trans_Fat))] = 0
subset_nutrition$Cholesterol[which(is.na(subset_nutrition$Cholesterol))] = 0
subset_nutrition$Sodium[which(is.na(subset_nutrition$Sodium))] = 0
subset_nutrition$Carbohydrates[which(is.na(subset_nutrition$Carbohydrates))] = 0
subset_nutrition$Protein[which(is.na(subset_nutrition$Protein))] = 0
subset_nutrition$Sugar[which(is.na(subset_nutrition$Sugar))] = 0
subset_nutrition$Dietary_Fiber[which(is.na(subset_nutrition$Dietary_Fiber))] = 0




### Calories  


new_col<-c("grey50", "blue","hotpink","Magenta","orange", "red","seagreen","violet","brown","maroon","navyblue", "yellow")


### Calories
Calories_content<-subset_nutrition%>% select(restaurant,Food_Category,Calories)%>% group_by(Food_Category)%>%summarise(tCalories = mean(Calories))

pcal1<- plot_ly(
  x = Calories_content$tCalories,
  y = Calories_content$Food_Category,
  marker = list(color = new_col),
  type = "bar"
)

Calories_content<-subset_nutrition%>% select(restaurant,Food_Category,Calories)%>% group_by(restaurant)%>%summarise(tCalories = mean(Calories))

high_Calories <- Calories_content%>%arrange(desc(tCalories))%>%head(26)

pcal2<-plot_ly(x= high_Calories$restaurant,y=high_Calories$tCalories,
               color = high_Calories$restaurant,type="bar")%>%layout(title="Top High Calories Restaurants",height=400)


low_Calories <- Calories_content%>%arrange(desc(tCalories))%>%tail(26)

pcal3<-plot_ly(x= low_Calories$restaurant,y=low_Calories$tCalories,
               color = low_Calories$restaurant,type="bar")%>%layout(title="Top low Calories Restaurants",height=400)




### Protein
Protein_content<-subset_nutrition%>% select(restaurant,Food_Category,Protein)%>% group_by(Food_Category)%>%summarise(tProtein = mean(Protein))

ppro1<- plot_ly(
  x = Protein_content$tProtein,
  y = Protein_content$Food_Category,
  marker = list(color = new_col),
  type = "bar"
)

Protein_content<-subset_nutrition%>% select(restaurant,Food_Category,Protein)%>% group_by(restaurant)%>%summarise(tProtein = mean(Protein))

high_Protein <- Protein_content%>%arrange(desc(tProtein))%>%head(26)

ppro2<-plot_ly(x= high_Protein$restaurant,y=high_Protein$tProtein,
               color = high_Protein$restaurant,type="bar")%>%layout(title="Top High Protein Restaurants",height=400)


low_Protein <- Protein_content%>%arrange(desc(tProtein))%>%tail(26)

ppro3<-plot_ly(x= low_Protein$restaurant,y=low_Protein$tProtein,
               color = low_Protein$restaurant,type="bar")%>%layout(title="Top low Protein Restaurants",height=400)




### Total_Fat
Total_Fat_content<-subset_nutrition%>% select(restaurant,Food_Category,Total_Fat)%>% group_by(Food_Category)%>%summarise(tTotal_Fat = mean(Total_Fat))

pfat1<- plot_ly(
  x = Total_Fat_content$tTotal_Fat,
  y = Total_Fat_content$Food_Category,
  marker = list(color = new_col),
  type = "bar"
)

Total_Fat_content<-subset_nutrition%>% select(restaurant,Food_Category,Total_Fat)%>% group_by(restaurant)%>%summarise(tTotal_Fat = mean(Total_Fat))

high_Total_Fat <- Total_Fat_content%>%arrange(desc(tTotal_Fat))%>%head(26)

pfat2<-plot_ly(x= high_Total_Fat$restaurant,y=high_Total_Fat$tTotal_Fat,
               color = high_Total_Fat$restaurant,type="bar")%>%layout(title="Top High Total_Fat Restaurants",height=400)


low_Total_Fat <- Total_Fat_content%>%arrange(desc(tTotal_Fat))%>%tail(26)

pfat3<-plot_ly(x= low_Total_Fat$restaurant,y=low_Total_Fat$tTotal_Fat,
               color = low_Total_Fat$restaurant,type="bar")%>%layout(title="Top low Total_Fat Restaurants",height=400)


### Cholesterol
Cholesterol_content<-subset_nutrition%>% select(restaurant,Food_Category,Cholesterol)%>% group_by(Food_Category)%>%summarise(tCholesterol = mean(Cholesterol))

pcho1<- plot_ly(
  x = Cholesterol_content$tCholesterol,
  y = Cholesterol_content$Food_Category,
  marker = list(color = new_col),
  type = "bar"
)

Cholesterol_content<-subset_nutrition%>% select(restaurant,Food_Category,Cholesterol)%>% group_by(restaurant)%>%summarise(tCholesterol = mean(Cholesterol))

high_Cholesterol <- Cholesterol_content%>%arrange(desc(tCholesterol))%>%head(26)

pcho2<-plot_ly(x= high_Cholesterol$restaurant,y=high_Cholesterol$tCholesterol,
               color = high_Cholesterol$restaurant,type="bar")%>%layout(title="Top High Cholesterol Restaurants",height=400)


low_Cholesterol <- Cholesterol_content%>%arrange(desc(tCholesterol))%>%tail(26)

pcho3<-plot_ly(x= low_Cholesterol$restaurant,y=low_Cholesterol$tCholesterol,
               color = low_Cholesterol$restaurant,type="bar")%>%layout(title="Top low Cholesterol Restaurants",height=400)





### Carbohydrates
Carbohydrates_content<-subset_nutrition%>% select(restaurant,Food_Category,Carbohydrates)%>% group_by(Food_Category)%>%summarise(tCarbohydrates = mean(Carbohydrates))

pcar1<- plot_ly(
  x = Carbohydrates_content$tCarbohydrates,
  y = Carbohydrates_content$Food_Category,
  marker = list(color = new_col),
  type = "bar"
)

Carbohydrates_content<-subset_nutrition%>% select(restaurant,Food_Category,Carbohydrates)%>% group_by(restaurant)%>%summarise(tCarbohydrates = mean(Carbohydrates))

high_Carbohydrates <- Carbohydrates_content%>%arrange(desc(tCarbohydrates))%>%head(26)

pcar2<-plot_ly(x= high_Carbohydrates$restaurant,y=high_Carbohydrates$tCarbohydrates,
               color = high_Carbohydrates$restaurant,type="bar")%>%layout(title="Top High Carbohydrates Restaurants",height=400)


low_Carbohydrates <- Carbohydrates_content%>%arrange(desc(tCarbohydrates))%>%tail(26)

pcar3<-plot_ly(x= low_Carbohydrates$restaurant,y=low_Carbohydrates$tCarbohydrates,
               color = low_Carbohydrates$restaurant,type="bar")%>%layout(title="Top low Carbohydrates Restaurants",height=400)




### Sodium
Sodium_content<-subset_nutrition%>% select(restaurant,Food_Category,Sodium)%>% group_by(Food_Category)%>%summarise(tSodium = mean(Sodium))

pso1 <- plot_ly(
  x = Sodium_content$tSodium,
  y = Sodium_content$Food_Category,
  marker = list(color = new_col),
  type = "bar"
)

Sodium_content<-subset_nutrition%>% select(restaurant,Food_Category,Sodium)%>% group_by(restaurant)%>%summarise(tSodium = mean(Sodium))

high_Sodium <- Sodium_content%>%arrange(desc(tSodium))%>%head(26)

pso2<-plot_ly(x= high_Sodium$restaurant,y=high_Sodium$tSodium,
             color = high_Sodium$restaurant,type="bar")%>%layout(title="Top High Sodium Restaurants",height=400)


low_Sodium <- Sodium_content%>%arrange(desc(tSodium))%>%tail(26)

pso3<-plot_ly(x= low_Sodium$restaurant,y=low_Sodium$tSodium,
              color = low_Sodium$restaurant,type="bar")%>%layout(title="Top low Sodium Restaurants",height=400)





### Sugar
sugar_content<-subset_nutrition%>% select(restaurant,Food_Category,Sugar)%>% group_by(Food_Category)%>%summarise(tsugar = mean(Sugar))

psu1 <- plot_ly(
  x = sugar_content$tsugar,
  y = sugar_content$Food_Category,
  marker = list(color = new_col),
  type = "bar"
)

sugar_content<-subset_nutrition%>% select(restaurant,Food_Category,Sugar)%>% group_by(restaurant)%>%summarise(tsugar = mean(Sugar))

high_sugar <- sugar_content%>%arrange(desc(tsugar))%>%head(26)

psu2<-plot_ly(x= high_sugar$restaurant,y=high_sugar$tsugar,
             color = high_sugar$restaurant,type="bar")%>%layout(title="Top High Sugar Restaurants",height=400)


low_sugar <- sugar_content%>%arrange(desc(tsugar))%>%tail(26)

psu3<-plot_ly(x= low_sugar$restaurant,y=low_sugar$tsugar,
              color = low_sugar$restaurant,type="bar")%>%layout(title="Top low Sugar Restaurants",height=400)

### Dietary_Fiber
Dietary_Fiber_content<-subset_nutrition%>% select(restaurant,Food_Category,Dietary_Fiber)%>% group_by(Food_Category)%>%summarise(tDietary_Fiber = mean(Dietary_Fiber))

pdi1 <- plot_ly(
  x = Dietary_Fiber_content$tDietary_Fiber,
  y = Dietary_Fiber_content$Food_Category,
  marker = list(color = new_col),
  type = "bar"
)

Dietary_Fiber_content<-subset_nutrition%>% select(restaurant,Food_Category,Dietary_Fiber)%>% group_by(restaurant)%>%summarise(tDietary_Fiber = mean(Dietary_Fiber))

high_Dietary_Fiber <- Dietary_Fiber_content%>%arrange(desc(tDietary_Fiber))%>%head(26)

pdi2<-plot_ly(x= high_Dietary_Fiber$restaurant,y=high_Dietary_Fiber$tDietary_Fiber,
             color = high_Dietary_Fiber$restaurant,type="bar")%>%layout(title="Top High Dietary_Fiber Restaurants",height=400)


low_Dietary_Fiber <- Dietary_Fiber_content%>%arrange(desc(tDietary_Fiber))%>%tail(26)

pdi3<-plot_ly(x= low_Dietary_Fiber$restaurant,y=low_Dietary_Fiber$tDietary_Fiber,
              color = low_Dietary_Fiber$restaurant,type="bar")%>%layout(title="Top low Dietary_Fiber Restaurants",height=400)

shinyServer(function(input, output,session) {
### map tab

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




### comparison tab
  #resx_name
  res_name<-function(k){
    return(input$restaurants[[k]])
  }

  #resx_raw
  res_raw<-function(k){
    return(
      data_comparison%>%
        filter(restaurant == res_name(k) )%>%
        filter(Food_Category%in%input$category_check)
    )
  }

  #resx_arranged
  res_arranged<-function(k){
    return(
      if(input$arrange1!= "Select"){
        if(input$arrange2!= "Select"){
          if(input$arrange3!= "Select"){
            if(input$desc1){
              if(input$desc2){
                if(input$desc3){
                  res_raw(k)%>%arrange(.data[[input$arrange1]],.data[[input$arrange2]],.data[[input$arrange3]])
                }
                else{
                  res_raw(k)%>%arrange(.data[[input$arrange1]],.data[[input$arrange2]],desc(.data[[input$arrange3]]))
                }
              }
              else{
                if(input$desc3){
                  res_raw(k)%>%arrange(.data[[input$arrange1]],desc(.data[[input$arrange2]]),.data[[input$arrange3]])
                }
                else{
                  res_raw(k)%>%arrange(.data[[input$arrange1]],desc(.data[[input$arrange2]]),desc(.data[[input$arrange3]]))
                }
              }
            }
            else{
              if(input$desc2){
                if(input$desc3){
                  res_raw(k)%>%arrange(desc(.data[[input$arrange1]]),.data[[input$arrange2]],.data[[input$arrange3]])
                }
                else{
                  res_raw(k)%>%arrange(desc(.data[[input$arrange1]]),.data[[input$arrange2]],desc(.data[[input$arrange3]]))
                }
              }
              else{
                if(input$desc3){
                  res_raw(k)%>%arrange(desc(.data[[input$arrange1]]),desc(.data[[input$arrange2]]),.data[[input$arrange3]])
                }
                else{
                  res_raw(k)%>%arrange(desc(.data[[input$arrange1]]),desc(.data[[input$arrange2]]),desc(.data[[input$arrange3]]))
                }
              }
            }
          }
          else{
            if(input$desc1){
              if(input$desc2){
                res_raw(k)%>%arrange(.data[[input$arrange1]],.data[[input$arrange2]])
              }
              else{
                res_raw(k)%>%arrange(.data[[input$arrange1]],desc(.data[[input$arrange2]]))
              }
            }
            else{
              if(input$desc2){
                res_raw(k)%>%arrange(desc(.data[[input$arrange1]]),.data[[input$arrange2]])
              }
              else{
                res_raw(k)%>%arrange(desc(.data[[input$arrange1]]),desc(.data[[input$arrange2]]))
              }
            }
          }
        }
        else{
          if(input$desc1){
            res_raw(k)%>%arrange(.data[[input$arrange1]])
          }
          else{
            res_raw(k)%>%arrange(desc(.data[[input$arrange1]]))
          }
        }
      }
    )
  }

  #resx_toped
  res_toped<-function(k){
    return(res_arranged(k)%>%ungroup()%>%head(input$topn)%>%mutate(menu_id=row_number()))
  }

  #pieplot

   res_pie_plot<-function(k,row_select){
    table_nu<-res_toped(k)[row_select,]

    plot<-plot_ly()

    for(i in 1:nrow(table_nu)){
      plot<-plot %>%
        add_pie(data = table_nu[i,]%>%
                  select(Item_Name,input$nutrition_show)%>%
                  pivot_longer(input$nutrition_show,"nutrition","value")%>%
                  mutate(value=replace_na(value, 0)),
                labels = ~nutrition, values = ~value,
                textposition = 'inside',
                textinfo = 'label+percent',
                name = table_nu[i,]$Item_Name,
                marker = list(colors = 'RdYlGn',
                              line = list(color = '#FFFFFF', width = 0.4)),
                title = table_nu[i,]$Item_Name,
                domain = list(row = 0, column = i-1))
    }

    plot <- plot %>%
      layout(title = "Pie Charts with Subplots", showlegend = T,
             grid=list(rows=1, columns=nrow(table_nu)),
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    return(plot)
  }



##1
  res1_name<-reactive({
    res_name(1)
  })

  output$res1_name<- renderText ({res1_name()})

  res1_arranged<-reactive({
    res_arranged(1)
  })

  res1_toped<- reactive({
    res_toped(1)
    })

  output$res1_table<- renderDataTable({
    datatable(
      data = res1_toped()%>%
        mutate(size=paste(Serving_Size,Serving_Size_Unit,sep=" ")%>%
                 str_remove_all("NA NA"))%>%
        select(Item_Name, Food_Category, size,Calories,input$nutrition_show),
      selection = 'multiple',
      rownames = FALSE,
      options = list(scrollX = TRUE,scrollY = TRUE)
    )
  })

  res1_plot<-reactive({
    req(input$res1_table_rows_selected, cancelOutput = F)
    row_id1 <- input$res1_table_rows_selected
    res_pie_plot(1,row_id1)
  })

  output$res1_plot<-renderPlotly({res1_plot()})
##2
  res2_name<-reactive({
    res_name(2)
  })

  output$res2_name<- renderText ({res2_name()})

  res2_arranged<-reactive({
    res_arranged(2)
  })

  res2_toped<- reactive({
    res_toped(2)
  })

  output$res2_table<- renderDataTable({
    datatable(
      data = res2_toped()%>%mutate(size=paste(Serving_Size,Serving_Size_Unit,sep=" ")%>%str_remove_all("NA NA"))%>%select(Item_Name, Food_Category, size,Calories,input$nutrition_show),
      selection = 'multiple',
      rownames = FALSE,
      options = list(scrollX = TRUE,scrollY = TRUE)
    )
  })

  res2_plot<-reactive({
    req(input$res2_table_rows_selected, cancelOutput = F)
    row_id2 <- input$res2_table_rows_selected
    res_pie_plot(2,row_id2)
  })

  output$res2_plot<-renderPlotly({res2_plot()})
##3
  res3_name<-reactive({
    res_name(3)
  })

  output$res3_name<- renderText ({res3_name()})

  res3_arranged<-reactive({
    res_arranged(3)
  })

  res3_toped<- reactive({
    res_toped(3)
  })

  output$res3_table<- renderDataTable({
    datatable(
      data = res3_toped()%>%mutate(size=paste(Serving_Size,Serving_Size_Unit,sep=" ")%>%str_remove_all("NA NA"))%>%
        select(Item_Name, Food_Category, size,Calories,input$nutrition_show),
      selection = 'multiple',
      rownames = FALSE,
      options = list(scrollX = TRUE,scrollY = TRUE)
    )
  })

  res3_plot<-reactive({
    req(input$res3_table_rows_selected, cancelOutput = F)
    row_id3 <- input$res3_table_rows_selected
    res_pie_plot(3,row_id3)
  })

  output$res3_plot<-renderPlotly({res3_plot()})


### statistics tab  
  
  output$pfat1 <- renderPlotly(pfat1)
  output$pfat2 <- renderPlotly(pfat2)
  output$pfat3 <- renderPlotly(pfat3)
  
  output$pcal1 <- renderPlotly(pcal1)
  output$pcal2 <- renderPlotly(pcal2)
  output$pcal3 <- renderPlotly(pcal3)
  output$ppro1 <- renderPlotly(ppro1)
  output$ppro2 <- renderPlotly(ppro2)
  output$ppro3 <- renderPlotly(ppro3)
  output$pcar1 <- renderPlotly(pcar1)
  output$pcar2 <- renderPlotly(pcar2)
  output$pcar3 <- renderPlotly(pcar3)
  output$psu1 <- renderPlotly(psu1)
  output$psu2 <- renderPlotly(psu2)
  output$psu3 <- renderPlotly(psu3)
  output$pdi1 <- renderPlotly(pdi1)
  output$pdi2 <- renderPlotly(pdi2)
  output$pdi3 <- renderPlotly(pdi3)
  output$pso1 <- renderPlotly(pso1)
  output$pso2 <- renderPlotly(pso2)
  output$pso3 <- renderPlotly(pso3)
  output$pcho1 <- renderPlotly(pcho1)
  output$pcho2 <- renderPlotly(pcho2)
  output$pcho3 <- renderPlotly(pcho3)
  
  

  output$plotgraph = renderPlot({
    g1<-ggplot(subset_nutrition, aes(x=Cholesterol,y=Calories))+geom_point(col="hotpink")+geom_smooth(method="lm",col="hotpink")
    g2<-ggplot(subset_nutrition, aes(x=Carbohydrates,y=Calories))+geom_point(col="navyblue")+geom_smooth(method="lm",col="navyblue")
    g3<-ggplot(subset_nutrition, aes(x=Sugar,y=Calories))+geom_point(col="darkorchid4")+geom_smooth(method="lm",col="darkorchid4")
    g4<-ggplot(subset_nutrition, aes(x=Total_Fat,y=Calories))+geom_point(col="magenta")+geom_smooth(method="lm",col="magenta")
    g5<-ggplot(subset_nutrition, aes(x=Sodium,y=Calories))+geom_point(col="olivedrab4")+geom_smooth(method="lm",col="olivedrab4")
    g6<-ggplot(subset_nutrition, aes(x=Protein,y=Calories))+geom_point(col="firebrick4")+geom_smooth(method="lm",col="firebrick4")
    g7<-ggplot(subset_nutrition, aes(x=Saturated_Fat,y=Calories))+geom_point(col="orange4")+geom_smooth(method="lm",col="orange4")
    g8<-ggplot(subset_nutrition, aes(x=Dietary_Fiber,y=Calories))+geom_point(col="tomato4")+geom_smooth(method="lm",col="tomato4")
    g9<-ggplot(subset_nutrition, aes(x=Trans_Fat,y=Calories))+geom_point(col="slateblue4")+geom_smooth(method="lm",col="slateblue4")
    grid.arrange(g1,g2,g3,g4,g5,g6,g7,g8,nrow=3,ncol=3)
  })  
 
### data search tab
## menu
  search_menu<-
    data_search_menu%>%
    mutate(Serving_Size=paste(Serving_Size,Serving_Size_Unit,sep=" ")%>%
             str_remove_all("NA NA"))%>%select(-Serving_Size_Unit)
  output$search_menu<- renderDataTable({
    datatable(
      data = search_menu,
      selection = 'multiple',
      filter = "top",
      rownames = FALSE,
      options = list(scrollX = TRUE,scrollY = TRUE,pageLength = 5)
    )
  })


## location

  search_location<-reactive({
    if (!is_null(input$restaurants_search_menu)) data_search_location<-data_search_location%>%filter(restaurant%in%input$restaurants_search_menu)
    if (!is_null(input$BORO_search_menu)) data_search_location<-data_search_location%>%filter(BORO%in%input$BORO_search_menu)
    if (!is_null(input$cuisine_search_menu)) data_search_location<-data_search_location%>%filter(`CUISINE DESCRIPTION`%in%input$cuisine_search_menu)
    if (!is_null(input$grade_search_menu)) data_search_location<-data_search_location%>%filter(GRADE%in%input$grade_search_menu)
    return(data_search_location)
  })


  output$search_location<- renderDataTable({
    datatable(
      data = search_location(),
      selection = 'multiple',
      rownames = FALSE,
      options = list(scrollX = TRUE,scrollY = TRUE,pageLength = 10)
    )
  })


#   
  
  
  
  
  
})


