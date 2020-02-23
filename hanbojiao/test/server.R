source("global.R")

shinyServer(function(input, output,session) {
  
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

  cp <- coord_polar(theta = "y")
  cp$is_free <- function() TRUE
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
        select(Item_Name, Food_Category, size,input$nutrition_show),
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
      data = res2_toped()%>%mutate(size=paste(Serving_Size,Serving_Size_Unit,sep=" ")%>%str_remove_all("NA NA"))%>%select(Item_Name, Food_Category, size,input$nutrition_show),
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
        select(Item_Name, Food_Category, size,input$nutrition_show),
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
  
  
  
  
  
  
  
  
})


