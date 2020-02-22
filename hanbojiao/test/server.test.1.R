#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(datasets)

source("global.R")



shinyServer(function(input, output,session) {
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
    if(input$arrange1!="NA"){ 
      if(input$arrange2!="NA"){
        if(input$arrange3!="NA"){
          result <- res_raw(k)[order(unlist(res_raw(k)[,input$arrange1]),
                                     unlist(res_raw(k)[,input$arrange2]), 
                                     unlist(res_raw(k)[,input$arrange3]), 
                                     decreasing = c(input$desc1, input$desc2, input$desc3), method = "radix"),]
        }
      }  
    }     
    return(result)
  }
    
  #resx_toped
  res_toped<-function(k){
    return(res_arranged(k)%>%ungroup()%>%head(input$topn)%>%mutate(menu_id=row_number()))
  }
  
  #pieplot
  menuid<-reactive({
    list(input$menuid1,input$menuid2,input$menuid3)
  })
  cp <- coord_polar(theta = "y")
  cp$is_free <- function() TRUE
  # res_pie_plot<-function(k){
  #   res_toped(k)%>%
  #     filter(menu_id%in%menuid()[[k]])%>%
  #     pivot_longer(Calories:Dietary_Fiber,"nutrition","value")%>%
  #     mutate(value=replace_na(value, 0))%>%
  #     ggplot(aes(x=factor(1),y=value  ,fill=factor(nutrition)))+
  #     facet_wrap(~menu_id,scales = "free")+
  #     geom_bar(stat = "identity", width=1)+
  #     cp+
  #     theme_void()
  # }

  res_pie_plot<-function(k,row_select){
    plot_raw<-res_toped(k)%>%select(menu_id,Item_Name:Serving_Size_Unit,input$nutrition_show)%>%select(-Item_Description)
    plot<-plot_raw[row_select,]%>%select(Item_Name,input$nutrition_show)%>%
      pivot_longer(input$nutrition_show,"nutrition","value")%>%
      mutate(value=replace_na(value, 0))%>%
      ggplot(aes(x=factor(1),y=value  ,fill=nutrition))+
      facet_wrap(~Item_Name,scales = "free")+
      geom_bar(stat = "identity", width=1)+
      cp+
      theme_void()
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
    validate(
      need(input$restaurants != "NA",""),
      need(input$arrange1 != "Select",""),
      need(input$arrange2 != "Select",""),
      need(input$arrange3 != "Select","")
    )
    datatable(
      res1_toped()%>%select(menu_id,Item_Name:Serving_Size_Unit,input$nutrition_show)%>%select(-Item_Description), 
      selection = 'multiple')
  })
  
  res1_plot<-reactive({
    req(input$res1_table_rows_selected, cancelOutput = F)
    row_id1 <- input$res1_table_rows_selected
    res_pie_plot(1,row_id1)
  })

  output$res1_plot<-renderPlot({res1_plot()})
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
    validate(
      need(input$restaurants != "NA",""),
      need(input$arrange1 != "Select",""),
      need(input$arrange2 != "Select",""),
      need(input$arrange3 != "Select","")
    )
    datatable(
      data = res2_toped()%>%select(menu_id,Item_Name:Serving_Size_Unit,input$nutrition_show)%>%select(-Item_Description),
      selection = 'multiple'
    )
  })
  
  res2_plot<-reactive({
    req(input$res2_table_rows_selected, cancelOutput = F)
    row_id2 <- input$res2_table_rows_selected
    res_pie_plot(2,row_id2)
  })
  
  output$res2_plot<-renderPlot({res2_plot()})
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
    validate(
      need(input$restaurants != "NA",""),
      need(input$arrange1 != "Select",""),
      need(input$arrange2 != "Select",""),
      need(input$arrange3 != "Select","")
    )
    datatable(
      data = res3_toped()%>%select(menu_id,Item_Name:Serving_Size_Unit,input$nutrition_show)%>%select(-Item_Description),
      selection = 'multiple'
    )
  })
  
  res3_plot<-reactive({
    req(input$res3_table_rows_selected, cancelOutput = F)
    row_id3 <- input$res3_table_rows_selected
    res_pie_plot(3,row_id3)
  })
  
  output$res3_plot<-renderPlot({res3_plot()})
  

 
 

})