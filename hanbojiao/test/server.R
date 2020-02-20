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

  output$res1 <- renderTable({
    restaurant1<-data_comparison%>%
      filter(restaurant == input$restaurants[[1]])%>%
      filter(Food_Category%in%input$category_check)

    if(input$arrange1!="NA"){
      if(input$arrange2!="NA"){
        if(input$arrange3!="NA"){
          if(input$desc1){
            if(input$desc2){
              if(input$desc3){
                restaurant1<-restaurant1%>%arrange(.data[[input$arrange1]],.data[[input$arrange2]],.data[[input$arrange3]])
              }
              else{
                restaurant1<-restaurant1%>%arrange(.data[[input$arrange1]],.data[[input$arrange2]],desc(.data[[input$arrange3]]))
              }
            }
            else{
              if(input$desc3){
                restaurant1<-restaurant1%>%arrange(.data[[input$arrange1]],desc(.data[[input$arrange2]]),.data[[input$arrange3]])
              }
              else{
                restaurant1<-restaurant1%>%arrange(.data[[input$arrange1]],desc(.data[[input$arrange2]]),desc(.data[[input$arrange3]]))
              }
            }
          }
          else{
            if(input$desc2){
              if(input$desc3){
                restaurant1<-restaurant1%>%arrange(desc(.data[[input$arrange1]]),.data[[input$arrange2]],.data[[input$arrange3]])
              }
              else{
                restaurant1<-restaurant1%>%arrange(desc(.data[[input$arrange1]]),.data[[input$arrange2]],desc(.data[[input$arrange3]]))
              }
            }
            else{
              if(input$desc3){
                restaurant1<-restaurant1%>%arrange(desc(.data[[input$arrange1]]),desc(.data[[input$arrange2]]),.data[[input$arrange3]])
              }
              else{
                restaurant1<-restaurant1%>%arrange(desc(.data[[input$arrange1]]),desc(.data[[input$arrange2]]),desc(.data[[input$arrange3]]))
              }
            }
          }
        }
        else{
          if(input$desc1){
            if(input$desc2){
              restaurant1<-restaurant1%>%arrange(.data[[input$arrange1]],.data[[input$arrange2]])
            }
            else{
              restaurant1<-restaurant1%>%arrange(.data[[input$arrange1]],desc(.data[[input$arrange2]]))
            }
          }
          else{
            if(input$desc2){
              restaurant1<-restaurant1%>%arrange(desc(.data[[input$arrange1]]),.data[[input$arrange2]])
            }
            else{
              restaurant1<-restaurant1%>%arrange(desc(.data[[input$arrange1]]),desc(.data[[input$arrange2]]))
            }
          }
        }
      }
      else{
        if(input$desc1){
          restaurant1<-restaurant1%>%arrange(.data[[input$arrange1]])
          }
        else{
          restaurant1<-restaurant1%>%arrange(desc(.data[[input$arrange1]]))
        }
      }
    }






    restaurant1%>%head(10)%>%select(-Item_Description)
  })

  output$res2 <- renderTable({
    restaurant2<-data_comparison%>%
      filter(restaurant == input$restaurants[[2]])%>%
      filter(Food_Category%in%input$category_check)

    if(input$arrange1!="NA"){
      if(input$arrange2!="NA"){
        if(input$arrange3!="NA"){
          if(input$desc1){
            if(input$desc2){
              if(input$desc3){
                restaurant2<-restaurant2%>%arrange(.data[[input$arrange1]],.data[[input$arrange2]],.data[[input$arrange3]])
              }
              else{
                restaurant2<-restaurant2%>%arrange(.data[[input$arrange1]],.data[[input$arrange2]],desc(.data[[input$arrange3]]))
              }
            }
            else{
              if(input$desc3){
                restaurant2<-restaurant2%>%arrange(.data[[input$arrange1]],desc(.data[[input$arrange2]]),.data[[input$arrange3]])
              }
              else{
                restaurant2<-restaurant2%>%arrange(.data[[input$arrange1]],desc(.data[[input$arrange2]]),desc(.data[[input$arrange3]]))
              }
            }
          }
          else{
            if(input$desc2){
              if(input$desc3){
                restaurant2<-restaurant2%>%arrange(desc(.data[[input$arrange1]]),.data[[input$arrange2]],.data[[input$arrange3]])
              }
              else{
                restaurant2<-restaurant2%>%arrange(desc(.data[[input$arrange1]]),.data[[input$arrange2]],desc(.data[[input$arrange3]]))
              }
            }
            else{
              if(input$desc3){
                restaurant2<-restaurant2%>%arrange(desc(.data[[input$arrange1]]),desc(.data[[input$arrange2]]),.data[[input$arrange3]])
              }
              else{
                restaurant2<-restaurant2%>%arrange(desc(.data[[input$arrange1]]),desc(.data[[input$arrange2]]),desc(.data[[input$arrange3]]))
              }
            }
          }
        }
        else{
          if(input$desc1){
            if(input$desc2){
              restaurant2<-restaurant2%>%arrange(.data[[input$arrange1]],.data[[input$arrange2]])
            }
            else{
              restaurant2<-restaurant2%>%arrange(.data[[input$arrange1]],desc(.data[[input$arrange2]]))
            }
          }
          else{
            if(input$desc2){
              restaurant2<-restaurant2%>%arrange(desc(.data[[input$arrange1]]),.data[[input$arrange2]])
            }
            else{
              restaurant2<-restaurant2%>%arrange(desc(.data[[input$arrange1]]),desc(.data[[input$arrange2]]))
            }
          }
        }
      }
      else{
        if(input$desc1){
          restaurant2<-restaurant2%>%arrange(.data[[input$arrange1]])
        }
        else{
          restaurant2<-restaurant2%>%arrange(desc(.data[[input$arrange1]]))
        }
      }
    }






    restaurant2%>%head(10)%>%select(-Item_Description)
  })

  output$res3 <- renderTable({
    restaurant3<-data_comparison%>%
      filter(restaurant == input$restaurants[[3]])%>%
      filter(Food_Category%in%input$category_check)

    if(input$arrange1!="NA"){
      if(input$arrange2!="NA"){
        if(input$arrange3!="NA"){
          if(input$desc1){
            if(input$desc2){
              if(input$desc3){
                restaurant3<-restaurant3%>%arrange(.data[[input$arrange1]],.data[[input$arrange2]],.data[[input$arrange3]])
              }
              else{
                restaurant3<-restaurant3%>%arrange(.data[[input$arrange1]],.data[[input$arrange2]],desc(.data[[input$arrange3]]))
              }
            }
            else{
              if(input$desc3){
                restaurant3<-restaurant3%>%arrange(.data[[input$arrange1]],desc(.data[[input$arrange2]]),.data[[input$arrange3]])
              }
              else{
                restaurant3<-restaurant3%>%arrange(.data[[input$arrange1]],desc(.data[[input$arrange2]]),desc(.data[[input$arrange3]]))
              }
            }
          }
          else{
            if(input$desc2){
              if(input$desc3){
                restaurant3<-restaurant3%>%arrange(desc(.data[[input$arrange1]]),.data[[input$arrange2]],.data[[input$arrange3]])
              }
              else{
                restaurant3<-restaurant3%>%arrange(desc(.data[[input$arrange1]]),.data[[input$arrange2]],desc(.data[[input$arrange3]]))
              }
            }
            else{
              if(input$desc3){
                restaurant3<-restaurant3%>%arrange(desc(.data[[input$arrange1]]),desc(.data[[input$arrange2]]),.data[[input$arrange3]])
              }
              else{
                restaurant3<-restaurant3%>%arrange(desc(.data[[input$arrange1]]),desc(.data[[input$arrange2]]),desc(.data[[input$arrange3]]))
              }
            }
          }
        }
        else{
          if(input$desc1){
            if(input$desc2){
              restaurant3<-restaurant3%>%arrange(.data[[input$arrange1]],.data[[input$arrange2]])
            }
            else{
              restaurant3<-restaurant3%>%arrange(.data[[input$arrange1]],desc(.data[[input$arrange2]]))
            }
          }
          else{
            if(input$desc2){
              restaurant3<-restaurant3%>%arrange(desc(.data[[input$arrange1]]),.data[[input$arrange2]])
            }
            else{
              restaurant3<-restaurant3%>%arrange(desc(.data[[input$arrange1]]),desc(.data[[input$arrange2]]))
            }
          }
        }
      }
      else{
        if(input$desc1){
          restaurant3<-restaurant3%>%arrange(.data[[input$arrange1]])
        }
        else{
          restaurant3<-restaurant3%>%arrange(desc(.data[[input$arrange1]]))
        }
      }
    }






    restaurant3%>%head(10)%>%select(-Item_Description)
  })


  
  
  

  
 

})