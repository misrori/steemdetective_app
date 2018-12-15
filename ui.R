library(shiny)
library(plotly)
library(data.table)
library(DT)
library(leaflet)
library(lubridate)
library(shinyWidgets)

navbarPage(
  title="Steemblockchain explorer",theme = "journal",fluid = TRUE,
  tabPanel("trransferee",
           sidebarLayout(
             sidebarPanel(
              
               shiny::textInput('name', 'Name',value = ""),
               actionButton("button", "An action button")
               

               
               ),
             mainPanel(
              
               
               DT::dataTableOutput('alltr_df')
               
             )
               )),
  tabPanel("Előrejelzés"
  ),
  tabPanel("Térkép"
  )
           
           

           
           
  )
  

