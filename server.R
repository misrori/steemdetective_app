
library(shiny)
library(shinyWidgets)

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
  
  my_reactive_all_data <- eventReactive(input$button, {
    

    
      osszes <- list()
      init_szamlalo <- 1000
      szamlalo <- 1000
      limit<- 1000
      last_tr <- get_all_tr_number(input$name)$all_tr_number
      print(paste('I have found ', last_tr, 'transactions'))
      vege_szamlalo <- round(last_tr/limit, 0)
      
      
      if(init_szamlalo>=last_tr){
        r = POST('http://127.0.0.1:54321/get_all_tr', body = list("name" = input$name, 'to'=init_szamlalo, 'limit'=limit), encode = 'json')
        adat <- fromJSON( content(r, 'text' , encoding = "ISO-8859-1"))
        osszes <- append(osszes, adat)
        
        
      }else{
        my_seq <- seq(szamlalo, last_tr, limit)
        
        withProgress(message = 'Downloading data',
                     detail = paste0('0/',last_tr) , value = 0, {
        for (i in my_seq) {
          incProgress(1/vege_szamlalo, detail = paste0(szamlalo,'/',last_tr))
          print(i)
          r = POST('http://127.0.0.1:54321/get_all_tr', body = list("name" = input$name, 'to'=szamlalo, 'limit'=limit), encode = 'json')
          adat <- fromJSON(content(r, 'text' , encoding = "ISO-8859-1"))
          adat[length(adat)]<- NULL
          osszes <- append(osszes, adat)
          szamlalo <- szamlalo+limit
          
          
        }
      })
                       
                       
        r = POST('http://127.0.0.1:54321/get_all_tr', body = list("name" = input$name, 'to'=last_tr, 'limit'=last_tr- my_seq[length(my_seq)]), encode = 'json')
        adat <- fromJSON( content(r, 'text' , encoding = "ISO-8859-1"))
        osszes <- append(osszes, adat)
      }


      return(osszes)
      
  })
      
      

    
  
    
  my_reactive_all_tr_dt <- reactive({
    if(input$name==''){
      return(data.table())
    }else{

    return(get_all_tr_df(my_reactive_all_data(), input$name))
    }
  })


  output$alltr_df<- DT::renderDataTable(
    return(my_reactive_all_tr_dt())
  )

  
  

  
})
