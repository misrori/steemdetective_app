library(httr)
library(jsonlite)
library(data.table)
library(dplyr)
library(rvest)
library(stringi)

get_all_tr_number <- function(name) {
  r = POST('http://127.0.0.1:54321/get_all_tr_number', body = list("name" = name), encode = 'json')
  adat <- fromJSON( content(r, 'text'  ,encoding = "ISO-8859-1"))
  return(adat)
}



name<- 'misrori'
get_all_tr <- function(name) {
  
  
  osszes <- list()
  init_szamlalo <- 1000
  szamlalo <- 1000
  limit<- 1000
  last_tr <- get_all_tr_number(name)$all_tr_number
  print(paste('I have found ', last_tr, 'transactions'))
  
  if(init_szamlalo>=last_tr){
    r = POST('http://127.0.0.1:54321/get_all_tr', body = list("name" = name, 'to'=init_szamlalo, 'limit'=limit), encode = 'json')
    adat <- fromJSON( content(r, 'text' , encoding = "ISO-8859-1"))
    osszes <- append(osszes, adat)
    return(osszes)
    
  }
  my_seq <- seq(szamlalo, last_tr, limit)
  
  for (i in my_seq) {
    print(i)
    r = POST('http://127.0.0.1:54321/get_all_tr', body = list("name" = name, 'to'=szamlalo, 'limit'=limit), encode = 'json')

    adat <- fromJSON(content(r, 'text' , encoding = "ISO-8859-1"))
    adat[length(adat)]<- NULL
    adat
    osszes <- append(osszes, adat)
    szamlalo <- szamlalo+limit
  }
  
  r = POST('http://127.0.0.1:54321/get_all_tr', body = list("name" = name, 'to'=last_tr, 'limit'=last_tr- my_seq[length(my_seq)]), encode = 'json')
  adat <- fromJSON( content(r, 'text' , encoding = "ISO-8859-1"))
  osszes <- append(osszes, adat)
  
  return(osszes)
  
  
}


osszes <- get_all_tr('admin')


