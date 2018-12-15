library(httr)
library(jsonlite)
library(data.table)
library(dplyr)
library(rvest)




get_all_tr <- function(name) {
  
  osszes <- list()
  name <- 'admin'
  my_to <- 10
  my_limit <- 10
  last_tr <- get_all_tr_number(name)$all_tr_number
  
  my_seq <- seq(my_to, last_tr, my_limit)
  for (i in my_seq) {
    print(i)
    r = POST('http://127.0.0.1:54321/get_all_tr', body = list("name" = name, 'to'= my_to, 'limit'= my_limit), encode = 'json')
    adat <- fromJSON( content(r, 'text' , encoding = "ISO-8859-1"))
    adat[length(adat)] <- NULL
    osszes <- append(osszes, adat)
    
    my_to <- my_to +my_limit
    
      
  }
  
  
  r = POST('http://127.0.0.1:54321/get_all_tr', body = list("name" = name, 'to'= last_tr, 'limit'= last_tr- my_seq[length(my_seq)]-1), encode = 'json')
  adat <- fromJSON( content(r, 'text' , encoding = "ISO-8859-1"))
  osszes <- append(osszes, adat)
  
  return(adat)
}





get_all_tr_number <- function(name) {
  r = POST('http://127.0.0.1:54321/get_all_tr_number', body = list("name" = name), encode = 'json')
  adat <- fromJSON( content(r, 'text'  ,encoding = "ISO-8859-1"))
  return(adat)
}
