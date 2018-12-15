library(httr)
library(jsonlite)
library(data.table)
library(dplyr)
library(rvest)


acc_name <- "misrori"
setwd('/home/mihaly/R_codes/steemdetective_app/')

download_steem_price <- function(){
  end_date<- gsub('-', '',Sys.Date())
  #steem
  my_link <- paste0('https://coinmarketcap.com/currencies/steem/historical-data/?start=20130428&end=', end_date)
  t<- read_html(my_link)
  tablak <- 
    t%>%
    html_table()
  adat <- data.table(tablak[[1]])
  adat <-  adat[,c(1:2), with= F]
  names(adat)<- c('Date', 'steem_price')
  
  adat$Date <-gsub( 'Jan', '01',adat$Date)
  adat$Date <-gsub( 'Feb', '02',adat$Date)
  adat$Date <-gsub( 'Mar', '03',adat$Date)
  adat$Date <-gsub( 'Apr', '04',adat$Date)
  adat$Date <-gsub( 'May', '05',adat$Date)
  adat$Date <-gsub( 'Jun', '06',adat$Date)
  adat$Date <-gsub( 'Jul', '07',adat$Date)
  adat$Date <-gsub( 'Aug', '08',adat$Date)
  adat$Date <-gsub( 'Sep', '09',adat$Date)
  adat$Date <-gsub( 'Oct', '10',adat$Date)
  adat$Date <-gsub( 'Nov', '11',adat$Date)
  adat$Date <-gsub( 'Dec', '12',adat$Date)
  adat$Date<- as.Date(adat$Date, format= '%m %d, %Y')
  steem <- adat
  
  
  
  ###sbd
  my_link <- paste0('https://coinmarketcap.com/currencies/steem-dollars/historical-data/?start=20130428&end=', end_date)
  t<- read_html(my_link)
  tablak <- 
    t%>%
    html_table()
  adat <- data.table(tablak[[1]])
  adat <-  adat[,c(1:2), with= F]
  names(adat)<- c('Date', 'sbd_price')
  
  adat$Date <-gsub( 'Jan', '01',adat$Date)
  adat$Date <-gsub( 'Feb', '02',adat$Date)
  adat$Date <-gsub( 'Mar', '03',adat$Date)
  adat$Date <-gsub( 'Apr', '04',adat$Date)
  adat$Date <-gsub( 'May', '05',adat$Date)
  adat$Date <-gsub( 'Jun', '06',adat$Date)
  adat$Date <-gsub( 'Jul', '07',adat$Date)
  adat$Date <-gsub( 'Aug', '08',adat$Date)
  adat$Date <-gsub( 'Sep', '09',adat$Date)
  adat$Date <-gsub( 'Oct', '10',adat$Date)
  adat$Date <-gsub( 'Nov', '11',adat$Date)
  adat$Date <-gsub( 'Dec', '12',adat$Date)
  adat$Date<- as.Date(adat$Date, format= '%m %d, %Y')
  sbd <- adat
  
  osszes <- merge(steem, sbd, by='Date', all.x = T)
  osszes[is.na(sbd_price)]$sbd_price<- 1
  
  last <- data.table('Date'=Sys.Date(), 'steem_price'= tail(osszes, 1)$steem_price, 'sbd_price'=tail(osszes, 1)$sbd_price)
  osszes <- rbind(osszes, last)
  
  write.csv(osszes, 'steem_price.csv', row.names = FALSE)

  
  
}
download_steem_price()



get_all_tr_number <- function(name) {
  r = POST('http://127.0.0.1:54321/get_all_tr_number', body = list("name" = name), encode = 'json')
  adat <- fromJSON( content(r, 'text'  ,encoding = "ISO-8859-1"))
  return(adat)
}

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



all_tr<- get_all_tr(acc_name)



get_all_tr_df <- function(all_tr, name) {
  get_tr_lapply <- function(x) {
    tryCatch({
      if(x[[1]][[2]]$op[[1]]=='transfer'){
        return(x)
      }
    }, error = function(e) {
      NULL
    })
    
    
  }
  
  all_filtered_tr <- NULL
  
  for(i in c(1:length(all_tr))){
     t<- get_tr_lapply(all_tr[i])
     if(is.null(t)==FALSE){
       #print(t)
       
      k<-  data.frame('amount'=t[[1]][[2]]$op[[2]]$amount,
                      'from'= t[[1]][[2]]$op[[2]]$from, 
                      'memo' = t[[1]][[2]]$op[[2]]$memo,
                      'to' = t[[1]][[2]]$op[[2]]$to,
                      'timestamp' = t[[1]][[2]]$timestamp, stringsAsFactors = F)
       
       all_filtered_tr[[i]]=k
       
     }
  }
  eredmeny<- rbindlist(all_filtered_tr)
  eredmeny$currency_amount <- as.numeric(lapply(strsplit(as.character(eredmeny$amount), ' '),'[[', 1))
  eredmeny$currency_type <- as.character(lapply(strsplit(as.character(eredmeny$amount), ' '),'[[', 2))
  eredmeny$Date<- substring(eredmeny$timestamp, 1,10)
  setkey(eredmeny, 'Date')
  
  prices <- data.table(read.csv('steem_price.csv', stringsAsFactors = F))
  setkey(prices, 'Date')
  #prices$Date <- as.Date(prices$Date)
  
  vege<-prices[eredmeny]
  #vege[is.na(price)]$price<-  tail(vege[is.na(price)==F], 1)$price
  vege$direction <- ifelse(vege$from==name, 'out', 'in')
  
  vege$dollar_price_of_tr <- ifelse(vege$currency_type=='STEEM', round(as.numeric(vege$currency_amount)*as.numeric(vege$steem_price), 2), 
                                    round(as.numeric(vege$sbd_price)*as.numeric(vege$currency_amount), 2))
  
  kibeszalok <- c('bittrex', 'blocktrades', 'deepcrypto8')
  vege$tr_type <-'normal_tr'
  vege[direction=="in" & from%in% kibeszalok, ]$tr_type <-'investment' 
  vege[direction=="out" & to%in% kibeszalok, ]$tr_type <-'cash_out'
  
  
  
  Encoding(vege$memo) <- "latin1"  # (just to make sure)

  vege$memo <- iconv(vege$memo, "latin1", "ASCII", sub="")
  
 
  return(data.table(vege))
}


vege<- get_all_tr_df(all_tr, acc_name)










#rm(all_tr)

#save(all_tr, file="blocktrades_all.RData")

#load('blocktrades_all.RData')

# 
# 
# vege[, .('sum_value_dollar'= sum(dollar_price_of_tr, na.rm = T)), by=tr_type]
# 
# 
# 
# 
# vege$dollar_price_of_tr <- ifelse(vege$currency_type=='STEEM', round(as.numeric(vege$currency_amount)*as.numeric(vege$steem_price), 2), 
#                                   round(as.numeric(vege$sbd_price)*as.numeric(vege$currency_amount), 2))
# 
# 
# write.csv(vege)
# sum(vege$dollar_price_of_tr)
# 
