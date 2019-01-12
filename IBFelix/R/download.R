listformbind <- function(data,save=TRUE) #,whereall=NULL,where=NULL,files=NULL,saveas=NULL,tz='EST5EDT') # do this later
{
  classx <- function(x) { class(x)[1] }
  bind <- function(z) {
    where <<- which(lapply(z, classx)!='xts')
    if(length(where)==0) print('all lists completed') # https://stackoverflow.com/questions/6451152/how-to-catch-integer0
    if(length(where)!=0) print(paste('no data in list',where)) # put this in warning vector somehow
    z[where] <- NULL
    temp2 <- do.call(rbind, z) 
    temp2 <- temp2[ ! duplicated( index(temp2) ),  ] # should not be the case
    indexTZ(temp2) <- 'EST5EDT'
    return(temp2)
  }
  
  final <- bind(data)
  # if(!is.null(where)) dat <- lapply(paste0(where,files,'.rds'), readRDS)
  # if(!is.null(whereall)) dat <- lapply(list.files(whereall), readRDS)
  
  name_extract <- gsub('.Open','',colnames(final)[1])
  # if(class(x)=='character') saveRDS(temp2, paste0(x,'.rds'))
  # if(class(x)!='character') {
  #   if(x$local=='') saveRDS(temp2, paste0(x$symbol,format(Sys.time(), '%H-%M'),'.rds'))
  #   if(x$local!='') saveRDS(temp2, paste0(x$local,'.rds'))
  # }
  if(save) saveRDS(final, paste0(name_extract,'_miau.rds'))
  return(final)
}

listbidask <- function(x,Sys.sleep=0,dates=NULL,save=NULL,type='BID') {
  if(is.null(dates)) dates <- tryCatch(finddates(x, type=type), error = function(e) return(NULL))
  if(is.null(dates)) return(NULL) # and skip
  
  # if(is.null(save)) temp2 <- lapply(dates, function(y) .ib$bidask(x, y, Sys.sleep=Sys.sleep))
  if(is.null(save)) {
    if(type=='BID') temp2 <- lapply(dates, function(y) bidask(x, y, Sys.sleep=Sys.sleep))
    if(type=='TRADES') {
      temp2 <- lapply(dates, function(y) {
      rapp <- getTrades(x, y, Sys.sleep=Sys.sleep) 
      if(is.null(rapp)) { print('Additional sleep....'); Sys.sleep(10) }
      return(rapp)
      }) }
  }
  if(!is.null(save)) temp2 <- lapply(dates, function(y) bidask(x, y, Sys.sleep=Sys.sleep, option='SAVE'))
  
  ### IF
  # classx <- function(x) { class(x)[1] }
  # where <- which(lapply(temp, classx) != 'xts')
  # print(paste('no data in list', where)) # put this in warning vector somehow
  # temp[where] <- NULL
  # temp2 <- do.call(rbind, temp)
  # temp2 <- temp2[!duplicated(index(temp2)),] # should not be the case
  # indexTZ(temp2) <- 'EST5EDT'
  ### IF
  
  if(class(x)=='character') saveRDS(temp2, paste0(x,'_listform.rds'))
  if(class(x)!='character') {
    if(x$local=='') saveRDS(temp2, paste0(x$symbol,format(Sys.time(), '%H-%M'),'_listform.rds'))
    if(x$local!='') saveRDS(temp2, paste0(x$local,'_listform.rds'))
  }
}

bidask <- function(x, # first argument = contract
                       DateTime=gsub('-','',format(Sys.Date(), "%Y-%m-%d %H:%M:%S")),
                       barSize='1 min',duration='5 D',Sys.sleep=1,option='NOSAVE') {
  if(class(DateTime)=='Date') DateTime <- gsub('-','',format(DateTime, "%Y-%m-%d %H:%M:%S"))
  if(class(x)=='character') x <- twsEquity(x)
  print(paste(x$symbol, substr(DateTime, 1, 8), format(Sys.time(), "%X"))) # x$ticker -> x$symbol
  # where is the sys.sleep?
  # print(Sys.sleep)
  a <- b <- NULL
  if(option!='NOSAVE') a <- savedl(x,DateTime,whatToShow='BID',Sys.sleep=Sys.sleep)[,c(1:4)]
  if(option=='NOSAVE') a <- reqHistoricalData(tws,Contract=x,endDateTime=DateTime,barSize=barSize,duration=duration,useRTH='0',whatToShow='BID')[,c(1:4)]
  # counter()
  Sys.sleep(Sys.sleep)
  if(option!='NOSAVE') b <- savedl(x,DateTime,whatToShow='ASK',Sys.sleep=Sys.sleep)[,c(1:4)]
  if(option=='NOSAVE') b <- reqHistoricalData(tws,Contract=x,endDateTime=DateTime,barSize=barSize,duration=duration,useRTH='0',whatToShow='ASK')[,c(1:4)]
  # counter()
  Sys.sleep(Sys.sleep)
  yy <- cbind(a,b) # works because a and b do EXIST (but maybe NULL)
  if(is.null(a)) yy <- cbind(NA,NA,NA,NA,b)
  if(is.null(b)) yy <- cbind(a,NA,NA,NA,NA)
  if(is.null(a) & is.null(b)) yy <- NULL
  return(yy) 
}

indexdl <- function(x,DateTime,Sys.sleep=2) {
  if(class(DateTime)=='Date') DateTime <- gsub('-','',format(DateTime, "%Y-%m-%d %H:%M:%S"))
  print(paste(x$ticker, substr(DateTime, 1, 8), format(Sys.time(), "%X")))
  # if(option!='NOSAVE') 
  a <- savedl(x,DateTime,whatToShow='TRADES',Sys.sleep=Sys.sleep); counter()
  # if(option=='NOSAVE') a <- reqHistoricalData(tws,Contract=x,endDateTime=DateTime,barSize=barSize,duration=duration,useRTH='0',whatToShow='TRADES')
  return(a) 
}

getTrades <- function(x,DateTime=gsub('-','',format(Sys.Date(), "%Y-%m-%d %H:%M:%S")),Sys.sleep=2) {
  if(class(DateTime)=='Date') DateTime <- gsub('-','',format(DateTime, "%Y-%m-%d %H:%M:%S"))
  if(class(x)=='character') x <- twsEquity(x)
  print(paste(x$symbol, substr(DateTime, 1, 8), format(Sys.time(), "%X"))) # x$ticker -> x$symbol
  # a <- savedl(x,DateTime,whatToShow='BID',Sys.sleep=Sys.sleep) # [,c(1:4)]
  a <- reqHistoricalData(tws, x, DateTime, '1 min', '5 D', '0', 'TRADES')
  counter()
  Sys.sleep(Sys.sleep)
  return(a) 
}

seconddl <- function(x,DateTime=gsub('-','',format(Sys.Date(), "%Y-%m-%d %H:%M:%S")),Sys.sleep=2,save='TRUE') {
  DateTime <- gsub('-','',format(DateTime, "%Y-%m-%d %H:%M:%S")) # always reformat
  print(paste(DateTime, format(Sys.time(), "%X"))) # x$ticker here incorrect
  if(is.null(save)) {
    a <- reqHistoricalData(tws, x, DateTime,whatToShow='BID',barSize='1 secs', duration='600 S')[,c(1:4)]; Sys.sleep(Sys.sleep); counter()
    b <- reqHistoricalData(tws, x, DateTime,whatToShow='ASK',barSize='1 secs', duration='600 S')[,c(1:4)]; Sys.sleep(Sys.sleep); counter()
  }
  # 
  if(!is.null(save)) {
  a <- savedl(x,DateTime,whatToShow='BID',barSize='1 secs', duration='600 S',Sys.sleep=Sys.sleep)[,c(1:4)]; counter()
  b <- savedl(x,DateTime,whatToShow='ASK',barSize='1 secs', duration='600 S',Sys.sleep=Sys.sleep)[,c(1:4)]; counter()
  }
  yy <- cbind(a,b) # works because a and b do EXIST (but maybe NULL)
  if(is.null(a)) yy <- cbind(NA,NA,NA,NA,b)
  if(is.null(b)) yy <- cbind(a,NA,NA,NA,NA)
  if(is.null(a) & is.null(b)) yy <- NULL
  return(yy) 
}

getVol <- function(x, DateTime=gsub('-','',format(Sys.Date(), "%Y-%m-%d %H:%M:%S")), barSize='1 min', duration='5 D', Sys.sleep=2) {
  if(class(DateTime)=='Date') DateTime <- gsub('-','',format(DateTime, "%Y-%m-%d %H:%M:%S"))
  # if(class(x)=='character') x <- twsEquity(x)
  print(paste(x$symbol, substr(DateTime, 1, 8), format(Sys.time(), "%X"))) # x$ticker -> x$symbol
  a <- reqHistoricalData(tws,Contract=x,endDateTime=DateTime,barSize=barSize,duration=duration, useRTH='0',whatToShow='OPTION_IMPLIED_VOLATILITY')
  counter()
  Sys.sleep(Sys.sleep)
  return(a) 
}

### OLD DO NOT USE (passing function call instead of arguments)
# .ib$bidask <- function(y,x) {
#   timexx <<- gsub('-','',format(y, "%Y-%m-%d %H:%M:%S")); print(paste(timexx,format(Sys.time(), "%X")))
#   savexx <<- paste('BA',which(dates==y))
#   a <- savedl(reqHistoricalData(tws,x,timexx,'1 min','5 D','0','BID')[,c(1:4)]); Sys.sleep(5); counter()
#   b <- savedl(reqHistoricalData(tws,x,timexx,'1 min','5 D','0','ASK')[,c(1:4)]); Sys.sleep(5); counter()
#   yy <- cbind(a,b) # works because a and b do EXIST (but maybe NULL)
#   if(is.null(a)) yy <- cbind(NA,NA,NA,NA,b)
#   if(is.null(b)) yy <- cbind(a,NA,NA,NA,NA)
#   if(is.null(a) & is.null(b)) yy <- NULL
#   return(yy) 
# }

### DAILY ----
# download_daily <- function(x,option=NULL,sys.sleep=NULL,save=NULL) {
#   # if(!exists('warn')) warn <<- NULL
#   if(class(x)=='character') x <- twsSTK(x)
#   data1 <- data2 <- data3 <- data4 <- NA
#   today <- gsub('-','',format(Sys.Date(), "%Y-%m-%d %H:%M:%S"))
#   data1 <- savedl(Contract=x, barSize='1 day', duration='15 Y', whatToShow='OPTION_IMPLIED_VOLATILITY', Sys.sleep=0)[,4]
#   data2 <- savedl(Contract=x, barSize='1 day', duration='15 Y', whatToShow='HISTORICAL_VOLATILITY', Sys.sleep=0)[,4]
#   data3 <- savedl(Contract=x, barSize='1 day', duration='15 Y', whatToShow='MIDPOINT')[,c(1:4)] # MIDPOINT / TRADES
#   data4 <- savedl(Contract=x, barSize='1 day', duration='15 Y', whatToShow='TRADES')[,c(1:4)] # MIDPOINT / 
#   try(colnames(data1) <- paste0(x$symbol, '.Implied'), silent = TRUE)
#   try(colnames(data2) <- paste0(x$symbol, '.Historical'), silent = TRUE)
#   try(colnames(data4) <- paste0(colnames(data4), '.T'), silent = TRUE)
#   data <- cbind(data1,data2,data3,data4)
#   index(data) <- as.Date(index(data), tz='Europe/Berlin')
#   return(data)
# }

download_daily_vol <- function(x,option=NULL,sys.sleep=NULL,save=NULL) {
  # if(!exists('warn')) warn <<- NULL
  if(class(x)=='character') x <- twsSTK(x)
  today <- gsub('-','',format(Sys.Date(), "%Y-%m-%d %H:%M:%S"))
  data1 <- savedl(Contract=x, barSize='1 day', duration='15 Y', whatToShow='OPTION_IMPLIED_VOLATILITY', Sys.sleep=0)[,4]
  data2 <- savedl(Contract=x, barSize='1 day', duration='15 Y', whatToShow='HISTORICAL_VOLATILITY', Sys.sleep=0)[,4]
  try(colnames(data1) <- paste0(x$symbol, '.Implied'), silent = TRUE)
  try(colnames(data2) <- paste0(x$symbol, '.Historical'), silent = TRUE)
  if(is.null(data1)) data1 <- NA
  if(is.null(data2)) data2 <- NA
  data <- cbind(data1,data2)
  try(index(data) <- as.Date(index(data), tz='Europe/Berlin'))
  return(data)
}

### LIST FORM ----
requestbla = function() {  
  x <- twsSTK('AAPL')
  today <- gsub('-','',format(Sys.Date(), "%Y-%m-%d %H:%M:%S"))
  reqHistoricalData(tws,x,today,'1 day','15 Y','0','MIDPOINT')
}

download_daily = function(x,sys.sleep=NULL,save=NULL) {
  if(class(x)=='character') x <- twsSTK(x)
  today <- gsub('-','',format(Sys.Date(), "%Y-%m-%d %H:%M:%S"))
  OPTION_IMPLIED_VOLATILITY <- try(reqHistoricalData(tws,x,today,'1 day','15 Y','0','OPTION_IMPLIED_VOLATILITY')); counter()
  HISTORICAL_VOLATILITY <- try(reqHistoricalData(tws,x,today,'1 day','15 Y','0','HISTORICAL_VOLATILITY')); counter()
  MIDPOINT <- try(reqHistoricalData(tws,x,today,'1 day','15 Y','0','MIDPOINT')); counter()
  TRADES <- try(reqHistoricalData(tws,x,today,'1 day','15 Y','0','TRADES')); counter()
  if(!is.null(sys.sleep)) { Sys.sleep(sys.sleep) }
  data <- list('OPTION_IMPLIED_VOLATILITY'=OPTION_IMPLIED_VOLATILITY,'HISTORICAL_VOLATILITY'=HISTORICAL_VOLATILITY,'MIDPOINT'=MIDPOINT,'TRADES'=TRADES)
  if(!is.null(save) & exists('data')) saveRDS(data, paste(x$symbol, '.rds', sep='')) # ?
  return(data)
}

