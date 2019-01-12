IBFelix.pkg.env <- new.env()
IBFelix.pkg.env$warn <- NULL
# append(IBFelix.pkg.env$warn, 1)

finddates <- function(x, type='BID') { 
  if(class(x)=='character') x <- twsEquity(x)
  today <- gsub('-','',format(Sys.Date(), "%Y-%m-%d %H:%M:%S"))
  if(type=='BID') dates <- as.Date(index(reqHistoricalData(tws,x,today,'1 week','15 Y','0','BID')))
  if(type=='OPT') dates <- as.Date(index(reqHistoricalData(tws,x,today,'1 week','15 Y','0','OPTION_IMPLIED_VOLATILITY')))
  if(type=='TRADES') dates <- as.Date(index(reqHistoricalData(tws,x,today,'1 week','15 Y','0','TRADES'))) # FUNKTIONIERT NICHT RICHTIG BEI SPY z.B.
  
  if(Sys.Date()-last(dates)>7) { # IF NOT UNTIL TODAY
    print(last(dates))
    dates <- seq(dates[1],last(dates)+7,by=1) # in case of missing data points, make consistent series
  }
  if(Sys.Date()-last(dates)<7) {
    dates <- seq(dates[1],Sys.Date(),by=1) # in case of missing data points, make consistent series
  }
  dates[weekdays(dates) %in% 'Sunday']
}

counter <- function() { 
  # print(environment())
  # test <- environment()
  # print(parent.env(test)) # <environment: namespace:IBFelix>
  
  # door.env <- new.env()
  # test2 <- 123
  # assign('test2', test)
  # test2 <<- 1234
  # assign('test2', 123, envir = pkg.env)
  # pkg.env$fritz <- 12352
  # The semantics of these operations are those of get(i, env=x, inherits=FALSE).
  # https://stackoverflow.com/questions/10276941/r-environment-lookup
  if(!exists('count', envir = IBFelix.pkg.env)) { 
    IBFelix.pkg.env$count <- 0; IBFelix.pkg.env$j <- Sys.time() 
    }
  IBFelix.pkg.env$count <- IBFelix.pkg.env$count+1; print(IBFelix.pkg.env$count) # Making more than 60 requests within any ten minute period.
  if(IBFelix.pkg.env$count==60) { print(Sys.time()-IBFelix.pkg.env$j); IBFelix.pkg.env$count <- 0; IBFelix.pkg.env$j <- Sys.time() } 
}
