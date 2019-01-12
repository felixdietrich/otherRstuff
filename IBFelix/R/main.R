savedl <- function(Contract, endDateTime=gsub('-','',format(Sys.Date(), "%Y-%m-%d %H:%M:%S")), 
                       barSize='1 min', duration='5 D', useRTH='0', whatToShow='TRADES', Sys.sleep=0) {
  # reqHistoricalData
  success <- FALSE
  while (!success) {
    success <- TRUE 
    xx <- tryCatch(reqHistoricalData(tws, Contract, endDateTime, barSize, duration, useRTH, whatToShow), 
                   warning = function(w) { 
                     print(paste(substr(endDateTime, 1, 8), w$message))
                     IBFelix.pkg.env$warn <- append(IBFelix.pkg.env$warn, paste(substr(endDateTime, 1, 8), whatToShow, w$message))
                     if(grepl('Connectivity',w$message)) { 
                       success <<- FALSE; Sys.sleep(20); Sys.sleep(20); tws <<- ibgConnect() } # twsDisconnect(tws); 
                     if(grepl('No security definition',w$message)) { 
                       success <<- FALSE; Sys.sleep(20); Sys.sleep(20); tws <<- ibgConnect() }
                     if(grepl('tws connection timed-out',w$message)) { 
                       success <<- FALSE; Sys.sleep(20); Sys.sleep(20); tws <<- ibgConnect() }
                     # if(grepl('Historical Market Data Service',w$message)) {  }
                     if(grepl('Error processing request',w$message)) { 
                       success <<- FALSE; Sys.sleep(20); Sys.sleep(20); tws <<- ibgConnect() }
                     Sys.sleep(1)
                     #}, error = function(e) { print('fritz'); success <<- FALSE; Sys.sleep(20); twsDisconnect(tws); Sys.sleep(20); tws <- ibgConnect() })
                   })
    Sys.sleep(Sys.sleep)
    gc()
  }
  return(xx)
}

### THIS IS NA (not duplicates) # from supermerge.R
nacheck = function(z) {
  # zz <- paste0(substr(z,1,3),'.',substr(z,4,6))
  contr <- all(z, 'nodetails')
  a <- get(z) # attention needs same timezone
  a1 <- which.na.xts(a)
  # das braucht man hier gar nicht als lapply...
  data <- lapply(list(contr), function(x) {
    dates <- as.Date(unlist(lapply(unique(as.Date(index(a1))), function(x) x+c(1:7)))) # how to unlist and keep dates?
    dates <- unique( dates[weekdays(dates) %in% 'Sunday'] ); print(dates)
    temp <- lapply(dates, function(y) bidask2nosave(x, y, Sys.sleep=0))
    classx <- function(x) { class(x)[1] }
    where <- which(lapply(temp, classx)!='xts')
    print(paste('no data in list',where)) # put this in warning vector somehow
    temp[where] <- NULL
    temp2 <- do.call(rbind, temp) 
    temp2 <- temp2[ ! duplicated( index(temp2) ),  ] # should not be the case
    indexTZ(temp2) <- 'EST5EDT'
    gc()
    Sys.sleep(5)
    return(temp2)
  })
  data <- data[[1]]
  a2 <- data[index(data) %in% index(a1)]
  print(paste('N/A VALUES ARE FROM API:', all.equal(a1,a2, check.attributes = FALSE)))
  return(data) 
}

nacombine = function(old,new,return='newdata') {  ### ODER DATAONE??? hier only fuer NAs machen
  na1 <- which.na.xts(new) # nrow ist hier hoeher
  na2 <- new[index(old) %in% index(which.na.xts(new))] ### DER CODE IST FALSCH
  # zumindest wenn new nur ein paar teile sind, und old full
  print('New data is superior',all.equal(na1,na2))
  
  dat <- rbind(old,new)
  navalues <- rbind(which.na.xts(old),new[index(new) %in% index(which.na.xts(old))])
  na1 <- which.na.xts(old) # nrow ist hier hoeher
  na2 <- new[index(new) %in% index(which.na.xts(old))]
  na1[is.na(na1)] <- 88
  na2[is.na(na2)] <- 88
  na3 <- na1!=na2
  na3 <- xts(rowSums(na3), index(na3))
  navalues_t <- navalues[index(navalues) %in% index(na3[na3>0])]
  # allduplicates <- navalues[ index(navalues) %in% index(navalues[duplicated( index(navalues) )]) ]
  # dat <- dat[ ! duplicated( index(dat) ), ] 
  dat <- dat[ ! duplicated( index(dat), fromLast = 'TRUE' ), ] # removes first entry, i.e. old, see:
  # test <- cbind(duplicated( index(dat) ),duplicated( index(dat), fromLast = 'TRUE' ))
  # which(rowSums(test)>0)
  # head(test[62180:63000,], 20)
  # completely new values:
  # new[!index(new) %in% index(old)]
  if(return=='newdata') return(dat)
  return(navalues_t)
}

updatedata = function(z) {
  zz <- paste0(substr(z,1,3),'.',substr(z,4,6))
  contr <- all(zz, 'nodetails')
  a <- readRDS(z)
  data <- lapply(list(contr), function(x) {
    dates <- finddates(x)
    dates <- dates[as.Date(index(last(a)))<dates]
    # temp <- lapply(dates, function(y) bidask2(x, y, Sys.sleep=0))
    temp <- lapply(dates, function(y) bidask2nosave(x, y, Sys.sleep=0))
    classx <- function(x) { class(x)[1] }
    where <- which(lapply(temp, classx)!='xts')
    print(paste('no data in list',where)) # put this in warning vector somehow
    temp[where] <- NULL
    temp2 <- do.call(rbind, temp) 
    temp2 <- temp2[ ! duplicated( index(temp2) ),  ] # should not be the case
    indexTZ(temp2) <- 'EST5EDT'
    if(class(x)=='character') saveRDS(temp2, paste0(x,'.rds'))
    if(class(x)!='character') saveRDS(temp2, paste0(x$local,'.rds'))
    gc()
    Sys.sleep(5)
  })
}

