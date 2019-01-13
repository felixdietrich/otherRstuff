# https://stackoverflow.com/questions/26118622/r-user-defined-functions-in-new-environment
# require(xts)

# attach(.hE,name="helper",pos=-1)
# detach(.hE,name="helper")
# source('~/Dropbox/newdiss/git/functions.R')
# source('~/Dropbox/newdiss/git/functions_plot.R')
# print(c('Data','Holidays','Outliers','DOWN-IB','OutliersOLD'))
# loadFunctions <- function(x)
# {
  ### Data ----
  # if(sum(x %in% 'Data')>=1) {

# FUNKTIONIERT NICHT AUS FUNKTION HERAUS
clearall <- function() {
  # other = search()[!search() %in% c('.GlobalEnv',paste0('package:', sessionInfo()$basePkgs))]
  # other # macht dann in RStudio problems
  # lapply(other, detach, character.only = TRUE, unload = TRUE) ### FOR ALL OTHER PACKAGES
  # pkgs = paste0('package:', names(sessionInfo()$otherPkgs))
  # lapply(pkgs, detach, character.only = TRUE, unload = TRUE) ### FOR ALL OTHER PACKAGES
  
  print(ls(all.names = TRUE, pos=1))
#   for (i in c(3:length(search()))) { detach(2) }
  rm(list=ls(all.names = TRUE, pos=1), pos=1)
}

readzip <- function(link) {
  data <- NULL
  temp <- tempfile()
  download.file(link,temp)
  check <- unzip(temp)
  if(substr(check, nchar(check)-3+1, nchar(check)) %in% c('txt','csv'))
  {
    data <- read.csv(check)
  }
  unlink(temp); unlink(check); return(data)
}

headtail <- function(x,n=5) {
  print(head(x,n))
  print(tail(x,n))
}

Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

bidask <- function(x,type='perc') { 
  if(type=='perc') return( (x[,8]-x[,4]) / ((x[,8]+x[,4])/2) )
  if(type!='perc') return( (x[,8]-x[,4]) )
}

midpoint <- function(x) { (x[,4]+x[,8])/2 }

wherestart <- function(x, mode='full') { # full: uses the first value incl. NA's / na: the first complete 
  if(ncol(x)==1) {
    print(first(x[!is.na(x)])); print(last(x[!is.na(x)]))
    return(paste0(index(first(x[!is.na(x)])),'/',index(last(x[!is.na(x)])))) 
  }
  if(ncol(x)!=1) {
    if(mode=='full') { print(first(x[rowSums(x, na.rm=T)!=0])); print(last(x[rowSums(x, na.rm=T)!=0])) 
      return(paste0(index(first(x[rowSums(x, na.rm=T)!=0])),'/',index(last(x[rowSums(x, na.rm=T)!=0])))) }
    if(mode=='na') { print(first(x[rowSums(x)!=0])); print(last(x[rowSums(x)!=0])) 
      return(paste0(index(first(x[rowSums(x)!=0])),'/',index(last(x[rowSums(x)!=0])))) }
  }
}

duplicatedindex <- function(x,y='show') { 
  if(y=='show') return(x[ duplicated( index(x) ),  ])
  if(y=='both') { return(rbind(x[ duplicated( index(x) ),  ], x[ duplicated( index(x), fromLast = TRUE ),  ])) }
  if(y=='remove') return(x[ ! duplicated( index(x) ),  ])
}

duplicatecombine = function(x,y=NULL,keep='last') {
  dat <- x
  if(!is.null(y)) dat <- rbind(x,y)
  if(keep=='last') dat <- dat[ ! duplicated( index(dat), fromLast = 'TRUE' ), ]
  if(keep=='first') dat <- dat[ ! duplicated( index(dat) ), ]
  return(dat)
}

findmissingvalues <- function(z,j) { ### THIS ALSO FOR INTRADAY 
  subset(apply.daily(z, nrow), apply.daily(z, nrow)!=j)
}

findgapsweekly <- function(a) {
  # fcompare(unique(as.Date(index(a), tz=indexTZ(a))), removeSundays(seq(as.Date(index(first(a)), tz=indexTZ(a)),Sys.Date(),1), andSat = TRUE))
  dates_want <- removeSundays(seq(as.Date(index(first(a)), tz=indexTZ(a)),Sys.Date(),1), andSat = TRUE)
  dates_have <- unique(as.Date(index(a), tz=indexTZ(a)))
  data <- xts(rep(1, length(dates_want)), dates_want)
  dat <- split(data, 'weeks')
  ret <- dat[unlist(lapply(dat, function(x) sum(index(x) %in% dates_have)==0))]
  if(length(ret)==0) { return() }
  return(ret)
}

findgaps <- function(x,y='intraday') { 
  if(y=='daily') { 
      first_date <- as.Date(index(first(x)), tz=indexTZ(x))
      last_date <- as.Date(index(last(x)), tz=indexTZ(x))
      dates <- seq(first_date,last_date,1)
      dates_want <<- dates[!weekdays(dates) %in% c('Saturday','Sunday')]
      dates_have <<- unique(as.Date(index(x), tz=indexTZ(x)))
      ret <- dates[!dates_want %in% dates_have] ### check here isholremoved package timedate
      return(ret)
    }
  if(y=='weekly') { 
    gapsat=index(x[ which( diff(index(x))>1 ) +2 ]) # nochmal checken, old code
    # gapdates=as.Date(gapsat, tz=indexTZ(x))
    gapdates=as.Date(gapsat)
    print(gapdates)
    where=gapdates[which(diff(gapdates)<1)] # wo gibt es gaps die nicht weekend sind
    return(where) }
  if(y=='intraday') { 
    gapsat=index(x[ which( diff(index(x))>1 ) +2 ]) # nochmal checken, old code
    # gapdates=as.Date(gapsat, tz=indexTZ(x))
    gapdates=as.Date(gapsat)
    print(gapdates)
    where=gapdates[which(diff(gapdates)<1)] # wo gibt es gaps die nicht weekend sind
    return(where) }
  if(y=='weekendcurrencies') { 
    temp=as.list(which( diff(index(mid))>16 ))
    # temp=as.character(as.Date(index(x[ which( diff(index(x))>16 ) ])))
    # print(lapply(temp, function(y) tail(x[y])))
    print(lapply(temp, function(y) x[(y-3):(y+3)]))
    return(index(x[ which( diff(index(x))>16 ) ])) }
  if(y=='highfreq') { return(index(x[ which( diff(index(x))>1 ) ])) }
  if(y=='highfreq2') { 
    gap <- x[ diff(index(x))>1 ] 
    gap[!index(gap) %in% index(apply.daily(x, last))]
    }
  # weekly:
  # dates_have=dates_have[4:length(dates_have)] # remove the first non-consistent
  # dates_have
  # dates_want=dates_have[1]+seq(from=0,to=length(dates_have),by=1)*7
  # lapply(g, na.omit)
  # then rbind
  # then duplicates remove
}

whatisdifferent = function(old,new,return='numbers') {
  x1 <- old[!index(old) %in% index(new)]
  x2 <- new[!index(new) %in% index(old)]
  x3 <- duplicatedindex(rbind(old,new))
  # nrow(x1)+nrow(x3) # =nrow(old)
  # nrow(x2)+nrow(x3) # =nrow(new)
  if(return=='numbers') return(paste('Old Values:',nrow(x1),' New Values:',nrow(x2),' Same Values:',nrow(x3)))
  if(return=='old') return(x1)
  if(return=='new') return(x2)
}

wheredifferent <- function(x,y=NULL,frequency='minutes') {
  if(!is.null(y)) print(whatisdifferent(x,y))
  if(!is.null(y)) x <- rbind(x,y)
  xx <- split(x, frequency)
  xx <- xx[lapply(xx, function(x) nrow(x))==2] ### ENSURE ... later improve here
  xxx <<- lapply(xx, function(x) identical(x[1,],x[2,])==FALSE)
  if(length(unique(xxx))==1 & unique(xxx)[1]==FALSE) return('Within same indices: All Identical')
  return(xx[unlist(xxx)])
}

fcompare <- function(x,y,option='different') {
  if(class(x)[1]=='xts') {
    temp1 <- index(x)[!index(x) %in% index(y)]
    temp2 <- index(y)[!index(y) %in% index(x)]
    if(option=='different') return(list('values in x but not in y'=x[temp1], # if(option=='same') return(unique(x[temp1],y[temp2]))
                                        'values in y but not in x'=y[temp2]))
  }
  if(class(x)[1]!='xts') {
    temp1 <- x %in% y
    temp2 <- y %in% x
    # if(FALSE %in% unique(temp1)) { print(x[!temp1]) } # print()
    # if(FALSE %in% unique(temp2)) { print(y[!temp2]) }  # print()
    # if(option=='different') return(list(x[!temp1],y[!temp2]))
    if(option=='different') return(c(x[!temp1],y[!temp2]))
    if(option=='same') return(unique(x[temp1],y[temp2]))
  }
}

containsamevalues <- function(x,y=NULL,exceptNA=NULL,printNA=NULL,return='') {
  dat <- x
  if(!is.null(y)) {
    print(paste('Duplicates x (alone):',anyDuplicated(index(x))))
    print(paste('Duplicates y (alone):',anyDuplicated(index(y))))
    if(!is.null(printNA)) { print(which.na.xts(x)); print(which.na.xts(y)) }
    # print(x[duplicated( index(x) )])
    # print(y[duplicated( index(y) )])
    dat <- rbind(x,y)
  }
  if(!is.null(exceptNA)) dat <- na.omit(dat)
  ### NOCH WAS EINBAUEN FALLS ES KEINE DUPLICATES GIBT
  f1 <-  duplicated( index(dat) ) # this is Y
  f2 <-  duplicated( index(dat), fromLast = 'TRUE' ) # this is X
  test1 <- dat[f1]; test2 <- dat[f2]
  print(paste0(nrow(test1),'/',nrow(test2),' duplicates equal ',all.equal(test1,test2)))
  if(return=='binded') return(rbind(test2,test1))
  if(return=='listed') return(list(test2,test1))
  # mergedat <- dat[ ! duplicated( index(dat) ),  ]
}

dataone <- function(x=NULL,whereall=NULL,where=NULL,files=NULL,saveas=NULL,tz='EST5EDT') # dat muss list sein
{
  dat <- x
  if(!is.null(where)) dat <- lapply(paste0(where,files,'.rds'), readRDS)
  if(!is.null(whereall)) dat <- lapply(list.files(whereall), readRDS)
  # print(head(dat))
  print(do.call(rbind, lapply(dat, colnames)))
  # dat <- lapply(dat, na.omit)
  dat <- do.call(rbind, dat[ unlist(lapply(dat, function(x) nrow(x)!=0))] ) 
  # rbind but remove those completely without values (to keep colnames) (wenn z.B. die daten im woechentlichen list format sind)
  # brauch man z.b. bei second-data in lists?
  if(max(table(index(dat)))==1) { 
    print('Duplicated rows: 0'); return(dat) } # ACHTUNG HIER FUNKTIONIERT DANN SAVEAS NICHT
  if(max(table(index(dat)))>=2) {
    print(paste0('Duplicated rows max: ',max(table(index(dat)))))
    f1 = duplicated( index(dat) ) 
    f2 = duplicated( index(dat), fromLast = 'TRUE' ) 
    test1=dat[f1]; test2=dat[f2]; print(paste('Duplicates Equality',all.equal(test1,test2)))
    # find all duplicated data
    # man muesste die duplicates anhand rowSums identifizieren
    # print(nrow(dat))
    allduplicates <- dat[ index(dat) %in% index(dat[duplicated( index(dat) )]) ]
    # print(nrow(allduplicates))
    woduplicates <- dat[ ! index(dat) %in% index(dat[duplicated( index(dat) )]) ]
    # print(nrow(woduplicates))
    duplicates_wona <- na.omit(allduplicates) # remove first the NA's (maybe the duplicate has correct values)
    containsamevalues(duplicates_wona) # check if difference (after NA removal)
    print(wheredifferent(duplicates_wona)) # AUSKLAMMERN
    duplicates_wona <- duplicates_wona[ ! duplicated( index(duplicates_wona) ),  ] # then remove duplicates, nimmt den 2. (neuen)
    
    if(all.equal(unique(index(allduplicates)),unique(index(duplicates_wona)), check.attributes=FALSE)==FALSE) print('some observations completely removed')
    # mergedat <- dat[ ! duplicated( index(dat) ),  ]
    mergedat <- rbind(woduplicates, duplicates_wona) }
  ### HIER NOCH EINBAUEN: print duplicates wie bei wheredifferent
  if(!is.null(saveas)) { 
    indexTZ(mergedat) <- tz
    saveRDS(mergedat, saveas); return() }
  return(mergedat)
}
# dataone('~/',c('MMPamin.rds','MMPbmin.rds','MMPcmin.rds','MMPdmin.rds'),'MMPaminnew.rds')

# twowayplot = function(x,y) 
# { 
#   tempdata=cbind(x,y)
#   name1=gsub('.Open','',colnames(x)[1]); name2=gsub('.Open','',colnames(y)[1])
#   plot(as.zoo(tempdata[,1]), las=1, xlab="", ylab='', main=paste(name1,'vs.',name2)) #mtext('AUD/JPY')
#   par(new=TRUE)      
#   plot(as.zoo(tempdata[,2]), col=3, bty='n', xaxt="n", yaxt="n", xlab="", ylab="")
#   axis(side = 4)
#   legend('topleft', c(name1,name2), lty=c('solid','solid'), lwd=2, col=c('black','green'), cex=0.8) # inset=c(-0.4,0)
# }

normalize <- function(x,y=1) { 
  if(ncol(x)==1) return(x*as.numeric(100/x[1])) # first(x)
  if(ncol(x)>1) { 
    temp <- apply(x, 2, function(x) x/as.numeric(first(x))*y)
    ifelse(class(x)[1]=='xts',return(xts(temp, index(x))),return(temp)) }
}

cumsumna <- function(x, type='sum') { 
  if(type=='sum') { x$cum <- cumsum(ifelse(is.na(x), 0, x)); return(x$cum) }
  if(type=='prod') { x$cum <- cumprod(ifelse(is.na(x), 1, x+1)); return(x$cum) }
  if(type=='exp') { x$cum <- cumprod(ifelse(is.na(x), 1, x)); return(x$cum) }
}
cumsumna2 <- function(x) { cumsum(na.omit(x)) }

compare_against_normal <- function(x,at=0.90,method='single',inputsd=FALSE) {
  mean_s <- mean(x, na.rm=T)
  sd_s <- sd(x, na.rm=T)
  if(inputsd) {
    mean_s <- 0
    sd_s <- inputsd
  }
  q_s_u <- qnorm(at, mean = mean_s, sd = sd_s)
  q_s_d <- qnorm((1-at), mean = mean_s, sd = sd_s)
  if(method=='both') return(mean(c( length(x[x>q_s_u])/length(x), length(x[x<q_s_d])/length(x) )))
  c( length(x[x>q_s_u])/length(x), length(x[x<q_s_d])/length(x) )
}

make_front <- function(x,y=1,option='correct') {
  if(y==1) return( xts(apply(coredata(x), 1, function(row) { row[!is.na(row)][1] }), order.by = index(x)) )
  if(y>1) {
    if(option=='correct') {
      temp <- xts(t(apply(coredata(x), 1, function(row) { 
        row[which(!is.na(row))[1]:(which(!is.na(row))[1]+(y-1))] })), index(x)) # falls observation missing?
      colnames(temp) <- c(1:y)
      return(temp) }
    if(option=='mix') {
      temp <- xts(t(apply(coredata(x), 1, function(row) { row[!is.na(row)][1:y] })), index(x))
      colnames(temp) <- c(1:y)
      return(temp) }
    }
}

splitquantile <- function(x,y=2,n=5) {
  # split(x, cut( x[,y], quantile(x[,y], prob = seq(0, 1, length = n+1)) ))
  split(x, cut( x[,y], quantile(na.omit(x[,y]), prob = seq(0, 1, length = n+1)) )) # CHECK THIS
}

make_pairs <- function(data) {
  combs <- combn(ncol(data), 2, simplify = FALSE)
  ret <- lapply(combs, function(x) {
    data[,c(x[1],x[2])]
  })
  names(ret) <- lapply(ret, function(x) paste0(colnames(x)[1],'/',colnames(x)[2]))
  return(ret)
}

indexmin <- function(x,y,z) { x[.indexhour(x) %in% y & .indexmin(x) %in% z] } # gibt dasselbe

which.max.xts <- function(data) {
  ncol=seq(1:ncol(data))
  # lapply(ncol, function(x) data[which.max(data[,x]),x])
  lapply(ncol, function(x) data[data[,x] %in% sort(coredata(data[,x]), decreasing=TRUE)[1:5]])
}

which.min.xts <- function(data) {
  ncol=seq(1:ncol(data))
  # lapply(ncol, function(x) data[which.min(data[,x]),x])
  lapply(ncol, function(x) data[data[,x] %in% sort(coredata(data[,x]))[1:5]])
}
which.na.xts <- function(data, full=NULL) {
  # ncol=seq(1:ncol(data))
  # lapply(ncol, function(x) data[which.min(data[,x]),x])
  if(!is.null(full)) return(data[rowSums(data, na.rm=T)==0])
  data[is.na(rowSums(data))]
}
which.na.xts2 <- function(data) {
  do.call(rbind, lapply(c(1:ncol(data)), function(x) data[is.na(data[,x])]))
}
which.inf.xts <- function(x, what='show') { # TO DO # funktioniert nur fuer ncol = 1?
  if(what=='show') return( x[abs(x)==Inf] )
  if(what=='remove') return( x[!abs(x)==Inf] )
  #return(x)
}
which.any.xts <- function(data, y=0, method='combined') {
  if(method=='combined') {
    data[apply(data, 1, function(x) any(x %in% y)),]
    # as.xts(unlist(apply(final, 1, function(x) any(x %in% 0))), dateFormat="Date") # nicht unbedingt notwendig
  }
  if(method=='split') {
    splitted <- as.list(data)
    temp <- lapply(splitted, function(x) x[x %in% y])
    return( temp[unlist(lapply(temp, function(x) nrow(x)!=0))] )
  }
  if(method=='splitaround') {
    splitted <- as.list(data)
    test <- lapply(splitted, function(x) which(x %in% y))
    test2 <- lapply(test, function(contract) unique(unlist(lapply(contract, function(x) x+seq(-3,3,1)))))
    test3 <- lapply(names(splitted), function(x) { splitted[[x]][test2[[x]],] })
    names(test3) <- names(splitted)
    return( test3[unlist(lapply(test3, function(x) nrow(x)!=0))] )
  }
}

newxts <- function(y) {
  xts(as.numeric(y), as.Date(1:nrow(y)))
}

clean_date <- function(x)
{
  index(x) <- as.Date(index(x), tz=indexTZ(x))
  return(x)
}

snap <- function(yyy) # mit getClose function zusammenlegen! wo hatte ich das benutzt?
{
  tempX=yyy['T16:59/T17:00:01'] # at 16:59
  tempY=cbind(xts(tempX[,4], order.by=as.Date(index(tempX), 'EST5EDT')),
              xts(tempX[,8], order.by=as.Date(index(tempX), 'EST5EDT')))
  tempZ=tempY[,2]-tempY[,1]
  return(tempZ)
}
snap2 <- function(yyy)
{
  tempX=yyy['T16:59/T17:00:01'] # at 16:59
  tempY=cbind(xts(tempX[,4], order.by=as.Date(index(tempX), 'EST5EDT')),
              xts(tempX[,8], order.by=as.Date(index(tempX), 'EST5EDT')))
  tempZ=(tempY[,2]-tempY[,1])/((tempY[,2]+tempY[,1])/2)
  return(tempZ)
}

getOHLC <- function(z,close='') { # ,what=NULL
  indexTZ(z) <- 'EST5EDT'
  data <- z
  data <- (data[,c(1:4)]+data[,c(5:8)])/2
  if(close=='Real') data <- z['T09:30/T16:00']
  if(close=='1MinBefore') data <- z['T09:31/T15:59']
  # paste0(gsub('.Open','',colnames(EURCHF)[1]),c('.Open','.High','.Low','.Close'))
  data <- to.daily(data, drop.time = FALSE) ### drop.time = FALSE
  index(data) <- as.Date(index(data), tz=indexTZ(data))
  colnames(data) <- colnames(z)[1:4]
  return(data)
}

getClose <- function(z,what=NULL,plot=NULL) {      
  indexTZ(z) <- 'EST5EDT'
  lala <- gsub(".Open", "", colnames(z)[1])
  z$Mid <- (z[,8]+z[,4])/2
  close1 <- z$Mid['T16:00/T16:00:01']
  close2 <- z$Mid['T15:59/T15:59:01']
  index(close1) <- as.Date(index(close1), tz='EST5EDT') # nicht notwendig, aber...
  index(close2) <- as.Date(index(close2))
  from <- index(first(close1))
  to <- index(last(close1))
  close <- cbind(close1,close2)
  colnames(close) <- c('RealClose','1MinBefore')
  if(!is.null(plot)) {
    require(quantmod)
    xx <- getSymbols(plot, auto.assign=F, from=from, to=to, src='google')
    close <- cbind(close1,close2,xx[,4])
    colnames(close) <- c('RealClose','1MinBefore','Quantmod') }
  # print(head(close))
  if(!is.null(plot)) {
    if(what=='RealClose') plot(close[,'RealClose'], main=plot)
    if(what=='1MinBefore') plot(close[,'1MinBefore'], main=plot)
    try(lines(xx[,6], col='red'), silent=TRUE)
    try(lines(xx[,4], col='green'), silent=TRUE)
  }
  if(!is.null(what)) return(close)
  colnames(close1) <- lala
  return(close1)
}

### OF A GIVEN YEAR (to be used in IB for instance)
getSundays <- function(year) {      
  dates <- seq(as.Date(paste0(year,"-01-01")),as.Date(paste0(year,"-12-31")), by = "day")
  dates[weekdays(dates) == "Sunday"]      
}

removeSundays <- function(z,andSat=NULL) {      
  if(class(z)[1]!='Date') {
    z <- z[!weekdays(as.Date(index(z), tz=indexTZ(z))) %in% 'Sunday']
    if(!is.null(andSat)) z <- z[!weekdays(as.Date(index(z), tz=indexTZ(z))) %in% 'Saturday']
    return(z) }
  if(class(z)[1]=='Date') {
    z <- z[!weekdays(z) %in% 'Sunday']
    if(!is.null(andSat)) z <- z[!weekdays(z) %in% 'Saturday']
    return(z) }
}

day_subset <- function(z,zz,type=NULL) {      
  if(is.null(type)) z <- z[weekdays(as.Date(index(z), tz=indexTZ(z))) %in% zz]
  if(!is.null(type)) z <- z[!weekdays(as.Date(index(z), tz=indexTZ(z))) %in% zz] # REMOVE
  return(z)
}

f_vol_daily <- function(data, return='sd') {      
  if(ncol(data)!=8) { print('Error: needs 8 columns high-frequency data'); return(NULL) }
  data_daily <- apply.daily(midpoint(data), median)
  index(data_daily) <- as.Date(index(data_daily), tz=indexTZ(data_daily))
  sd <- sd( diff(log(removeSundays(data_daily))), na.rm = T )
  TTR <- TTR::volatility(removeSundays(data_daily), calc='close') # ergibt rolling XTS
  if(return=='dailysd') return(sd)
  if(return=='sd') return(sd*sqrt(252))
  if(return=='TTR') return(mean(TTR, na.rm = T))
}

f_vol_summary <- function(data,n=10,dc=FALSE) {
  if(frequency(data)!=1)
  {
    if(ncol(data)!=8) { print('Error: needs 8 columns high-frequency data'); return(NULL) }
    ### first estimate full sample sd of returns
    if(dc==TRUE) {
      vol1 <- removeSundays(as.xts(do.call(rbind, directionalchanges_eachday(data)))) # returned CEST/CET durch as.xts, je nach Sys.timezone()
      # asd <- do.call(rbind, directionalchanges_eachday(data))
      # vol1 <- xts(asd, as.Date(rownames(asd))) # this would also work without posixct
      index(vol1) <- as.Date(format(index(vol1), '%Y-%m-%d')) # convert from class posixct to date (automatically UTC)
    }
    vol2 <- removeSundays(rv(data)) # rv_calc richtig? 
    temp <- removeSundays(getOHLC(data)) ### IF NOT.. EINBAUEN
  }
  if(frequency(data)==1) { 
    if(ncol(data)!=5) { print('Error: needs 5 columns daily data (incl. diff(log(x)))'); return(NULL) } 
    vol2 <- removeSundays(rv_calc(data[,5]))
    temp <- removeSundays(data[,c(1:4)])
  }
  vol3 <- TTR::volatility(temp, calc='close')
  vol4 <- TTR::volatility(temp, calc='garman')
  vol5 <- apply.daily(temp, function(x) diff(range(x)))
  vol6 <- rollapply(temp, n, function(x) diff(range(x)), by.column = FALSE)
  if(dc==TRUE) {
    vol <- cbind(vol1,vol2,vol3,vol4,vol5,vol6,temp[,4])
    colnames(vol) <- c('DC','RV','CC','GK','R_d','R_n','Close')
  }
  if(dc==FALSE) {
    vol <- cbind(vol2,vol3,vol4,vol5,vol6,temp[,4])
    colnames(vol) <- c('RV','CC','GK','R_d','R_n','Close')
  }
  return(vol)
}

smoothxts <- function(x) { xts(smooth.spline(as.timeSeries(x))$y, order.by = index(x)) }

plothourly <- function(z,sunday='yes',outliers=NULL,save='no') # braucht als z bid-ask argument 
{
  Sys.setenv(TZ=indexTZ(z))
  print(Sys.timezone())
  asd <- seq(0:23)
  if(sunday=='no') z <- z[!weekdays(as.Date(index(z), tz='EST5EDT')) %in% 'Sunday']
  if(!is.null(outliers)) z <- z[!z %in% sort(as.numeric(z), decreasing=T)[1:outliers]]
  test <- lapply(asd, function(x) z[.indexhour(z) %in% x])
  tobox <- do.call(cbind, lapply(test, function(x) as.numeric(x)))
  boxplot(tobox, col='magenta', border='lightgrey', main=substr(colnames(z),1,6)) # names=names, 
  if(save=='yes') {
    pdf(paste(substr(colnames(z),1,6),'plot_hourly.pdf',sep=''), width=14, height = 7)
    boxplot(tobox, col='magenta', border='lightgrey', main=substr(colnames(z),1,6)) # names=names, 
    mtext(paste(as.Date(index(first(z)), tz='EST5EDT'),'-',as.Date(index(last(z)), tz='EST5EDT')))
    dev.off() }
}

whichhours <- function(x) {
  # usfullhours <- paste0(rep(8:19, each=60),':',sprintf("%02d", seq(0,59,by=1)))
  usfullhours <- apply(cbind(rep(8:19, each=60),seq(0,59,by=1)), 1, as.list)
  names(usfullhours) <- paste0(rep(8:19, each=60),':',sprintf("%02d", seq(0,59,by=1)))
  unique(paste0(.indexhour(x),':',sprintf("%02d", .indexmin(x))))
}

letters_only <- function(x) { 
  res <- !grepl("[^A-Za-z]", x) # str_subset(xxx[,1], "[:alpha:]") aus stringr check! # http://stringr.tidyverse.org/
  x[res]
}

checkcorrecttime <- function(z,critical=NULL,year='2016',sunday='yes',save='no',outliers=NULL) # braucht als z bid-ask argument 
# .GlobalEnv$checkcorrecttime <- function(z,critical=NULL,year='2016',sunday='yes',save='no',outliers=NULL) # braucht als z bid-ask argument 
{
  # plotdata = function(x) { plot(x, main=index(first(x))); Sys.sleep(5) }
  ### OLD WAY OF SUBSETTING
  # subset <- paste('T',format(as.POSIXct('2000-01-01 8:00', tz='')+60*seq(5,565,by=5), '%H:%M'),'/T',
  #                     format(as.POSIXct('2000-01-01 8:00', tz='')+60*seq(5,565,by=5)+10, '%H:%M:%S'),sep='')
  subset <- paste('T',format(as.POSIXct('2000-01-01 0:00', tz='')+60*seq(5,1435,by=5), '%H:%M'),'/T',
                  format(as.POSIXct('2000-01-01 0:00', tz='')+60*seq(5,1435,by=5)+10, '%H:%M:%S'),sep='')
  subset <- subset[!subset %in% c("T17:00/T17:00:10","T17:05/T17:05:10","T17:10/T17:10:10","T17:15/T17:15:10")]
  # make hourly indicator from 08:05 to 17:25
  
  ### NEW WAY
  Sys.setenv(TZ=indexTZ(z))
  print(Sys.timezone())
  # asd <- apply(cbind(rep(0:23, each=12),seq(0,55,by=5)), 1, as.list) # sonst wird 0:00 wird purem datum
  asd <- apply(cbind(rep(0:23, each=12),seq(1,56,by=5)), 1, as.list)
  # now return character
  names <- apply(cbind(rep(0:23, each=12),seq(1,56,by=5)), 1, function(x) paste(x[1],':',sprintf("%02d", x[2]),sep=''))
  
  # plot TZ critical dates
  if(!is.null(critical)) {
    if(year=='2016') datex <- readRDS('~/Dropbox/data/critical_timezones_dates2016.rds')
    if(year=='2014') datex <- readRDS('~/Dropbox/data/critical_timezones_dates2014.rds')
    data <- z[as.character(datex)]; data2 <- split(data, 'days')
    if(ncol(z)==1) lapply(data2, function(x) plot.zoo(x, main=index(first(x))))
    if(ncol(z)==2) lapply(data2, function(x) { plot.zoo(x[,1], main=index(first(x))); lines(as.zoo(x[,2]), col='red') })
  }
  # doublecheck z[1725,] as.Date(index(z), tz='EST5EDT')[1725] # das braucht man OBWOHL Sys.timezone korrekt ist
  
  if(sunday=='no') z <- z[!weekdays(as.Date(index(z), tz='EST5EDT')) %in% 'Sunday']
  if(!is.null(outliers)) z <- z[!z %in% sort(as.numeric(z), decreasing=T)[1:outliers]]
  
  if(is.null(critical)) {
    # test <- lapply(subset, function(x) z[x])
    test <- lapply(asd, function(x) z[.indexhour(z) %in% x[[1]] & .indexmin(z) %in% x[[2]]])
    names(test) <- names
    # print(test['17:01']); print(test['17:06']); print(test['17:11'])
    # test['17:01'] <- 0; test['17:06'] <- 0; test['17:11'] <- 0
    tobox <- do.call(cbind, lapply(test, function(x) as.numeric(x)))
    # names <- unlist(lapply(test, function(x) unique(format(index(x), '%H:%M')))) # OLD
    # boxplot(tobox, col='magenta', border='lightgrey', names=names, main=substr(colnames(z),1,6)) 
    if(save=='yes') {
      pdf(paste(substr(colnames(z),1,6),'plotx.pdf',sep=''), width=14, height = 7)
      boxplot(tobox, col='magenta', border='lightgrey', names=names, main=substr(colnames(z),1,6)) 
      mtext(paste(as.Date(index(first(z)), tz='EST5EDT'),'-',as.Date(index(last(z)), tz='EST5EDT')))
      dev.off()
    }
    return(tobox)
  }
}

orrel = function(x,y) {
  sapply(c('kendall','spearman','pearson'), function(z) cor(x, y, method=z))
}

f_lm = function(formula, data=NULL, newey='', standardized=FALSE) {
  lm.beta <- function(object) { # https://github.com/cran/lm.beta/blob/master/R/lm.beta.R
    if(!"lm"%in%attr(object,"class")) stop("object has to be of class lm")
    object$standardized.coefficients <- coef(object)*apply(as.matrix(model.matrix(object)),2,function(x) sqrt(sum((x-mean(x,na.rm=T)*attr(attr(object$model,"terms"),"intercept"))^2,na.rm=T)))/apply(as.matrix(model.frame(object)[,1]),2,function(x) sqrt(sum((x-mean(x,na.rm=T)*attr(attr(object$model,"terms"),"intercept"))^2,na.rm=T)))
    attr(object,"class") <- c("lm.beta","lm")
    return(object)
  }
  x <- lm(as.formula(formula), data = data) # If not found in data, the variables are taken from environment(formula)
  temp.summ <- summary(x)
  if(newey=='newey') temp.summ$coefficients <- unclass(lmtest::coeftest(x, vcov. = NeweyWest))
  if(newey=='HAC') temp.summ$coefficients <- unclass(lmtest::coeftest(x, vcov. = vcovHAC)) # package sandwich
  if(standardized) temp.summ$coefficients[,'Estimate'] <- lm.beta(x)$standardized.coefficients
  return(temp.summ)
}

### Calculations ----
# selfcoded blockbootstrap
fbootstrap <- function(data,size=5,statistics) {
  nrow = nrow(data)
  # size = 5
  # dataq[[1]][(nrow-4):(nrow-5+5),]
  nums <- sample(c(1:(nrow-size+1)), replace = FALSE)
  miau2 <- lapply(nums, function(x) {
    y <- x+size-1
    data[x:y,]
  })
  miau <- lapply(nums, function(x) {
    y <- x+size-1
    statistics(data[x:y,])
  })
  return(list(unlist(miau),miau2))
}

seq_block = function(data,statistics,blocks,by) { # blocks
  # if(is.character(statistics)) {
  #   B <- function(z) {}
  #   body(B) <- parse(text = statistics) }
  if(!is.character(statistics)) {
    B <- statistics
  }
  all <- lapply(blocks, function(from) B(data[seq(from,nrow(data),by=by),]))
  mean(unlist(all))
}

rv_calc = function(data, method='annualized') { ### CHECK ALSO f_vol_daily
  RV = apply.daily(data, function(x) sum(x^2, na.rm = T))
  RV = removeSundays(RV)
  if(method=='annualized') {
    RVOL = sqrt(RV*260)*100 # RVOL = sqrt(RV*365)*100 
    index(RVOL) <- as.Date(index(RVOL), tz=indexTZ(RVOL))
    return(RVOL)
  }
  if(method=='annualizedvar') {
    RVOL = RV*260
    index(RVOL) <- as.Date(index(RVOL), tz=indexTZ(RVOL))
    return(RVOL)
  }
  index(RV) <- as.Date(index(RV), tz=indexTZ(RV))
  return(RV)
}
rssimple <- function(x) {
  n <- length(x)
  y <- x - mean(x)
  s <- cumsum(y)
  rs <- (max(s) - min(s))/sd(x)
  log(rs)/log(n)
}
ou_chan <- function(z, return='coef') {
  # z = xtsObject[,1]
  # z <- z+1
  data <- cbind(z,lag.xts(z)) ### ACHTUNG! falls packages geladen
  colnames(data) <- c('z','prevz')
  data$dz <- data$z-data$prevz
  # data$dz <- diff(log(data$z))
  data$prevz_m <- data$prevz-as.numeric(mean(data$z, na.rm=T)) # as.numeric(mean(data$prevz, na.rm=T))
  datacheck <<- data
  xxx <- lm(dz ~ prevz_m, data = data) # assumes dz=theta*(z-mean(z))dt+w, where w is error term
  theta2 <- lm.beta::lm.beta(xxx)$coefficients[2]
  theta <- xxx$coefficients[2]
  halflife=-log(2)/theta
  res <- summary(xxx)
  if(return=='halflife') return(halflife)
  if(return=='coef') return(theta) # theta2 # return(res$coefficients[,'Estimate'][2])
  if(return=='t') return(abs(res$coefficients[,'t value'][2]))
  return(res)
}
felix_trend <- function(data, return='t') { 
  # see trend.R in folder R
  # asd <- dynlm::dynlm(Open ~ trend(Open), data = xtsObject)
  # colnames(data) <- 'Open'
  # asd <- dynlm(Open ~ trend(Open), data = data) # dynlm::
  # # asd <- dynlm::dynlm(data[,1] ~ trend(data[,1])) # ERROR VARIABLE LENGTHS DIFFER
  ads2 <- data.frame(as.numeric(data[,1]))
  ads2$y <- rownames(ads2)
  # ads2$y <- seq(0,1,length.out = nrow(xtsObject))
  colnames(ads2) <- c('dep','indep')
  asd = lm(as.numeric(ads2$dep) ~ as.numeric(ads2$indep))
  # lm.beta::lm.beta(asd)
  res <- summary(asd)
  if(return=='coef') return(res$coefficients[,'Estimate'][2])
  if(return=='t') return(abs(res$coefficients[,'t value'][2]))
  return(res)
}
pop <- function(x) { 
  if(class(data)[1]=='xts') res <- nrow(x[x>0])/nrow(x)
  if(class(data)[1]!='xts') res <- length(x[x>0])/length(x)
  return(res)
}

### Holidays ----
holremove <- function(x,daysafter='yes')  # updated!
{ 
  require(timeDate) # as.Date(index(head(AUDJPY['2006-03-13'])), tz='') as.Date(index(head(AUDJPY['2006-03-13'])), tz='EST5EDT') das problem tritt aber nicht auf wenn US hours 930-1600 sind
  x=x[!as.Date(index(x),tz='EST5EDT') %in% c(as.Date(holiday(2005:2015, Holiday = listHolidays('US'))),
                                             as.Date(holiday(2005:2015, "ChristmasEve")),as.Date(DENewYearsEve(2005:2015)))]
  ### wie bei HF dataset
  if(daysafter=='yes') {
    unwanted=c(as.Date("2005-01-02"),as.Date("2006-01-02"),as.Date("2007-01-02"),as.Date("2008-01-02"),as.Date("2009-01-02"),as.Date("2010-01-02"),as.Date("2011-01-02"),as.Date("2012-01-02"),as.Date("2013-01-02"),as.Date("2014-01-02"),as.Date("2015-01-02"),as.Date("2016-01-02"),
               as.Date("2005-12-26"),as.Date("2006-12-26"),as.Date("2007-12-26"),as.Date("2008-12-26"),as.Date("2009-12-26"),as.Date("2010-12-26"),as.Date("2011-12-26"),as.Date("2012-12-26"),as.Date("2013-12-26"),as.Date("2014-12-26"),as.Date("2015-12-26"),as.Date("2016-12-26"))
    x=x[!as.Date(index(x),tz='EST5EDT') %in% unwanted] }
  return(x)
}
showholidays <- function(from=1990,to=2020) 
{ 
  require(timeDate)
  dates1 = c(as.character(holiday(from:to, Holiday = listHolidays('US'))), 
             as.character(holiday(from:to, "ChristmasEve")), 
             as.character(DENewYearsEve(from:to)))
  sort(dates1)
}
isholremoved <- function(x,from=1990,to=2020,daysafter='yes') # daysafter makes clear: which holidays: only holidays or also 2.1. and 26.12.
{ 
  require(timeDate)
  dates1 = c(as.character(holiday(from:to, Holiday = listHolidays('US'))), 
             as.character(holiday(from:to, "ChristmasEve")), 
             as.character(DENewYearsEve(from:to)))
  output=c(); for (i in from:to){ output[i]=as.character(paste(i,'-01-02',sep='')) }; dates2=na.omit(output)
  output=c(); for (i in from:to){ output[i]=as.character(paste(i,'-12-26',sep='')) }; dates3=na.omit(output)
  unwanted = as.Date(c(dates1,dates2,dates3))
  if(daysafter=='no') unwanted = as.Date(c(dates1))
  if(daysafter=='check') unwanted = as.Date(c(dates2,dates3))
  if(daysafter=='yes') unwanted = as.Date(c(dates1,dates2,dates3))
  x[as.Date(index(x),tz='EST5EDT') %in% unwanted] 
}

### Overnight/Intraday ----
ret_overnight <- function(x, y=c(9,34,15,56)) {
  data <- rbind(x[.indexhour(x)==y[1] & .indexmin(x)==y[2]],x[.indexhour(x)==y[3] & .indexmin(x)==y[4]])
  returns <- diff(log(data))
  final <- do.call(rbind, lapply(split(returns, 'days'), first))
  index(final) <- as.Date(index(final), tz=indexTZ(final))
  return(final)
}

ret_intraday <- function(x, y=c(9,35,15,55)) {
  data <- rbind(x[.indexhour(x)==y[1] & .indexmin(x)==y[2]],x[.indexhour(x)==y[3] & .indexmin(x)==y[4]])
  returns <- diff(log(data))
  final <- do.call(rbind, lapply(split(returns, 'days'), last))
  index(final) <- as.Date(index(final), tz=indexTZ(final))
  return(final)
}

### Outliers ----
# if(sum(x %in% 'Outliers')>=1) {

checkoutliers_ba <- function(z, j=0.999, print='FALSE') # z=variables, j=which quantile, print or not
{
  print(quantile(z[,8]-z[,4], probs=c(0.95,0.96,0.97,0.98,0.99,0.995,0.999,0.9995,0.9999), na.rm=T)) #na.rm new
  print(paste0('using ',j))
  uni <- unique( z[,8]-z[,4][which(z[,8]-z[,4]>quantile(z[,8]-z[,4], probs=c(j), na.rm=T))] )
  uni <- sort(uni, decreasing = T)
  outl <- z[(z[,8]-z[,4]) %in% uni]
  dates <- unique(format(index(outl), '%Y-%m-%d'))
  print(uni); print(nrow(outl))
  # if(print=='TRUE') print(z[(z[,8]-z[,4]) %in% c(uni[4:length(uni)])]) # wieso 4?
  if(print=='TRUE') print(outl)
  if(print=='DATES') print(dates)
  # return(unique(format(index(z[which(z[,8]-z[,4]>quantile(z[,8]-z[,4], probs=c(j), na.rm=T))]), '%Y-%m-%d'))) 
  return(invisible(outl))
  # return(sort(uni, decreasing = T))
}

removeoutliers_ba_quantile <- function(z,j,print='FALSE') # new variable inserted 2017 / changed aug 2017
{
  q <- quantile(z[,8]-z[,4], probs = j, na.rm=T); print(q) # j = 0.9999
  print(sort(unique(z[,8]-z[,4][which(z[,8]-z[,4]>q)]), decreasing = T))
  outl <- which(z[,8]-z[,4]>=q)
  outl_zero <- which(z[,8]-z[,4]==0)
  outl_neg <- which(z[,8]-z[,4]<0)
  print(paste(length(outl),'+',length(outl_zero),'+',length(outl_neg)))
  if(print=='TRUE') { print(z[outl]); print(z[outl_zero]); print(z[outl_neg]) }
  z=z[-c(outl,outl_zero,outl_neg)]
  return(z)
}

printoutliers <- function(z,j,k=0) # wofuer ist k? 
{
  if(ncol(z)!=1) { q=quantile(z[,8]-z[,4], probs = j, na.rm=T)
  print(sort(unique(z[,8]-z[,4][which(z[,8]-z[,4]>q)]), decreasing = T)); print(q)
  if(k==1) print(z[which(z[,8]-z[,4]>q)])
  if(k==1) print(z[which(z[,8]-z[,4]<0)])
  z=z[-which(z[,8]-z[,4]>q)]
  z=z[-which(z[,8]-z[,4]<0)]
  return(z) }
  if(ncol(z)==1) { print(quantile(na.omit(z), probs=c(0.95,0.96,0.97,0.98,0.99,0.995,0.999,0.9995,0.9999,0.99999)))
    print(quantile(na.omit(z), probs=(1-c(0.95,0.96,0.97,0.98,0.99,0.995,0.999,0.9995,0.9999,0.99999)))) }
}

makewithoutoutliers <- function(z,j) # new variable inserted 2017
{
  x = readRDS(paste('~/Dropbox/data/',z,'amin.rds',sep = '')) # assign?
  x <- x[!duplicated(index(x)),]
  x = x['1980/']
  indexTZ(x) = 'EST5EDT'
  x = x['T09:30/T16:00']
  remove = c(0,which(is.na(x[,4])),which(is.na(x[,8])),which(x[,8]-x[,4] < 0),
             which(x[,8]-x[,4] >= quantile(x[,8]-x[,4], probs = j, na.rm = T))) # wozu c0 nochmal? 
  # rbind index muesste auch gehen
  # outliers1=DBVmin[,4][which(DBVmin[,4]>1)]; nrow(outliers1) ### somit kriegt man day after thanksgiving (friday)
  # outliers2=DBVmin[,4][which(DBVmin[,4]<0)] ### !
  # outliers=rbind(outliers1,outliers2) #outliers
  # DBVmin=DBVmin[-DBVmin[index(outliers), which.i=TRUE]]
  y = x[remove]
  x = x[-remove]
  print(length(remove)) # print how many
  saveRDS(y, paste('~/Dropbox/',z,'mindeleted.rds',sep = ''))
  assign(z, x, envir = .GlobalEnv)
  saveRDS(x, paste('~/Dropbox/',z,'minclean.rds',sep = ''))
}

outliers2017 <- function(x,probs,return='outliers') {
  outl <- na.omit(abs(diff(x)))
  na <- cbind(x,NA)[,2] # series with NA's and same dates
  quant <- quantile(outl, probs=probs)
  outliers <- outl[which(outl>quant)]
  data <- x[index(x) %in% index(outliers)]
  removed <- x[!index(x) %in% index(outliers)]
  na2 <- na[!index(na) %in% index(removed)] # print(na2)
  cleaned <- na.locf(rbind(x[!index(x) %in% index(outliers)],na2))
  if(return=='outliers') return(outliers)
  if(return=='data') return(data)
  if(return=='cleaned') return(cleaned)
  if(return=='removed') return(removed)
}

mod_hampel <- function (x, k, t0 = 3, nu=0.0005) {
  n <- dim(x)[1]
  y <- x
  ind <- c()   #vector with the corrected (by filter) elements
  L <- 1.4826
  if(mean(x,na.rm = TRUE)>50) {nu <- 0.05} #for JPY use nu=0.05
  
  for (j in 1: dim(x)[2]) {   #loop through currencies
    for (i in (k + 1):(n - k)) {  #loop through time
      x0 <- median(x[(i - k):(i + k),j],na.rm = TRUE)
      S0 <- L * median(abs(x[(i - k):(i + k),j] - x0),na.rm = TRUE)
      if (!is.na(x[i,j])) {
        if (abs(x[i,j] - x0) > (t0 * S0 + nu) ) {             #+nu makes it less responsive
          y[i,j] <- x0
          ind <- c(ind, i)
        }
      }
    }
  }
  list(y = y, ind = ind)
}


