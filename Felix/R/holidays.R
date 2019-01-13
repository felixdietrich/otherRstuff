### Outliers ----

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

showholidays <- function(from=1990,to=2020) { 
  require(timeDate)
  dates1 = c(as.character(holiday(from:to, Holiday = listHolidays('US'))), 
             as.character(holiday(from:to, "ChristmasEve")), 
             as.character(DENewYearsEve(from:to)))
  sort(dates1)
}

isholremoved <- function(x,from=1990,to=2020,daysafter='yes') { 
  require(timeDate) # daysafter makes clear: which holidays: only holidays or also 2.1. and 26.12.
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
