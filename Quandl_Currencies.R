require(Quandl)
Quandl.api_key('dyYHhrVEtyv9u_Hm7FhN')

Quandl('WARSAWSE/WINDICES',type='xts')
getFX('EUR/USD'); getFX('EUR/PLN')
getFX('USD/ZAR', from = Sys.Date() - 5*364)
plot(USDZAR)
Quandl('ODA/ZAF_NGDP',type='xts') # LCU GDP in South Africa
Quandl('ODA/ZAF_PCPIPCH',type='xts') # Inflation % change

Quandl.search(query = 'South African Rand')
plot(Quandl('NASDAQOMX/NQZAZAR',type='xts')) # NASDAQ South Africa ZAR Index (NQZAZAR)
plot(Quandl('UKONS/MRET_AJFW_M',type='xts')) # ???
# SGE/ZAFIR Trading Economics
plot(Quandl('WORLDBANK/ZAF_FR_INR_DPST',type='xts')) # ???

EURUSD <- Quandl("QUANDL/EURUSD", start_date="2014-01-01",end_date="2014-07-01", type="xts")
check <- Quandl('CURRFX/USDZAR',type='xts')
head(check)
# https://walczak.org/2015/09/analysing-quandl-fx-data-in-r-plotting-decomposing-time-series-and-detecting-anomalies/
plot(Quandl('CURRFX/USDZAR',type='xts')) # ???
plot(Quandl('CURRFX/USDBRL',type='xts')) # ???
plot(Quandl('CURRFX/USDMXN',type='xts')['1994/1997']) # ???

plot(Quandl('CURRFX/USDSEK',type='xts')) # ???
last(Quandl('FRED/USD2MTD156N',type='xts')) # one month libor gibt es nicht mehr

### ### ### ### ### ### ### ### END OF WEEK VOLATILITY / PERC MOVE ### ### ### ### ### ### ### ### 
### ### ### ### ### ### ### ### END OF WEEK VOLATILITY / PERC MOVE ### ### ### ### ### ### ### ### 
### check volatility and range of Friday's and exclude job report dates
opt=readRDS('~/Dropbox/opt.rds')
time = paste(substr(Sys.Date(),1,4),substr(Sys.Date(),6,7),substr(Sys.Date(),9,10)," 00:00:00 CET", sep = '')
opt2=sapply(opt, "[", 2)
for (i in 1:3) # length(opt2)
{
  plot(stream(opt2[[i]],time,'1 min','5 D','ASK')[,4]-stream(opt2[[i]],time,'1 min','5 D','BID')[,4], 
       main=paste(opt2[[i]]$strike, opt2[[i]]$right), ylim=c(0,0.3))
}

EURUSD=readRDS('~/Dropbox/data/UStime old?/EURUSDminustime.rds')
Sys.timezone()
Sys.setenv(tz='EST5EDT')
as.Date(index(EURUSD))[505:506]
as.Date(index(EURUSD), tz='EST5EDT')[505:506]
index(EURUSD)[505:506]
weekdays(as.Date(index(EURUSD), tz='EST5EDT')[505:506])
weekdays(as.Date(index(EURUSD))[505:506])
subset=EURUSD[EURUSD %in% weekdays(as.Date(index(EURUSD), tz='EST5EDT'))=='Friday'] # funktioniert nicht
# wday(EURUSD[505:506]) beruecksichtigt keine timezones
subset=EURUSD[which(weekdays(as.Date(index(EURUSD), tz='EST5EDT'))=='Friday'),]
dailyrange = function(x) { max(x[,4], na.rm=T)-min(x[,8], na.rm=T) }
dailyrange2 = function(x) { as.numeric(first(x[,4], na.rm=T))-as.numeric(last(x[,8], na.rm=T)) }
hist(apply.daily(subset, dailyrange))
hist(apply.daily(subset, dailyrange2))
hist(apply.daily(subset, sd))
# I crudely use a .16 vol as representing about a 1% move on average per day.
### ### ### ### ### ### ### ### END OF WEEK VOLATILITY / PERC MOVE ### ### ### ### ### ### ### ### 
### ### ### ### ### ### ### ### END OF WEEK VOLATILITY / PERC MOVE ### ### ### ### ### ### ### ### 

### ### ### ### ### ### ### ### DOLLAR INDEX OVER TIME? ### ### ### ### ### ### ### ### 
### ### ### ### ### ### ### ### DOLLAR INDEX OVER TIME? ### ### ### ### ### ### ### ### 
require(gdata)
mdata=read.xls('~/Downloads/Diss2/PAPER1/mdata.xlsx', sheet = 5, encoding = "UTF-8") # colnames funktionieren nicht, wahrscheinlich weil zu lang
options(java.parameters = "-Xmx16000m")
mdata=read.xlsx('~/Downloads/Diss2/PAPER1/mdata.xlsx', sheetIndex = 5, stringsAsFactors=F)
### funktioniert beides nicht
mdata=read.xls('~/Downloads/Diss2/PAPER1/monthly_portfolios_1.xlsx', header=FALSE, stringsAsFactors=F)
head(mdata[,12]) # gibt immer noch factors

# load('~/Downloads/Diss/forward.dat') # df.forward 1976-01-01 / 2013-11-29
# load('~/Downloads/Diss/spot.dat') #df.spot 1976-01-01 / 2013-11-29
# df.spot2=as.xts(df.spot, order.by = as.Date(rownames(df.spot)))

x = readRDS('portfolio_dev.rds')
x = readRDS('ind.returns.m.rds') # ist LIST und SP ist spot RATES und FD ist forward RATE (?)

plot(cumsum(x$rx$GBP['1991/1993']))

df.forward=readRDS('~/Dropbox/MS/df.forward.rds')
df.spot=readRDS('~/Dropbox/MS/df.spot.rds')
GBPfd = log(df.forward$GBP)-log(df.spot$GBP)
GBPfd['1991']
# lines(GBPfd*12, col='red') # wenn vorher von bloomberg ausgefuehrt
plot.zoo(GBPfd['1991/1993']*12, col='red', ylim=c(5,10))
plot.zoo(x$fd$GBP['1991/1993']*12, col='red')

plot.zoo(LIBOR['1991/1993'], plot.type = 'single')
plot.zoo(LIBOR$diff['1991/1993'])

plot.zoo(x$sp$GBP['1991/1993'], col='red')
plot.zoo(1/df.spot2$GBP['1991/1993'])

# makecurrency = function(x) funktioniert nicht
# {
#   temp=xts(cbind('spot'=get(paste('df.spot',x,sep='')),'forward'=get(paste('df.forward',x,sep=''))), order.by = as.Date(index(df.spot)))
#   assign(x, temp)
# }

CHF=xts(cbind('spot'=df.spot$CHF,'forward'=df.forward$CHF,'discount'=log(df.spot$CHF)-log(df.forward$CHF)), order.by = as.Date(index(df.spot)))
JPY=xts(cbind('spot'=df.spot$JPY,'forward'=df.forward$JPY,'discount'=log(df.spot$JPY)-log(df.forward$JPY)), order.by = as.Date(index(df.spot)))
CAD=xts(cbind('spot'=df.spot$CAD,'forward'=df.forward$CAD,'discount'=log(df.spot$CAD)-log(df.forward$CAD)), order.by = as.Date(index(df.spot)))
DEM=xts(cbind('spot'=df.spot$DEM,'forward'=df.forward$DEM,'discount'=log(df.spot$DEM)-log(df.forward$DEM)), order.by = as.Date(index(df.spot)))

plot(as.zoo(apply.monthly(DEM[,3], mean, trim=0.3)['1978/1986']), ylab='')
par(new = TRUE) 
plot(as.zoo(apply.monthly(DEM$spot, mean)['1978/1986']), col='red', ylab='', yaxt='n')
axis(side = 4)

plot(as.zoo(apply.monthly(JPY[,3], mean, trim=0.3)['1978/1986']), ylab='')
par(new = TRUE) 
plot(as.zoo(apply.monthly(JPY$spot, mean)['1978/1986']), col='red', ylab='', yaxt='n')
axis(side = 4)

plot(as.zoo(apply.monthly(CAD[,3], mean, trim=0.3)['1978/1986']), ylab='')
par(new = TRUE) 
plot(as.zoo(apply.monthly(CAD$spot, mean)['1978/1986']), col='red', ylab='', yaxt='n')
axis(side = 4)

dxy_a=Quandl('FRED/DTWEXM', type='xts')
dxy_a=Quandl('FED/JRXWTFN_N_M', type='xts') # same series ?

meanfd=xts(apply.monthly(rowMeans(log(df.spot), na.rm=T)-rowMeans(log(df.forward), na.rm=T), mean, trim=0.4), # 0.2 sieht schon ganz anders aus!!!
           order.by = as.Date(rownames(apply.monthly(rowMeans(log(df.forward), na.rm=T), mean)))) # not index!
plot(as.zoo(meanfd['1978/1986']))
abline(h=0, col='blue')
par(new = TRUE)
plot(as.zoo(dxy_a['1978/1986']), ylab='', yaxt='n')
# CFTC/DX_F_L_ALL
# Quandl has historical data for the ICE US Dollar Index Futures (DX) futures contract going back to 1999.

# https://blog.quandl.com/api-for-currency-data Bank of England!

ifelse(diff(abs(DEM$discount))>0.005,3,5)
plot(diff(abs(DEM$discount)))
require('outliers')
plot(rm.outlier(DEM$discount))

dxy_weights=read.xls('~/trading/dxy_weights.xlsx', sheet=1)
rownames(dxy_weights)=dxy_weights[,1]
dxy=data.frame(dxy_weights[,2:ncol(dxy_weights)], row.names=dxy_weights[,1])
dxy
asd=dxy_weights[,39, drop=FALSE]; asd # keeps rownames
asd=dxy_weights[,3, drop=FALSE] # keeps rownames
asd$asd<-1
asd[order(asd[,1], decreasing=T),]

## get Euro weights
dxy_weights=read.xls('~/trading/dxy_weights.xlsx', sheet=2)
rownames(dxy_weights)=dxy_weights[,1]
dxy=data.frame(dxy_weights[,2:ncol(dxy_weights)], row.names=dxy_weights[,1])
dxy
asd=dxy_weights[,39, drop=FALSE]; asd # keeps rownames

head(df.spot2)
plot(df.spot2$DEM['1980/1987'])
plot(df.spot2$CAD['1980/1987'])
plot(df.spot2$JPY['1980/1987'])
plot(df.spot2$GBP['1980/1987'])
plot(df.spot2$CHF['/1980']) # mindestkurs
EURCHF=Quandl('ECB/EURCHF', type='xts')['2008-11/2012-11']
plot(as.zoo(df.spot2$CHF['1976/1979']/df.spot2$DEM['1976/1979']), main='Einführung Mindestkurs', ylab='') # mindestkurs
abline(v=as.Date('1978-10-02'), col='blue')
par(new = TRUE)
plot(as.zoo(EURCHF), xaxt='n', yaxt='n', col='red', xlab='', ylab='')
axis(side = 4)
abline(v=as.Date('2011-09-06'), col='blue')

EURCHF=Quandl('ECB/EURCHF', type='xts')['2008/2009']
plot(EURCHF['2009-03']) # Geldpolitische Lagebeurteilung vom 12. März 2009 Die Schweizerische Nationalbank nimmt eine kräftige Lockerung der monetären Bedingungen vor

plot(df.spot2$MXN) # data history reicht nicht
plot(df.spot2$CAD['2013/'])

# 1980
# Euro area*       20.773 
# Canada*          18.849 
# Japan*           16.708 
# United Kingdom*   7.236 
# Mexico            4.419 

# 2016
# China            21.892 
# Euro area*       17.056 
# Mexico           12.600 
# Canada*          11.977 
# Japan*            6.281 

dxy_a=Quandl('FRED/DTWEXM', type='xts')
plot(dxy_a['1980/1987'])

# US Dollar Index DXY, not trade weighted
# Euro (EUR), 57.6% weight
# Japanese yen (JPY) 13.6% weight
# Pound sterling (GBP), 11.9% weight
# Canadian dollar (CAD), 9.1% weight
# Swedish krona (SEK), 4.2% weight

# getSymbols('DX-Y.NYB')
plot(DX)
plot()
order(asd)
asd
order(asd)
order(asd[,1])
# but none of the Q&A I've seen on SO have dealt with single column data frames. @eddi 's answer doesnt work when you only have one column:
# I have the same problem sorting a single column and my kludgy solution was to add a second column e.g. df.1col$one <- 1 and that worked
### ### ### ### ### ### ### ### DOLLAR INDEX OVER TIME? ### ### ### ### ### ### ### ### 
### ### ### ### ### ### ### ### DOLLAR INDEX OVER TIME? ### ### ### ### ### ### ### ### 

# was beachtet die SNB?
plot(Quandl('CURRFX/USDCHF',type='xts')['2015/'])
plot(Quandl('CURRFX/EURCHF',type='xts')['2015/'])
plot(0.75*Quandl('CURRFX/EURCHF',type='xts')+0.25*Quandl('CURRFX/USDCHF',type='xts')['2015/'], main='CHF Interventions?')

# BIS REER narrow1704.xlsx full_WEBSTATS_EER_D_DATAFLOW_csv

### Andy Krieger Vs. the Kiwi --- following the Black Monday crash. ----
# As investors and companies rushed out of the American dollar and into other currencies 
# that had suffered less damage in the market crash, there were bound to be some currencies 
# that would become fundamentally overvalued, creating a good opportunity for arbitrage.

spot=readRDS('~/Dropbox/MS/df.spot.rds')
plot(spot$NZD['1987-05/1988-07'])
plot(1/spot$AUD)

### new -----
head(Quandl('CURRFX/USDCHF', type='xts', start_date="1980-01-01"))
head(Quandl('CUR/AUD', type='xts', start_date="1980-01-01"))
head(Quandl('RBA/FXRUSD', type='xts', start_date="1980-01-01"))

FXTOP=read.xls('~/trading/FXTOP_Rates.xlsx', sheet=1)
FXTOP=xts(FXTOP[,2:ncol(FXTOP)], as.yearmon(FXTOP[,1], '%b-%Y'))
plot(FXTOP$ZAR)
plot(df.spot$ZAR)
library(chron)
# FXTOP=xts(FXTOP[,2], as.yearmon(paste(substr(FXTOP[,1], 1, 3),
#           year(as.Date(chron(format(as.Date(substr(FXTOP[,1], 5, 6), "%y"), "%m/%d/%y"))))), "%b %Y"))
saveRDS(FXTOP, '~/trading/FXTOP.rds')
FXTOP <- readRDS('~/trading/FXTOP.rds')
plot(FXTOP[,'GBP']['1999/2005'])
FXTOP[,'GBP']
df.spot2
lapply(colnames(FXTOP), function(x) { doubleplot(FXTOP[,x],df.spot[,x], main=x, a='full', b='align') })

### COASTLINE
test <- readRDS(gzcon(url("https://dl.dropboxusercontent.com/s/uo19beaq2lw0lii/EURCHF2016.rds")))[,4]
coastline <- apply.daily(test, function(x) sum(abs(diff(x)), na.rm=T))
daily <- apply.daily(test, last)
range <- apply.daily(test, function(x) range(x)[2]-range(x)[1] )
xxx <- cbind(coastline,daily,range)
colnames(xxx) <- c('coastline','rate','range')
index(xxx) <- as.Date(index(xxx))
xxx2=xxx[!weekdays(index(xxx)) %in% 'Sunday']
xxx2
plot.zoo(xxx2)
plot.zoo(xxx2[,c(1,3)], plot.type = 'single')

### MARTINGALE NEW
require(xts)
source('~/Dropbox/github/github/martingale.R')
source('~/Dropbox/github/github/martingale_2delete.R')

test <- readRDS(gzcon(url("https://dl.dropboxusercontent.com/s/uo19beaq2lw0lii/EURCHF2016.rds")))
head(test)
rm(test)
asd_old=martingale(test,'2016-05','2016-06',0.0001,100000,2,2,0)
asd_old$B=ifelse(diff(asd_old$outstanding)>0,asd_old$F,NA)
asd_old$S=ifelse(diff(asd_old$outstanding)<0,asd_old$F,NA)
sum(asd_old$B, na.rm=T)

asd=martingale(test,'2016-05','2016-06',0.0001,0.0001,100000,2,2,0)
asd$B=ifelse(diff(asd$outstanding)>0,asd$F,NA)
asd$S=ifelse(diff(asd$outstanding)<0,asd$F,NA)
sum(asd$B, na.rm=T)

tail(cbind(asd_old$B, asd$B), 100)

asdx=asd['2016-05-04']
plot(asd[,4]['2016-05-04'])
setwd('~/')
save('Maschine2017b', width=20, height=8)
plot.zoo((asdx[,4]+asdx[,8])/2, main='EUR/CHF 2016-05-04', ylab='', xlab='')
points(asdx$B, pch='-', col='blue') # cex=1
points(asdx$S, pch='-', col='red')
dev.off()

asd=martingale(test['2016-05'],'2016-05','2016-06',0.0001,0.0010,100000,2,'summary','plot') 
asd=martingale(test['2016-05-04'],'2016-05','2016-06',0.0001,0.0010,100000,2,1,1) 
asd=martingale(test['2016-05'],'2016-05','2016-06',0.0001,0.0010,100000,2,'all',1) 

head(asd)
mid=(asd[,4]+asd[,8])/2
M1 <- as.numeric(first((asd[, 4] + asd[, 8])/2))
M2 <- ((asd[, 4] + asd[, 8])/2 - M1)/2 * asd$outstanding
M3 <- cumsum(asd$realized_profit)
M4 <- M2 + M3
temp1=asd
require(ggplot2)

M=asd
plot(as.zoo(M[,1]), col='red', ylab='Underlying', main='Machine')
mtext(paste('from',index(first(M)),'to',index(last(M))))
par(new = T)
plot(as.zoo(M[,8]), yaxt='n', ylab='', col='blue', lty='dotted', lwd=2)
# M8 <- M4 + M7 + 1000000
axis(side = 4)
par(new = T)
profits=as.zoo(cbind(M[,4]+1000000,M[,7]+1000000,M[,4]+M[,7]+1000000))
profits=as.zoo(cbind(M3+1000000,M2+1000000,M4+1000000))

index(profits)=as.POSIXct(index(profits))
profits=cbind(profits,as.zoo(mid))
profits=na.locf(profits)
profits

save('newmaschine2')
plot(profits[,1], ylim=c(min(profits[,c(1:3)], na.rm=T),max(profits[,c(1:3)], na.rm=T)), ylab='', xlab='', 
     las=2, col='gray', lwd=1.25, lty='dotted', cex.axis=0.8)

plot(as.Date(index(profits[,1])), coredata(profits[,1])) # incl. weekends
plot.zoo(coredata(mid), xaxt='n', cex.axis=0.8)

# index(mid)[seq(1,nrow(mid),by=as.integer(nrow(mid)/10))]

indexTZ(mid)='UTC' # man muss vorher indexTZ setzen, sonst funktioniert apply.weekly nicht gut
weekdays(as.Date(index(apply.weekly(mid, last))))
which(index(mid) %in% index(apply.weekly(mid, last))==TRUE)

findgaps(mid, 'weekendcurrencies')

which( diff(index(mid))>16 )-5
which( diff(index(mid))>16 )+5
# test=mid[as.character(as.Date(index(mid[ which( diff(index(mid))>16 ) ])))] # subsettet for each

par(las = 2)
# weekdays(as.Date('2016-05-02'))
# paste(lapply(split(mid, 'weeks'), function(x) index(last(x)))) # SPLITTET NICHT KORREKT split(mid, 'weeks')[2]
axis(cex.axis=0.8, side = 1, at = which(index(mid) %in% index(apply.weekly(mid, last))==TRUE), 
     labels = format(as.Date(index(mid)[index(mid) %in% index(apply.weekly(mid, last))]), '%b-%d'))

axis(side = 1, at = seq(1,nrow(mid),by=as.integer(nrow(mid)/10)), 
     labels = as.Date(index(mid)[seq(1,nrow(mid),by=as.integer(nrow(mid)/10))], tz=''))
plot(mid)
# require(quantmod) chartSeries(na.omit(profits[,3]), theme='white')

polygon( c(index(profits),rev(index(profits))), 
         c(c(coredata(profits[,3])),rev(c(coredata(profits[,2])))), border='white', 
         # c(x,rev(x)), 
         col='lightgray') #adjustcolor("green", alpha.f = 0.5))
lines(profits[,3], col='red', lwd=1.5)
lines(profits[,2], col='blue', lwd=1.25, lty='dotted') # type='b')
dev.off()

par(new = T)
plot(as.zoo(mid), col='red', ylab='Underlying', main='Machine')
mtext(paste('from',index(first(M)),'to',index(last(M))))


legend('topleft', c('Underlying','Machine'), cex=0.6, lty=c('solid','dotted'), col=c('red','blue'))

# at should be round number
asd$B=ifelse(diff(asd$outstanding)>0,asd$at,NA)
asd$S=ifelse(diff(asd$outstanding)<0,asd$at,NA)
asdx=asd

plot.zoo((asdx[,4]+asdx[,8])/2, main='EUR/CHF 2016-05-04', ylab='', xlab='')
points(asdx$B, pch='-', col='blue') # cex=1
points(asdx$S, pch='-', col='red')
asdx

###
require(xts)
eins=readRDS('~/Dropbox/HYG.rds')
eins[[529]]
einsx=do.call(rbind, eins)
saveRDS(einsx, 'HYGimpl.rds')
HYG=readRDS('~/Dropbox/data/universe/HYGamin.rds')
HYG=readRDS('~/Dropbox/HYGnew.rds')
HYG[[1]]
HYG2=do.call(rbind, HYG)

HYG <- asd
indexTZ(HYG)='EST5EDT'
head(einsx['2014-12-08'])
head(HYG['2014-12-08 09:25/'], 90) # market opening um 10:30 (wenn Sys.timezone EST5EDT)
head(HYG['2014-12-08 15:50/'], 90)
HYG2=readRDS('~/data/HYGmincorrecttime.rds') ### !!!
tes=HYG2['T09:50/T09:50:30']
tes=HYG2['T09:50/T09:50:30']
mean(tes, na.rm=T)
checkcorrecttime(HYG2)
mean(tes, na.rm=T)
head(HYG2)

indexTZ(HYG2)
z <- Sys.time()
unclass(z) 
head(HYG2['2014-12-08'], 80)
Sys.setenv(TZ='Europe/Berlin')
Sys.setenv(TZ='EST5EDT')
SPY=readRDS('~/Dropbox/data/other/intlETF/minute/SPYmincorrecttime.rds')
head(SPY['2014-12-08'], 80)

head(einsx)
einsxx=apply.daily(einsx, mean)
plot(einsxx[,4])
head(eins)
einsx['2016-05'] # May missing?

test=download_daily(twsSTK('HYG'))
plot(test[,1])
lines(test[,2], col='red')

eins[[400]]
apply.daily(einsx, function(x) first(x))
plot(einsx['2014-12-08'][,4])
plot(einsx['2014-12-09'][,4])

### CHECK SPY
SPY=readRDS('~/Dropbox/data/other/intlETF/minute/SPYmincorrecttime.rds')
SPY=readRDS('~/Dropbox/data/universe/SPYamin.rds') # FALSCH
SPY=readRDS('~/Dropbox/data/other/intlETF/minute/old/SPYmin.rds')
head(SPY)
reqHistoricalData(tws,twsSTK('SPY'),gsub('-','',format(as.Date('2012-10-21'), "%Y-%m-%d %H:%M:%S")),
                  '1 day','5 D','0','BID')
reqHistoricalData(tws,twsSTK('SPY'),gsub('-','',format(as.Date('2012-10-21'), "%Y-%m-%d %H:%M:%S")),
                  '1 day','5 D','0','ASK')

luk=SPY['2012-10-15']
luk=SPY['2012-10-16'] # 2012-10-16 01:59:00 # 144.33 144.35 144.31 144.31 sind die werte von 2012-10-15 DAILY
tail(SPY['2012-10-15'])
tail(SPY['2012-10-16'])
tail(SPY['2012-10-17'])
tail(SPY['2012-10-18'])
tail(SPY['2012-10-19'])
tail(SPY['2012'])


tws = twsConnect()
accountInfo = reqAccountUpdates(tws)
twsPortfolioValue(accountInfo)
head(accountInfo)
str(accountInfo[[1]])
accountInfo[[1]]
accountInfo[[2]]

tws <- ibgConnect()
id <- reqIds(tws)      
reqExecutions(tws)
# https://stackoverflow.com/questions/35559742/reqexecutions-ibrokers-package
# https://stackoverflow.com/questions/36210244/multiple-quantities-in-bracket-orders-using-ibrokers-in-r

#
require(xts)
require(Quandl)
spot_num <- read.csv("~/R/spot_num.txt", sep='')
spot_num <- xts(spot_num[,c(3:4)], as.yearmon(paste(spot_num$YEAR, spot_num$MON, 01, sep='-')))
saveRDS(spot_num, 'spot_num.rds')
plot(spot_num['1976/'])

Quandl.search(query = 'Iceland interest')
Quandl('SGE/ISLIR', type='xts') # Premium
require(quantmod)
x <- getSymbols('IR3TIB01ISM156N', src='FRED', auto.assign = FALSE)
plot(x)
head(FXTOP)
plot(FXTOP[,'ISK']['2008/2010'])


Quandl.search(query = 'India inflation') # 'INDIA_LAB/INFLATION'
getSymbols('FPCPITOTLZGIND', src='fred') # INTDSRINM193N interest rate
plot.zoo(FXTOP$ZAR)
plot.zoo(FXTOP$IDR)
plot.zoo(FXTOP$INR)
plot.zoo(FXTOP$CNH)

SGE/INDCPIC