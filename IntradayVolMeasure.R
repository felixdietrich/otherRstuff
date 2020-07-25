require(xts)
source('~/R/fritztest.R')

SPY=readRDS('~/Dropbox/data/SPYvolmincorrecttime.rds')
SPY2=apply.weekly(SPY, last)[,6]
SPY2=diff(log(apply.weekly(SPY, last)[,6]))
index(SPY2)=as.Date(index(SPY2))
# strptime(index(SPY2), format='%Y-%m-%d', usetz = FALSE) # geht nicht
SPY=getSymbols('SPY', from='2006-01-01', auto.assign = F)[,6]
SPY=diff(log(apply.weekly(SPY, last)))
test=na.omit(cbind(SPY*100,SPY2*100))
test
summary(lm(test[,2] ~ test[,1])) # 1% weekly change = 0.01 vol point?
summary(lm(test[,2] ~ test[,1])) # 1% weekly change = 4.4% vol change
num=rbind(seq(from=1,by=0.5,to=5))
num
taileval = function(x) { test2=test[which(test[,1]<(-x)),]; mean(test2[,2], na.rm=T) }
apply(num, 2, taileval)

SPY=readRDS('~/Dropbox/data/SPYvolmincorrecttime.rds')
SPY2=apply.monthly(SPY, last)[,6]
index(SPY2)=as.Date(index(SPY2))
SPY=getSymbols('SPY', from='2006-01-01', auto.assign = F)[,6]
SPY=diff(log(apply.monthly(SPY, last)))
test=cbind(SPY*100,SPY2*100)
summary(lm(test[,2] ~ test[,1])) # 1% weekly change = 0.01 vol point?

apply(num, 2, taileval)
test[which(test[,1]<(-5)),]


require(Quandl)
Quandl.search('short interest')

require(xts)
install.packages("sos")
library("sos")
findFn("hurst exponent")


WEC=readRDS('~/data/WECmincorrecttime.rds')
XEL=readRDS('~/data/XELmincorrecttime.rds')
DUK=readRDS('~/data/DUKmincorrecttime.rds')
ED=readRDS('~/data/EDmincorrecttime.rds')

xx=readRDS('~/Documents/EURCHFnew_trump2.rds')
xxx=rbind(fritzb11, xx)
xxx <- xxx[ ! duplicated( index(xxx) ),  ]
saveRDS(xxx, 'EURCHFnew_trump.rds')

saveRDS()
plot(xx)
head(xx)
tail(xx)
# Intraday volatility
# fritzb11, ist das eurchf trump?
plot(fritzb11)
min(fritzb11)

index()
fritzb11[which(fritzb11=max(fritzb11))]
which(fritzb11==max(fritzb11))


max(fritzb11[,4])
asd=which(fritzb11[,4]==1.0824)
fritzb11[asd[1]]
asd
# scalable range
# wenn close<open
points(endpoints(fritzb11[,4], on='hours'), col="red", pch=20) #on=1)
points(index(fritzb11[xx]), col="red", pch=6, on=1) #on=1)
warnings()
abline(v = endpoints(fritzb11[,4], on='hours'), col = 'blue', lty = 3, lwd = 20)
abline(v = index(fritzb11[xx]), col = 'blue', lty = 1, lwd = 2)
## wenn

plot(fritzb11)
if(as.numeric(head(fritzb11[,4],1))<as.numeric(tail(fritzb11[,4],1))) 
  { abline(v = index(fritzb11[which.min(fritzb11[,8]),]), col = 'blue', lty = 1, lwd = 4)
} else { 
  abline(v = index(fritzb11[which.max(fritzb11[,4]),]), col = 'blue', lty = 1, lwd = 4)
  neu=fritzb11[which.max(fritzb11[,4]):nrow(fritzb11),]
  abline(v = index(neu[which(neu==as.numeric(fritzb11[1,4]))[1]]), col = 'red', lty = 1, lwd = 4)
}
neu[which(neu==as.numeric(fritzb11[1,4]))[1]]

fritzb11[1,4]
plot(fritzb11[index(fritzb11[which.max(fritzb11[,4]),]),])
fritzb11[index(fritzb11[which.max(fritzb11[,4]),]):index(tail(fritzb11,1)),]
index(fritzb11[which.max(fritzb11[,4]),])
index(tail(fritzb11,1))
tail(fritzb11,1)
x.subset <- index(x)[1:20]

fritzb11[index(fritzb11[which.max(fritzb11[,4]),])/index(tail(fritzb11,1))]
abline(v = index(fritzb11[which.min(fritzb11[,8]),]), col = 'blue', lty = 1, lwd = 4)
abline(v = index(fritzb11[which.max(fritzb11[,4]),]), col = 'blue', lty = 1, lwd = 4)

which.max(fritzb11[,4])
length(fritzb11)
nrow(length(fritzb11))
nrow(fritzb11)
plot(fritzb11[which.max(fritzb11[,4]):nrow(fritzb11),])
neu=fritzb11[which.max(fritzb11[,4]):nrow(fritzb11),]
index(neu[which(neu==1.08)[1]])

index(max(fritzb11[,4]))
max(fritzb11[,4], which.i=TRUE)
index(fritzb11[which.min(fritzb11[,4]),])

warnings()
warnings()
xx=endpoints(fritzb11[,4], on='hours')
index(fritzb11[xx])

### WAVELET
# https://www.r-bloggers.com/time-series-analysis-and-mining-with-r/
AirPassengers
f <- decompose(AirPassengers) # Classical Seasonal Decomposition by Moving Averages
f$figure
plot(f$figure, type='b', xaxt='n', xlab='')
monthNames <- months(ISOdate(2011,1:12,1))
axis(1, at=1:12, labels=monthNames, las=2)
plot(f)

getSymbols('XEL')
f <- decompose(XEL[,4]) # Classical Seasonal Decomposition by Moving Averages
f$figure
plot(f$figure, type='b', xaxt='n', xlab='')
monthNames <- months(ISOdate(2011,1:12,1))
axis(1, at=1:12, labels=monthNames, las=2)
plot(f)

# https://stat.ethz.ch/R-manual/R-devel/library/stats/html/stl.html
require(graphics)
plot(stl(nottem, "per"))
plot(stl(nottem, s.window = 7, t.window = 50, t.jump = 1))
nottem

stl(fritzb11[,4])
stl(ts(fritzb11,freq=10), t.window=15, s.window="per", robust=TRUE)
ts(fritzb11[,4],freq=1000)

# https://stackoverflow.com/questions/18615254/analyzing-a-time-series-with-r
freq <- 100
ny <- 3 # no of years, i.e. periods
n <- ny * freq
set.seed(13)
tt <- ts(rnorm(n), freq = freq)
s <- stl(tt, "periodic")
plot(s)

# here was wavelet.R

require(xts)
require(PortfolioEffectHFT)
getDividends('JEM')
getSymbols('JEM')
JEM2=cbind(JEM[,4],getDividends('JEM'))
JEM3=cbind(JEM2[,1],cumsum(ifelse(is.na(JEM2[,2]), 0, JEM2[,2])))
JEM3$total=JEM3[,1]+JEM3[,2]
plot(JEM3$total)
plot(JEM[,6])

test=readRDS('~/Dropbox/data/universe/USOamin_2016.rds')
test=readRDS('~/Dropbox/data/universe/AAPLamin.rds')['2016/']
test=readRDS('~/Dropbox/Public/EURCHF2015-2016.rds')['2015-08-10/2016'] # hat scheinbar gaps??
test=readRDS('~/Dropbox/data/currencies/EURCHF2016.rds')
# https://dl.dropboxusercontent.com/u/13594047/GBPUSD2015-2016.rds # why different linkstyle?
# test=readRDS(gzcon(url("https://www.dropbox.com/s/uo19beaq2lw0lii/EURCHF2016.rds"))) 
test=readRDS(gzcon(url("https://dl.dropboxusercontent.com/s/uo19beaq2lw0lii/EURCHF2016.rds"))) 

test=getFX('EUR/CHF', from='2016-01-04', to='2016-12-16', auto.assign = FALSE)
test=getSymbols('USO', from='2016-01-04', to='2016-12-16', auto.assign = FALSE)
test=getSymbols('SPY', from='2012-01-01', to='2016-06-24', auto.assign = FALSE)
plot(test)
HurstIndex(test) # kann nicht sein dass SPY Hurst unter 0.20 muss ein fehler im package sein
tail(test)
require(pracma)
hurstexp(test[,1], d=100) # komplett unterschiedliche werte. am besten selbst bauen! range, sd, real vol 1,5,15,60 min

plot(test['2015-08-13/2016'], main='EURCHF 2015-08-10/2016-06-10')
plot(test['2015-09-01/2015-10-30'], main='EURCHF 2015-09-01/2015-10-30')

checkhowmuch=fritztest2(test,'2015','2016',0.0015,100000,0)
checkhowmuch=martingale(test,'2015','2016',0.0015,100000,0,2,0)

sum(checkhowmuch$realized_profit != 0)/length(unique(as.Date(index(test))))
fritztest(test,'2015','2016',0.0015,100000,2,3,1)
fritztest3(test,'2015','2016',0.0015,100000,0)

histt=hist(apply.daily(checkhowmuch$realized_profit, sum)/150, breaks=max(apply.daily(checkhowmuch$realized_profit, sum)/150, na.rm=T))
plot(histt, xaxt='n', main='Maschine Frequency Histogram', xlab='')
axis(side=1, at=seq(from=0,to=max(apply.daily(checkhowmuch$realized_profit, sum)/150, na.rm=T)))

hist(apply.daily(checkhowmuch$realized_profit, sum)/150, freq=FALSE, breaks=max(apply.daily(checkhowmuch$realized_profit, sum)/150, na.rm=T),
     ylim=c(0,0.5), main='Maschine Probability Histogram', xlab='')
hist(apply.weekly(checkhowmuch$realized_profit, sum)/150, freq=FALSE, breaks=max(apply.weekly(checkhowmuch$realized_profit, sum)/150, na.rm=T),
     main='Maschine Probability Histogram', xlab='')
# "2016-06-24" fast 100 mal

# labels=seq(0:max(apply.daily(checkhowmuch$realized_profit, sum)/150, na.rm=T))
apply.daily(checkhowmuch$realized_profit, sum)[which(apply.daily(checkhowmuch$realized_profit, sum)/150>15)]

pdf(file = "~/Dropbox/Maschine_Freq.pdf", width = 15, height = 5, bg='white') 
plot( index(apply.daily(checkhowmuch$realized_profit, sum)/150), 
      coredata(apply.daily(checkhowmuch$realized_profit, sum)/150), type="h", xlab='Date', ylab='Frequency', 
      main=paste(as.Date(index(first(checkhowmuch))),'-',as.Date(index(last(checkhowmuch)))))
mtext('Red line = mean')
abline(h = sum(checkhowmuch$realized_profit != 0)/length(unique(as.Date(index(test)))), col='red')
dev.off()

lalo=apply.daily(checkhowmuch$realized_profit, sum)/150
lalo2=as.Date(index(lalo[which(lalo>40)]))
lalo2
# which(apply.daily(checkhowmuch$realized_profit, sum)/150>=4)
test[as.Date(index(test)) %in% lalo2[5]]
lalo2
plot(checkhowmuch['2015-08-10'])
lines(checkhowmuch$realized_profit)
sum(checkhowmuch$realized_profit['2015-08-11'])/150
head(test)
#abline(v='2015-08-11 01:30:00', col='red')
plot(checkhowmuch['/2015-08-11'])
lines(checkhowmuch$realized_profit)
write.xlsx(checkhowmuch['/2015-08-11'], 'checkhowmuch.xlsx')
plot(checkhowmuch['/2015-08-11'])
plot(test['/2015-08-11'])
chart.TimeSeries(test[,4]['/2015-08-11'], event.lines = risk.dates, event.labels = risk.labels, event.color = 'red', lwd=1)
risk.dates = index(test[100])
risk.dates = c('2015-08-10 13:50:00 EDT')
risk.labels = c('fritz')
cycles.dates<-paste(index(test[100])-200,'/',index(test[100])+200,sep='')
chart.TimeSeries(test[,4]['/2015-08-11'], period.areas = cycles.dates, period.color = "lightblue")

chart.TimeSeries(apply.daily(test[,4], last), event.lines = risk.dates, event.color = 'red')
risk.dates = c('2015-08-17')
chart.TimeSeries
index(test)
### new test
if(diff(checkhowmuch$outstanding)>0)

checkhowmuch[,11] <- diff(checkhowmuch$outstanding)>0 # funktioniert wegen indices nicht
checkhowmuch2 <- cbind(checkhowmuch,diff(checkhowmuch$outstanding)>0,diff(checkhowmuch$outstanding)<0)
head(checkhowmuch2)
generator1=ifelse(checkhowmuch2$outstanding.1==1, paste('bought at',checkhowmuch2[,7]), 0)
generator2=ifelse(checkhowmuch2$outstanding.2==1, paste('sold at',checkhowmuch2[,2]), 0)

generator=rbind(generator1[generator1!='0'],generator2[generator2!='0']) # timezone ist CEST
#indexTZ(generator)='EST5EDT'
generator3=paste(index(generator)-200,'/',index(generator)+200,sep='')
generator3b=index(generator)
generator4=coredata(generator)
chart.TimeSeries(checkhowmuch2[,4]['/2015-08-11'], period.areas = generator3, period.color = "lightblue")
chart.TimeSeries(checkhowmuch2[,4]['/2015-08-11'], event.lines = generator3b, event.color = 'red')
par(mar=c(5.1,4.1,4.1,2.1)) # default
plot(checkhowmuch2[,4]['/2015-08-11'])
plot(as.zoo(checkhowmuch2[,4]['/2015-08-11']))

lines(generator3b, col='red', lwd=10)
abline(v = as.POSIXct('2015-08-10 07:22:00'), col = 'red', lty = 2)
text(x = as.POSIXct(generator), label = coredata(generator), offset = 0.2, pos = 2, srt = 90, col = 'red')
text(x = as.POSIXct('2015-08-10 07:22:00'), y=1.08, label = 'hitler', offset = 0.2, pos = 2, srt = 90)
text(x = as.POSIXct('2015-08-10 07:22:00'), y=max(checkhowmuch2[,4]['/2015-08-11']), label = 'hitler', adj = c(1,0.5), srt = 90, cex=0.8) # aligned right, sonst center
# text(x = as.POSIXct('2015-08-10 07:22:00'), y=max(checkhowmuch2[,4]['/2015-08-11']), label = 'hitler', srt = 90, cex=0.8) # other tests

checkhowmuch2 <- cbind(checkhowmuch,diff(checkhowmuch$outstanding)>0,diff(checkhowmuch$outstanding)<0)['2015-12-03']
head(checkhowmuch2)
# generator1=ifelse(checkhowmuch2$outstanding.1==1, paste('bought at',checkhowmuch2[,7]), 0)
# generator2=ifelse(checkhowmuch2$outstanding.2==1, paste('sold at',checkhowmuch2[,2]), 0)
generator1=ifelse(checkhowmuch2$outstanding.1==1, formatC(checkhowmuch2[,7], digits=4,format='f'), 0)
generator2=ifelse(checkhowmuch2$outstanding.2==1, formatC(checkhowmuch2[,2], digits=4,format='f'), 0)
generator1b=generator1[generator1!='0']
generator2b=generator2[generator2!='0']

pdf(file = "~/Dropbox/Maschine_BS3.pdf", width = 35, height = 10, bg='white') 
#plot(checkhowmuch2[,4], main='Maschine')
plot(as.zoo(checkhowmuch2[,4]), main=paste('Maschine',as.Date(index(first(checkhowmuch2[,4])))), ylab='', xlab='')
mtext('Numbers show the either the lowest ask or the highest bid', cex=0.8)
abline(v = as.POSIXct(generator1[generator1!='0']), col = 'green', lty = 2)
abline(v = as.POSIXct(generator2[generator2!='0']), col = 'red', lty = 2)
text(x = as.POSIXct(generator1b), y=min(checkhowmuch2[,8], na.rm=T), label = coredata(generator1b), adj = c(0,0.5), srt = 90, cex=0.8, font=2)
text(x = as.POSIXct(generator2b), y=max(checkhowmuch2[,4], na.rm=T), label = coredata(generator2b), adj = c(1,0.5), srt = 90, cex=0.8, font=2)
dev.off()

# y = ylim[2], cex = cex.labels, 
plot(1:10, 1:10, main = "text(...) examples\n~~~~~~~~~~~~~~", sub = "R is GNU ©, but not ® ...")
mtext("«Latin-1 accented chars»: éè øØ å<Å æ<Æ", side = 3)
points(c(6,2), c(2,1), pch = 3, cex = 4, col = "red")
text(3, 2, "the text is CENTERED around (x,y) = (6,2) by default", cex = .8)
text(2, 1, "or Left/Bottom - JUSTIFIED at (2,1) by 'adj = c(0,0)'", adj = c(1,0))
generator3b
generator

head(checkhowmuch2[which(checkhowmuch2$realized_profit>0)],10)
head(generator3, 10)
head(index(checkhowmuch2[which(checkhowmuch2$outstanding.2==1 | checkhowmuch2$outstanding.1==1)]), 10)

sum(checkhowmuch2$outstanding.1, na.rm=T)

checkhowmuch[,10]
  diff(checkhowmuch$outstanding)>0

### new test

tail(test) # start 1.07815 end 1.08670
source('~/R/fritztest.R') # function fritztest2 returned fulldata nicht nur profit summary?
fritztest(test,'2016','2016',0.01,100000,1)
luk=fritztest2(test,'2016','2016',0.01,100000,1)
luk[which(luk$realized_profit!=0)]
abline(v = index(luk[which(luk$realized_profit!=0)]), col = 'blue', lty = 1, lwd = 1)
abline(v = index(luk[which(diff(luk$outstanding)<0)]), col = 'red', lty = 1, lwd = 1)

luk=fritztest2(test,'2016','2016',0.005,50000,1)
luk[which(luk$realized_profit!=0)]
sum(luk$realized_profit)

luk=fritztest2(test,'2016','2016',0.0025,25000,1)
luk[which(luk$realized_profit!=0)]
sum(luk$realized_profit)

luk=fritztest2(test,'2016','2016',0.0010,10000,1)
luk[which(luk$realized_profit!=0)]
sum(luk$realized_profit)

backtest=function(x) { fritztest3(test,'2016','2016',x[1],x[2],2) }
backtest2=function(x) { fritztest3(test,'2016','2016',0.01,100000,2) }
backtest3 = function(x) {
  checkhowmuch = fritztest2(test, '2015', '2016', x[1], x[2], 2)
  boot.1 <- tsboot(checkhowmuch$realized_profit,sum,R = 100,l = 3000,n.sim = 3000,sim = "fixed")
  return(mean(boot.1$t)) }

# do random block sampling 6 months! und dann wird die kleine einstellung besser sein...
asd=list(c(0.01,100000),c(0.0075,75000),c(0.0060,60000),c(0.0050,50000),c(0.0040,40000),c(0.0030,30000),c(0.0020,20000),c(0.0010,10000))
asd=list(c(0.01,100000),c(0.0075,75000))
xxx=lapply(asd,backtest)
# [[1]][1] 6972 [[2]][1] 5585 [[3]][1] 6764 [[4]][1] 5412 [[5]][1] 6084 [[6]][1] 6106 [[7]][1] 5868 [[8]][1] 4032
xxx=lapply(asd,backtest3)
# [[1]][1] 149.4 [[2]][1] 100.53 [[3]][1] 160.2 [[4]][1] 152.52 [[5]][1] 145.08 [[6]][1] 129.86 [[7]][1] 158.04 [[8]][1] 92.22
xxx

xxx2=do.call(cbind, xxx)
xxx2/(nrow(test)/3000)

require(boot)
boot.1 <- tsboot(test, backtest2, R = 10, l = 20000, sim = "fixed")
boot.1 # funktioniert nicht, bias 0
test=rnorm(10000)
boot.1 <- tsboot(test, mean, R = 10, l = 200, sim = "fixed")
str(boot.1)
boot.1$t
boot.1$l
boot.1[11]
nrow(test)

nrow(checkhowmuch)
sum(checkhowmuch$realized_profit)
boot.1 <- tsboot(checkhowmuch$realized_profit, sum, R = 100, l = 3000, n.sim=3000, sim = "fixed")
mean(boot.1$t)

plot(boot.1)
# require(tseries)
# tsbootstrap(test, nb = 10, statistic = backtest(c(0.01,100000)), m = 200000, type = c("block"))
# x is not a vector or univariate time series

# oder neu 2017
# http://eranraviv.com/bootstrapping-time-series-r-code/
# https://opensourcequant.wordpress.com/2016/04/26/block-bootstrapped-mc-function-for-backtest-results-in-r/

# https://tonybreyal.wordpress.com/2011/11/24/source_https-sourcing-an-r-script-from-github/
source_https('https://github.com/SamoPP/bootstrapTest/blob/master/bootstrap.R')

backtest=function(x) { fritztest3(test,'2016','2016',x[1],x[2],2) }

boot.1 <- boot(test, mean, R = 10, l = 200, sim = "fixed")

rint = round(runif(500)*365)

head(test)
myDates = as.Date("2015-08-10") + runif(10)*150
# cbind(as.Date(myDates),as.Date(myDates+120))
myDates+120
asd=as.data.frame(matrix(nrow=10,ncol=2,data=NA)) # matrix kann scheinbar kein date handeln??
asd[,1]=myDates
asd[,2]=myDates+120
asd


### ### TEST FOR VARIOUS STOCK PAIRS AND COMPARE TO REALIZED VOLATILITY

source('~/R/fritztest.R') # function fritztest2 returned fulldata nicht nur profit summary?
# Sys.setenv(TZ='Europe/Berlin')
# Sys.timezone()
# datahf=list(readRDS('~/Dropbox/data/universe/AAamin.rds')['2015'],readRDS('~/Dropbox/data/universe/AALamin.rds')['2015'],
#       readRDS('~/Dropbox/data/universe/AAPamin.rds')['2015'],readRDS('~/Dropbox/data/universe/AAPLamin.rds')['2015'],
#       readRDS('~/Dropbox/data/universe/ABBVamin.rds')['2015'],readRDS('~/Dropbox/data/universe/ABCamin.rds')['2015'],
#       readRDS('~/Dropbox/data/universe/ABTamin.rds')['2015'],readRDS('~/Dropbox/data/universe/ACNamin.rds')['2015'],
#       readRDS('~/Dropbox/data/universe/ADBEamin.rds')['2015'],readRDS('~/Dropbox/data/universe/ADIamin.rds')['2015'])
# maketimeright = function(x) { indexTZ(x)='EST5EDT'; x=x['T9:30/T16:00'] }
# datahf=lapply(datahf, maketimeright)
# datahf[[1]] IST IRGENDWIE ZU LANGSAM

require(xts)
datahf=list(readRDS('~/data/AAPLmincorrecttime.rds')['2015'],readRDS('~/data/ABCmincorrecttime.rds')['2015'],
            readRDS('~/data/ABTmincorrecttime.rds')['2015'],readRDS('~/data/ACGLmincorrecttime.rds')['2015'],
            readRDS('~/data/ACNmincorrecttime.rds')['2015'],readRDS('~/data/ADBEmincorrecttime.rds')['2015'],
            readRDS('~/data/ADPmincorrecttime.rds')['2015'],readRDS('~/data/AEEmincorrecttime.rds')['2015'],
            readRDS('~/data/AEPmincorrecttime.rds')['2015'],readRDS('~/data/AETmincorrecttime.rds')['2015'])
backtesting = function(x) { 
perc=(coredata(head(x,1))[,4]+coredata(head(x,1))[,8])/2*0.005
data=fritztest2(x,'2015','2015',perc,100000,2)
ret=sum(data$realized_profit != 0)
return(ret) 
}
backtesting2 = function(x) { 
  mid=(x[,4]+x[,8])/2
  fivt=na.omit(diff(log(to.minutes15(mid, OHLC=FALSE))))
  five=na.omit(diff(log(to.minutes5(mid, OHLC=FALSE))))
  hour=na.omit(diff(log(to.hourly(mid, OHLC=FALSE))))
  dail=na.omit(diff(log(apply.daily(mid, last))))
  #test=c(sd(fivt),sd(five),sd(hour),sum(fivt^2),sum(five^2),sum(hour^2))
  sd(dail)
}
plot(datahf[[10]])
lapply(datahf, backtesting2)
luk=fritztest2(test,'2016','2016',0.01,100000,1)
luk[which(luk$realized_profit!=0)]
abline(v = index(luk[which(luk$realized_profit!=0)]), col = 'blue', lty = 1, lwd = 1)

###

require(xts)
test=readRDS('~/Dropbox/data/old/EURCHFmin.rds')['2011-08/2011-09']
apply.daily(test, last)[,4]
40000*(1.01)^(52*6)
40000+400*52

###
source('~/R/fritztest.R') # function fritztest2 returned fulldata nicht nur profit summary?
USO=readRDS('~/Dropbox/data/USOamin.rds')
makecorrecttimepure('USOamin')

plot(USO)

USO[which(USO[,4]<8)]
tail(USO)
asd=fritztest(USO, '2016-01-20','2017', 0.50, 5000, 1)
require(quantmod)
getSymbols('USO')
index(asd)<-as.Date(index(asd))
F=na.omit(cbind(USO[,4],asd[,8]))

# plot(as.zoo(F[,1]), col='red', ylab='USO', main='USO vs. Maschine')
# mtext('from 2016-01-20 low')
# par(new = T)
# plot(as.zoo(F[,2]), yaxt='n', ylab='', col='blue', lty='dotted', lwd=2)
# axis(side = 4)
# legend('topleft', c('USO','Maschine'), cex=0.7, lty=c('solid','dotted'), col=c('red','blue'))

backtest=function(x) { fritztest(USOamin,'2016-01-20','2017',x[1],x[2],1) }
xx=rbind(seq(0.10,1,by=0.1),seq(1000,10000,by=1000))
asd=as.list(split(xx, col(xx)))
xxx=lapply(asd,backtest)

extract=function(x) { x[,8] }
xxxx=do.call(cbind, lapply(xxx, extract))
colnames(xxxx)=names(asd)
xxxxx=cbind(xxx[[1]][,1],xxxx)

# require(graphics) csphd(hcl(h = 120, c=c(100,150,200,300))) hcl(h, c, l, alpha)
require(RColorBrewer)
brewer.pal.info 
# colors()[142] # gold
# rgb2hsv( col2rgb( colors()[142] ) )
# rgb() This function creates colors corresponding to the given intensities  rgb(0,0,seq(1,10,1))
# https://www.nceas.ucsb.edu/~frazier/RSpatialGuides/colorPaletteCheatsheet.pdf

require(xts)
colorpackage='Spectral'
saveRDS(xxxxx, 'USOmaschine.rds')
xxxxx=readRDS('USOmaschine.rds')
plot.zoo(xxxxx[,2:11], plot.type = 'single', col=brewer.pal(10, colorpackage), ylab='', main='USO Maschine')
mtext(paste('from',index(first(xxxxx))))
legend('bottomright', as.character(xx[1,]), cex=0.7, lty=1, lwd=3, col=brewer.pal(10, colorpackage), ncol = 2)
par(new = T)
plot(as.zoo(xxxxx[,1]), yaxt='n', ylab='', col='blue', lty='dotted', lwd=1)
axis(side = 4)

require(PerformanceAnalytics)
SharpeRatio.annualized(na.omit(diff(log(xxxxx))))

### ### APRIL 2017 ### ###
source('~/R/fritztest.R')
### check je nach intervall die directional back-and-forths
checkhowmuch=fritztest2(test,'2015','2016',0.0015,100000,0)
sum(checkhowmuch$realized_profit != 0)/length(unique(as.Date(index(test))))

test=readRDS(gzcon(url("https://dl.dropboxusercontent.com/s/uo19beaq2lw0lii/EURCHF2016.rds"))) 
test=readRDS('~/Dropbox/data/universe/USOamin_2016.rds')
test=readRDS('~/Dropbox/data/universe/SPYamin.rds')

test=(test[,4]+test[,8])/2
range1=apply.daily(test, range)
range1
index(range1)=as.Date(index(range1))
colnames(range1)=c('low','high')
range1$perc=as.numeric(range1$high)/as.numeric(range1$low)
range1$perc=range1$perc-1

plot(range1$perc)
# test2=apply.daily((test[,4]+test[,8])/2, xts::last)
test2=apply.daily(test, xts::last)

nrow(test2)
plot(test2)
range2=(rollapply(test2, 90, max)/rollapply(test2, 90, min))-1 # range
index(range2)=as.Date(index(range2))
range2
range3=cbind(range2,rollapply(range1$perc, 90, sum, na.rm=T))
range3$ratio=range3[,2]/range3[,1]
range3
rollapply(range1$perc, 90, sum)

library('quantmod')
getSymbols("AAPL")
chartSeries(AAPL, subset='last 3 months')
chartSeries

require('PerformanceAnalytics')
# layout(matrix(c(1, 2, 3)), heights = c(2, 1, 1.3), widths = 1)
# layout(matrix(c(1, 2)), heights = c(2, 1), widths = 1)
# par(mar = c(1, 4, 4, 2))
par(mfrow=c(2,1)) 
plot(as.zoo(range3$ratio), ylab='', xlab='')
barplot(range3$ratio, col='lightgrey', names.arg=NULL, ylab=NULL, xlab=NULL, axes=FALSE, axisnames=FALSE)
axis(side = 4)
par(new = T)
plot(as.zoo(test2), ylab='', xlab='', col='red', lwd=3, main='SPY 2010/2011')

dev.off()
