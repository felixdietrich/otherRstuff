setwd('/Users/fd/Dropbox/jan/')
EURUSD2012 = readRDS('EURUSD2012clean.rds')

test=EURUSD2012['2012-08']

intradayvol=function(x)
{ 
  sum(diff(log(x[,4]))^2, na.rm=T)*10000
}

apply.daily(test, intradayvol)

########### THEORETISCHE IDEE WARUM: high INTRADAY REALIZED VOL vs. low SD ###########  
process1=rep(c(1,2),20)
process2=rep(c(1,1,2,2),10)
process1=c(1,3,1,3,2,4,2,4,5,7,5,7,6,8,6,8,10,9,10,9)
process2=c(1,1,3,3,2,2,4,4,5,5,7,7,6,6,8,8,10,10,9,9)
plot(process1, type='l')
plot(process2, type='l')
skewness(process1)
sd(process1)
sd(process2)
process1b=diff(log(process1))
process2b=diff(log(process2))
process1c=sum(process1b^2)
process2c=sum(process2b^2)
# realized vol higher!!!

### 2017: MACHE HIER EIN BEISPIEL FUER REALIZED VOL SAME
process3=rep(c(1,2),each=20)
plot(process3)

source('~/Dropbox/otherRstuff/directionalchange.R')
interval <- 1
# asd <- martingale(transform_to_4(process[[1]]), increment_position = interval, increment_profit = interval, betsize = 100, trade_com = 0, version='all', plot='')

unlist(lapply(test, directionalchanges, interval=0.2))
process <- lapply(c(1:100), function(x) sample(c(1,1,3,3,2,2,4,4,5,5,7,7,6,6,8,8,10,10,9,9)))
asd <- cbind( unlist( lapply(process, sd) ),
              unlist( lapply(process, function(x) sum(diff(log(x))^2)) ),
              unlist( lapply(process, function(x) directionalchanges(transform_to_4(x), interval=1)) ) )
rownames(asd) <- unlist(lapply(process, function(x) paste(x, collapse = " ")))

asd[order(asd[,3]),]

########### SIMULATED RANDOM WALK ########### 
set.seed(1)
rnorm(20)

n <- 1000
for(i in 1:100) 
{
  x <- cumsum(sample(c(-1, 1), n, TRUE))
  print(min(x))
}
