library(Quandl)
library(ggplot2)
load('~/Dropbox/data/CLDataQuandl.RData')
Quandl.api_key('dyYHhrVEtyv9u_Hm7FhN')
R.home()
R.Version()

### new 2017
CL12=Quandl('CHRIS/CME_CL12',type='xts')[,6] # gibt sogar 13 
CL11=Quandl('CHRIS/CME_CL11',type='xts')[,6]
CL10=Quandl('CHRIS/CME_CL10',type='xts')[,6]
CL9=Quandl('CHRIS/CME_CL9',type='xts')[,6]
CL8=Quandl('CHRIS/CME_CL8',type='xts')[,6]
CL7=Quandl('CHRIS/CME_CL7',type='xts')[,6]
CL6=Quandl('CHRIS/CME_CL6',type='xts')[,6]
CL5=Quandl('CHRIS/CME_CL5',type='xts')[,6]
CL4=Quandl('CHRIS/CME_CL4',type='xts')[,6]
CL3=Quandl('CHRIS/CME_CL3',type='xts')[,6]
CL2=Quandl('CHRIS/CME_CL2',type='xts')[,6]
#CL2=Quandl('CHRIS/CME_CL2',type='xts')
CL1=Quandl('CHRIS/CME_CL1',type='xts')[,6]
#CL1=Quandl('CHRIS/CME_CL1',type='xts')

term=cbind(CL1-CL2,CL2-CL3,CL3-CL4,CL4-CL5,CL5-CL6,CL6-CL7,CL7-CL8,CL8-CL9,CL9-CL10,CL10-CL11,CL11-CL12)
CL=cbind(CL1,CL2,CL3,CL2-CL1,CL3-CL2)
CLL=cbind(CL9-CL10,CL10-CL11,CL11-CL12)
apply.weekly(CLL['2014/'], last)
apply.weekly(term['2014/'], last)
plot(term['2010/'][,11])
head(CL,100)
Quandl.search(query = 'oil settlement') 

### ###

cont=log(CL1)-log(CL9)
cont=CL1-CL12
cont=CL1-CL6
cont=CL1-CL3
# plot(CL1)
# plot(cont, type='l')
# plot(rollmean(cont, 30))
CL=na.omit(cbind(CL1,rollmean(cont, 30)))
plot1=CL[,1]
plot2=CL[,2]
colnames(plot2)='fritz'

k=0.001 # for percentages
k=0.1 # for absolute dollar
Date=index(plot1)
ggplot(plot1, aes(x=Date, y=Settle)) + geom_line(color='blue', lwd=1) +
  scale_y_continuous('First Oil Future', sec.axis = sec_axis(~ . * k, name = 'Contango / Backwardation')) +
  geom_area(data=plot2, aes(y=fritz/k, fill=fritz>0)) + ggtitle("3 Month Contango") +
  scale_fill_manual(values=c(adjustcolor("red", alpha.f = 0.5),
                             adjustcolor("green", alpha.f = 0.5)), guide=FALSE)

lapply(CL1986, last)
### end new 2017

CL1983=list(
  CLM1983=Quandl('CME/CLM1983',type='xts'),
  CLN1983=Quandl('CME/CLN1983',type='xts'),CLQ1983=Quandl('CME/CLQ1983',type='xts'),
  CLU1983=Quandl('CME/CLU1983',type='xts'),CLV1983=Quandl('CME/CLV1983',type='xts'),
  CLX1983=Quandl('CME/CLX1983',type='xts'),CLZ1983=Quandl('CME/CLZ1983',type='xts'))

Quandl(c('CME/CLF1986','CME/CLG1986'),type='xts') # returned leider kombiniert

for (i in 1986:1987)
{ print(paste('CME/CLF',i,sep='')) }
lala = function(x) { c(paste('CME/CLF', x, sep = ''), paste('CME/CLG', x, sep = ''),
                     paste('CME/CLH', x, sep = ''), paste('CME/CLJ', x, sep = ''),
                     paste('CME/CLK', x, sep = ''), paste('CME/CLM', x, sep = ''),
                     paste('CME/CLN', x, sep = ''), paste('CME/CLQ', x, sep = ''),
                     paste('CME/CLU', x, sep = ''), paste('CME/CLV', x, sep = ''),
                     paste('CME/CLX', x, sep = ''), paste('CME/CLZ', x, sep = '')) }
xx=c(1984:2016)
xxx=lapply(xx, lala)
xxx2=unlist(xxx)
xxx2
lala2 = function(x) { index(tail(Quandl(x,type='xts'),1)) } # index gibt zahlen
# xxx3=unlist(lapply(xxx2, lala2))
xxx3=lapply(xxx2, lala2)
xxx4=as.Date(unlist(xxx3))
saveRDS(xxx4, 'CLrolldates.rds')

xxx5=xts(seq(1:length(xxx4)), order.by=as.Date(xxx4))
CLf=cbind(CL,xxx5)
CLx=CLf['2016-08/']
CLx[xxx4]
weekdays(ymd(20160222))
weekdays(ymd(20160420))
CLx
CLx[xxx4-1] # das problem ist DATES, vielleicht erst auf index konvertieren? oder mit na.locf ueber all dates
CLx[xxx4+1]
plot(CLx[,3])

CL1986=list(
  CLF1986=Quandl('CME/CLF1986',type='xts'),CLG1986=Quandl('CME/CLG1986',type='xts'),
  CLH1986=Quandl('CME/CLH1986',type='xts'),CLJ1986=Quandl('CME/CLJ1986',type='xts'),
  CLK1986=Quandl('CME/CLK1986',type='xts'),CLM1986=Quandl('CME/CLM1986',type='xts'),
  CLN1986=Quandl('CME/CLN1986',type='xts'),CLQ1986=Quandl('CME/CLQ1986',type='xts'),
  CLU1986=Quandl('CME/CLU1986',type='xts'),CLV1986=Quandl('CME/CLV1986',type='xts'),
  CLX1986=Quandl('CME/CLX1986',type='xts'),CLZ1986=Quandl('CME/CLZ1986',type='xts'))

arima.sim(model=as.list(coef(model_AR)), n=100)
ts_AR <- arima.sim(n=100, list(ar=c(0.5), sd = sqrt(0.01)))
plot(ts_AR)

### woher kommt CLintraday?
spread=(CLintraday20151215[,1]+CLintraday20151215[,2])/2
plot(spread)
nrow(spread)
sss=arima(spread, order = c(3,0,3), seasonal=list(order = c(2,0,1)), include.mean = T)
sss=arima(spread, order = c(30,0,30), include.mean = T)
sss
plot(arima.sim(model=as.list(coef(sss)), n=100))
as.list(coef(sss))
sss2=ar(spread, order.max = 5)
sss2=ar.ols(spread, order=30, demean=F, intercept=T) 
plot(arima.sim(model=as.list(coef(sss2)), n=100))
sss2

plot(arima.sim(list(order=c(1,0,0), ar=.8), n=100, sd=sd(spread)) + mean(spread))
plot(arima.sim(model=as.list(coef(sss2)), n=1300, sd=sd(spread)) + mean(spread)) # SD braucht man
plot(arima.sim(model=as.list(coef(sss)), n=1300, sd=sd(spread)) + mean(spread)) # SD braucht man


arima.sim(model=as.list(coef(sss2)), n=100, rand.gen = rnorm(n=100, mean=mean(spread), sd=sd(spread)))
plot()
require('FitARMA')
FitARMA(spread)


