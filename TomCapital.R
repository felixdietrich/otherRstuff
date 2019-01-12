
### TOM CAPITAL https://www.tomcapital.ch/solutions/

getSymbols('^SSMI', from='2005-01-01')
SMI=SSMI[,4]
plot(SMI['2007/'])
lines(SMA(SMI, n=50), col='blue', lty='dotted', lwd=3)
plot(SMI['2007/'])
lines(EMA(SMI, n=50), col='blue', lty='dotted', lwd=3)

plot(SMI['2007/'], main='Tom Capital')
lines(BBands(SMI, n=100)[,2], col='blue', lty='dotted', lwd=2)
lines(BBands(SMI, n=100)[,1], col='darkgreen', lty='solid', lwd=2)
lines(BBands(SMI, n=100)[,3], col='red', lty='solid', lwd=2)
head(BBands(SMI)['2007/'])
head(SMI)
require(TTR)
getSymbols('^TNX', from='2005-01-01')
plot(TNX)

TNF=Quandl('CME/TYU2016',type='xts')
TNF=Quandl('SCF/CME_TY1_EN',type='xts') # Continuous CBOT 10-year US Treasury Note Futures #1 (TY1) bis 2002
TNF=Quandl('CHRIS/CME_TY2',type='xts')
plot(TNF['2014-2/2016-02']) # price series

TNFb=Quandl('CFTC/TY_F_L_ALL',type='xts') # Commitment of Traders - 10-Year U.S. Treasury Notes Weekly, since 1993
TNFb=Quandl('CFTC/TIFF_CBOT_US_ALL',type='xts') # Positions in the U.s. Treasury Bonds (TIFF) Weekly, since 2014
TNFa=TNFb[,2]-TNFb[,3]
TNFa=TNFb[,5]-TNFb[,6]
TNFa=TNFb[,7]-TNFb[,8]
### SECOND DATA
TNFa=TNFb[,8]-TNFb[,9] # leveraged long
head(cbind(TNFb[,8],TNFb[,9]))

plot(TNFa['2013-2/2016/02'])
tail(TNFb['2014-2/2016/02'])
TNFa['2014-2/2016/02']
rm(SMI)

AUDpositioning=Quandl('CFTC/AD_F_L_ALL',type='xts')
head(AUDpositioning)
AUDnet=AUDpositioning[,2]-AUDpositioning[,3]
plot(AUDnet['2007/'])
AUD=1/getSymbols('AUD=X', auto.assign = F)[,4]
AUD2=EMA(AUD, n=100); plot(AUD); lines(AUD2, col='red', lty='dotted')

AUDnet2=Delt(AUDnet, x2 = NULL, k = 100)
AUDnet2=EMA(AUDnet, n=100); plot(AUDnet); lines(AUDnet2, col='red', lty='dotted')

library(latticeExtra)
aa=xyplot(AUD)
#bb=xyplot(AUDnet)
bb=xyplot(AUDnet2)
doubleYScale(aa,bb, text = c('AUDUSD','AUDUSD CFTC Speculator Positioning'))

### ### ### ### ### highfrequency package FOR TAQ ### ### ### ### ### 

xx=read.csv2('~/Downloads/9603f0b8776e9cd8.txt') # TAQ Energy
data <- read.table("~/Downloads/9603f0b8776e9cd8_txt.zip", nrows=100, header=T, quote="\"", sep=",")
data <- read.table("~/Downloads/9603f0b8776e9cd8_txt.zip")
data <- read.csv2("~/Downloads/9603f0b8776e9cd8_txt.zip")

require(highfrequency)
require(TAQMNGR)
data("sample_qdata")
sample_qdata
aggregateQuotes(sample_qdata,on="seconds",k=30)

### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### 
