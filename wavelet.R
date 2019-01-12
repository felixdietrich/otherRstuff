require(wavelets)
getFX('EUR/USD', from = Sys.Date() - 179)
EUR.USD <- na.omit(readRDS('~/Dropbox/data/currencies/EUR.USD.rds'))
EUR.USD <- EUR.USD['2015/2017']
EUR.USD <- to.hourly(EUR.USD, OHLC=FALSE)
EUR.USD <- EUR.USD['2016-01/2016-06']
EUR.USD <- (EUR.USD[,4]+EUR.USD[,8])/2
plot.zoo(EUR.USD)

require(wavelets)
# see page 3 wavelets.pdf :
# obtain the two series listed in Percival and Walden (2000), page 42
X1 <- c(.2,-.4,-.6,-.5,-.8,-.4,-.9,0,-.2,.1,-.1,.1,.7,.9,0,.3)
X2 <- c(.2,-.4,-.6,-.5,-.8,-.4,-.9,0,-.2,.1,-.1,.1,-.7,.9,0,.3)
# combine them and compute DWT
newX <- cbind(X1,X2)
wt <- dwt(newX, n.levels=3, boundary="reflection", fast=FALSE)
wt

# plot.dwt example in page 24 wavelets.pdf :
X <- rnorm(2048)
dwtobj <- dwt(X)
str(dwtobj)
dwtobj@V
# in @W und @V halbieren sich die frequencys immer...
plot(dwtobj)
# Plotting wavelet coefficients of levels 1 through 6 and scaling
# coefficients of level 6.
plot.dwt(dwtobj, levels = 6)

dwtobj <- dwt(as.numeric(EUR.USD))
pdf('wavelet_EURUSD.pdf', width=14, height=20)
plot.dwt(dwtobj, levels = 6)
dev.off()

### 
source('~/Dropbox/newdiss/git/functions_plot.R')
test <- rollapply(EUR.USD, 3*252, function(x) range(x)[2]-range(x)[1])
doubleplot(EUR.USD,test, align=FALSE)
plot.zoo(EUR.USD)
par(new=T)
plot.zoo(test, col='red')
