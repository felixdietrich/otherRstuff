SPY=readRDS('SPY2016.rds')
SPX=readRDS('SPX2016.rds')
head(SPY)
# SPY http://www.nasdaq.com/de/symbol/spy/dividend-history
# Ex-Dividenden-Datum	Art	Barbetrag	Deklarationsdatum	Festlegungsdatum	Ausschüttungsdatum
# 9/16/2016	Cash	1.082068	9/15/2016	9/20/2016	10/31/2016
# 6/17/2016	Cash	1.078442	6/16/2016	6/21/2016	6/27/2016
# 3/18/2016	Cash	1.049604	3/17/2016	3/22/2016	4/29/2016
# 12/18/2015	Cash	1.21155	12/17/2015	12/22/2015	1/29/2016
SPYSPX=na.omit(cbind(SPY[,4],SPX[,4]))
indexTZ(SPYSPX)='EST5EDT'
plot(as.zoo(SPYSPX[,1]['2015-12-16/2015-12-18']), las=1, xlab="", ylab='', main='Ex-Dividend')
par(new=TRUE)      
plot(as.zoo(SPYSPX[,2]['2015-12-16/2015-12-18']), col=3, bty='n', xaxt="n", yaxt="n", xlab="", ylab="")
axis(side = 4)
legend('bottomleft', c('SPY','SPX'), lty=c('solid','solid'), lwd=2, col=c(1,3), cex=0.8)
weekdays(ymd(20151217)) 

plot(as.zoo(SPYSPX[,1]['2016-03-16/2016-03-18']), las=1, xlab="", ylab='', main='Ex-Dividend')
par(new=TRUE)      
plot(as.zoo(SPYSPX[,2]['2016-03-16/2016-03-18']), col=3, bty='n', xaxt="n", yaxt="n", xlab="", ylab="")
axis(side = 4)
legend('bottomright', c('SPY','SPX'), lty=c('solid','solid'), lwd=2, col=c(1,3), cex=0.8)

