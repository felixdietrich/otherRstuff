require(PerformanceAnalytics)
getSymbols('VXX')
getSymbols('SPY')
plot(VXX$VXX.Adjusted)
FF=VXX$VXX.Adjusted
GG=SPY$SPY.Adjusted
VXX['2016-11-25']
VXXm=apply.monthly(VXX, last)
VXXm=diff(log(VXXm))
mean(VXXm$VXX.Adjusted, na.rm=T)*12
FF=cbind(VXX$VXX.Adjusted,diff(log(VXX$VXX.Adjusted)))
head(FF)

lokok=na.omit(-diff(log(VXX$VXX.Adjusted)))
head(lokok)
lokok2=diff(log(SPY$SPY.Adjusted))['2009-02-02/']
### from PerformanceAnalytics package
BurkeRatio(lokok); BurkeRatio(lokok2)
ActiveReturn(lokok, lokok2, scale = 252)
AverageLength(lokok)
AverageDrawdown(lokok)
chart.Bar(lokok)
chart.Boxplot(lokok)
chart.Boxplot(lokok2)
chart.CumReturns(lokok)
chart.Drawdown(lokok)
chart.Drawdown(lokok2)
chart.ECDF(lokok)
chart.Events(lokok, '2010-01-04', prior = 12, post = 12)
chart.Histogram(lokok)
chart.QQPlot(lokok); chart.QQPlot(lokok2)
chart.Regression(lokok, lokok2)
chart.RelativePerformance(lokok, lokok2)
chart.RiskReturnScatter(lokok, lokok2)
chart.RollingMean(lokok, width=30)
chart.RollingPerformance(lokok, width = 12)
chart.SnailTrail(lokok, width = 252, stepsize = 252, add.names = F)
chart.SnailTrail(lokok, width = 252, stepsize = 252)
apply.yearly(lokok['/2016'], mean)*252
chart.StackedBar(lokok)
chart.VaRSensitivity(lokok)
plot(cumsum(lokok))
head(lokok['2010'])
data(edhec)
head(edhec)
plot(cumsum(edhec[,3]))
plot(cumsum(edhec$`Global Macro`))
findDrawdowns(lokok)
sortDrawdowns(findDrawdowns(lokok))
KellyRatio(lokok); KellyRatio(lokok2)
table.ProbOutPerformance(lokok, lokok2)
VolatilitySkewness(lokok)
###

vxxtest=cbind(diff(log(apply.monthly(GG, last))),diff(log(apply.monthly(FF, last))))
vxxtest[which(vxxtest[,1]<=0),]
vxxtest=cbind(diff(log(rollmean(GG, k=126))),diff(log(rollmean(FF, k=126))))
vxxtest[which(vxxtest[,1]<=0),]
tail(vxxtest[which(vxxtest[,1]<=0),], 200)
vxxtest=cbind(diff(log(apply.yearly(GG, last))),diff(log(apply.yearly(FF, last))))
vxxtest[which(vxxtest[,1]<=0.1),]

### USO vs. QM (first oil future mini)
getSymbols('USO')
OIL=Quandl('CHRIS/CME_CL1',type='xts')[,4]
cbind(4.5*USO[,4],OIL)['2016-12/']
# 450 USO short = 245 Margin * 4.5 = 1100
# QM 820 1640 overnight 

