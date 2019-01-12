### CHECK CERTAIN HOURS AND FIND WRAMPELMEYERS PAPER VALUE ---- 
AUDUSD=readRDS('~/Dropbox/data/UStime old?/AUDUSDminustime.rds')
AUDUSD=AUDUSD['2008-10']
head(AUDUSD)
AUDUSDspread=AUDUSD[,8]-AUDUSD[,4]

cbind(max(AUDUSDspread['T00:00/T01:00'],na.rm=T),max(AUDUSDspread['T01:00/T02:00'],na.rm=T),
      max(AUDUSDspread['T02:00/T03:00'],na.rm=T),max(AUDUSDspread['T03:00/T04:00'],na.rm=T),
      max(AUDUSDspread['T04:00/T05:00'],na.rm=T),max(AUDUSDspread['T05:00/T06:00'],na.rm=T),
      max(AUDUSDspread['T06:00/T07:00'],na.rm=T),max(AUDUSDspread['T07:00/T08:00'],na.rm=T),
      max(AUDUSDspread['T08:00/T09:00'],na.rm=T),max(AUDUSDspread['T09:00/T10:00'],na.rm=T),
      max(AUDUSDspread['T10:00/T11:00'],na.rm=T),max(AUDUSDspread['T11:00/T12:00'],na.rm=T),
      max(AUDUSDspread['T12:00/T13:00'],na.rm=T),max(AUDUSDspread['T13:00/T14:00'],na.rm=T),
      max(AUDUSDspread['T14:00/T15:00'],na.rm=T),max(AUDUSDspread['T15:00/T16:00'],na.rm=T),
      max(AUDUSDspread['T16:00/T17:00'],na.rm=T),max(AUDUSDspread['T17:00/T18:00'],na.rm=T),
      max(AUDUSDspread['T18:00/T19:00'],na.rm=T),max(AUDUSDspread['T19:00/T20:00'],na.rm=T),
      max(AUDUSDspread['T20:00/T21:00'],na.rm=T),max(AUDUSDspread['T21:00/T22:00'],na.rm=T),
      max(AUDUSDspread['T22:00/T23:00'],na.rm=T),max(AUDUSDspread['T23:00/T00:00'],na.rm=T))

AUDUSDspread[which(AUDUSDspread>=0.0050)]*100 # 0.54 === 54 bps
AUDUSD[which(AUDUSDspread>=0.0050)] # 0.54 === 54 bps

AUDUSDspreadperc=(AUDUSD[,8]-AUDUSD[,4])/(AUDUSD[,8]+AUDUSD[,4])
weekdays(as.Date('2008-10-13'))
test <- AUDUSDspreadperc['2008-10-10/2008-10-13']
testx <- x['T9:30/T16:00'] ### FUNKTIONIERT HIER WIEDER NICHT
Sys.timezone()
indexTZ(AUDUSDspreadperc)
head(testx)
pdf('Wramp.pdf')
plot_wo_weekend(test, main='AUD/USD Liquidity', mtext='2008-10-10 / 2008-10-13 US time')
# which(z0>0.004)
points(1020,z0[1020], col='red')
text(1020,y=par("usr")[4]*0.95,"JoF value", pos=4, cex=1)
dev.off()
### END new ###

as.Date(index(AUDUSDspread[which(AUDUSDspread>=0.0050)]))
weekdays(as.Date(ymd(20081010)))
AUDUSDspread[as.Date(index(AUDUSDspread)) %in% as.Date(index(AUDUSDspread[which(AUDUSDspread>=0.0050)]))] # check dates of outliers
indexTZ(AUDUSDspread)='EST5EDT'
temp=as.data.frame(DBVmin[DBVmin[index(outl), which.i=TRUE]]) 
Sys.timezone()