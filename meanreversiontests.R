IWN_holdings <- read.csv('~/Dropbox/data/maschineexample/IWN_holdings.csv', skip=10, stringsAsFactors = FALSE)
IWN_holdings <- IWN_holdings[IWN_holdings$Weight....>0.33,]
IWN_holdings
asd <- getCombine(IWN_holdings$Ticker, src='yahoo', type='adjusted')
asd$IWN <- getSymbols('IWN', src='yahoo', auto.assign = F)[,6]
ase <- normalize(asd,100)

spreads <- lapply(c(1:(ncol(ase)-1)), function(x) ase[,x]-ase$IWN)
names(spreads) <- colnames(ase)[1:30]

for (i in c(1:30)) { try(plot.zoo(spreads[[i]])) } # WIESO GEHT DAS NICHT

lapply(spreads, function(x) try({ plot.zoo(x, xlab='', ylab='', main=colnames(x)); Sys.sleep(5) }))
lapply(spreads, function(x) unique(is.na(x)))
plot.zoo(spreads[[5]])

### STATISTICS
fr1=round(max(data_spread, na.rm=T)-min(data_spread, na.rm=T), digits=1)
fr2=round(sd(data_spread, na.rm=T), digits=1)
fr3=round(sd(apply.monthly(data_spread, last), na.rm=T), digits=1)
fr4=round(sd(apply.weekly(data_spread, last), na.rm=T), digits=1)
fr5=round(mean(data_spread, na.rm=T), digits=1)
reg=lm(maunz1()[,4]~maunz1()[,12]+0)
coRes=ca.jo(data.frame(maunz1()[,4],maunz1()[,12]),type="trace",K=10,ecdet="none", spec="longrun")
fr6=round(adf.test(as.numeric(reg$residuals))$p.value, digits=2)
fr7=round(coRes@teststat[2], digits=2)
# add VARIANCE RATIOS etc.
updn <- c(diff(sign(data_spread[2:nrow(data_spread),4])))
fr8 <- length(which(updn != 0))
table=cbind(rbind(fr1,fr2,fr3,fr4,fr5,fr6,fr7,fr8),rbind('','','','','','!<0.1','!>15.6','!>0'))
rownames(table)=c('range','sd','sd_w','sd_m','mean','ADF','Coint','0-X')

### 
test <- readRDS('~/Dropbox/stellathecat/IB_FX_OHLC.rds')['EUR.CHF'][[1]]

