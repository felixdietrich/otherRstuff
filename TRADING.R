VIX <- readRDS('~/Dropbox/newdiss/files/VIX-newcombined.rds')
quantmod::getSymbols("^GSPC",from="1896-01-01",to=Sys.Date())
plot.zoo(GSPC[,4])
# spx <- read.csv('~/Dropbox/stellathecat/vladi/SPX_data.csv', stringsAsFactors = F)
# spx <- spx[,c('date','days','delta','impl_volatility','impl_premium')]
head(spx)

data <- na.omit(cbind(VIX, GSPC[,4]))
data

# 