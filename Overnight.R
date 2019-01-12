SPY <- readRDS('~/Dropbox/data/other/intlETF/minute/old/SPYmin.rds')
# SPY2 <- readRDS('~/Dropbox/data/universe/SPYamin.rds')
apply.daily(SPY, findgaps, 'highfreq2')
findgaps(SPY['2010-01'], 'highfreq2')
SPY['2010-01-28']
indexTZ(SPY)

QQQ <- readRDS('~/Dropbox/data/other/QQQmin.rds')
QQQ <- readRDS('~/Dropbox/data/universe/HYGamin.rds')
QQQ <- readRDS('~/Dropbox/data/universe/SPYamin.rds')

plot.zoo(apply.daily(QQQ, median))
head(QQQ)
indexTZ(QQQ)='EST5EDT' ### IMPORTANT
substr(index(QQQ['2004-12-01']), 12, 21)
asd <- unique(substr(index(QQQ), 12, 21))
sort(asd)
asd <- QQQ['T17:00/T17:15']

QQQ <- midpoint(QQQ)
plot.zoo(QQQ)

### new
mean(ret_overnight(QQQ), na.rm=T)*100
final <- cbind(apply.yearly(ret_overnight(QQQ), sum, na.rm=T),apply.yearly(ret_intraday(QQQ), sum, na.rm=T))
barplot(final, beside=TRUE)
plot.zoo(cumsum(final[,1]))

sum(ret_overnight(QQQ), na.rm=T)
sum(ret_intraday(QQQ), na.rm=T)
sum(ret_intraday(QQQ, y=c(9,33,10,0)), na.rm=T)
sum(ret_intraday(QQQ, y=c(10,0,15,58)), na.rm=T)
sum(ret_intraday(QQQ, y=c(10,0,11,0)), na.rm=T)
sum(ret_intraday(QQQ, y=c(11,0,12,0)), na.rm=T)
sum(ret_intraday(QQQ, y=c(12,0,13,0)), na.rm=T)
asd <- ret_intraday(QQQ, y=c(13,0,14,0))
asd <- ret_intraday(QQQ, y=c(14,0,15,0))
boxplot(as.numeric(asd))
sum(ret_intraday(QQQ, y=c(14,0,15,0)), na.rm=T)
sum(ret_intraday(QQQ, y=c(15,0,15,58)), na.rm=T)
###

QQQ1 <- split(QQQ, 'days')
QQQ1[[1]]
QQQ1b <- lapply(QQQ1, function(x) {
  a <- x[.indexhour(x)==9 & .indexmin(x)==35]
  b <- x[.indexhour(x)==15 & .indexmin(x)==55]
  c <- rbind(a,b)
  last(diff(log(c)))
})

QQQ2 <- rbind(QQQ[.indexhour(QQQ)==9 & .indexmin(QQQ)==34],QQQ[.indexhour(QQQ)==15 & .indexmin(QQQ)==56])
QQQ2b <- split(diff(log(QQQ2)), 'days')
QQQ2c <- lapply(QQQ2b, first)

Q1 <- na.omit(do.call(rbind, QQQ1b)) # 3 NA's
Q2 <- na.omit(do.call(rbind, QQQ2c)) # 1 NA
Q1 <- do.call(rbind, QQQ1b) 
Q2 <- do.call(rbind, QQQ2c) 
sum(Q1, na.rm = T)
sum(Q2, na.rm = T)

index(Q1) <- as.Date(index(Q1))
index(Q2) <- as.Date(index(Q2))

Q <- na.omit(cbind(Q1, Q2))
which.na.xts(Q)
QQ <- cumsum(Q)
plot.zoo(QQ, plot.type = 'single', col=c(1,2), xlab='', ylab='QQQ')

###
require(xts)
check <- readRDS('~/Dropbox/NYSE/SPY_listform.rds')
check <- readRDS('~/Dropbox/data/universe/SPYamin.rds')
head(check)
check1 <- readRDS('~/Dropbox/data/universe/EXPDaminOLD.rds')
check2 <- readRDS('~/Dropbox/data/universe/EXPDamin.rds')
check1['2015-03-12 18:00']
check1['2007-05-25 14:53:00']
check2['2015-03-12 18:00']
check1[duplicated(index(check1))]
# to download new: RES / UHS / V

# BNDaminOLD
check <- readRDS('~/Dropbox/data/universe/SXLamin.rds')
check <- readRDS('~/Dropbox/data/universe/GPORaminX.rds')

xx <- findgaps(check, 'daily')
asd <- c(readRDS('~/Dropbox/stellathecat/temp-20-49.rds'),readRDS('~/Dropbox/stellathecat/temp-22-32.rds'))
str(asd)
xx
head(asd)
listformbind
setwd('~/Dropbox/x/')
list.files()
lapply(list.files(), function(x) listformbind(readRDS(x)))
# March 2018
listformbind(readRDS('~/Dropbox/x/LLL_listform.rds'))
listformbind(readRDS('~/Dropbox/x/GNW_listform.rds'))

check1 <- readRDS('~/Dropbox/data/listform/AMZN_miau.rds')
check2 <- readRDS('~/Dropbox/AMZNamin.rds')
wheredifferent(check1,check2)
fcompare(check1,check2)
file.rename('~/Dropbox/data/listform/Fritz.rtf','~/Dropbox/data/listform/Fritz2.rtf')
lapply(grep('_miau',list.files('~/Dropbox/data/listform/'), value=TRUE), function(x) file.rename(x,gsub('_miau.rds','amin.rds',x)))

# gsub('amin.rds','',list.files('~/Dropbox/data/universe/')),1]

index(check1)[!index(check1) %in% index(check2)]
check2['2006-11-17 07:36:00']
fcompare(c(1,2),c(2,4))

nrow(check1)
all.equal(readRDS('~/Dropbox/data/listform/AMZN_miau.rds'),readRDS('~/Dropbox/AMZNamin.rds'), check.attributes=FALSE)

asd <- dataone(list(readRDS('~/Dropbox/data/universe/BNDamin.rds'),readRDS('~/Dropbox/data/universe/BNDaminOLD.rds')), saveas = '~/Dropbox/data/universe/BNDaminX.rds')
asd <- dataone(readRDS('~/Dropbox/data/listform/AMZN_listform.rds'), saveas = '~/Dropbox/data/listform/AMZNamin.rds')

asd <- dataone(c(readRDS('~/Dropbox/stellathecat/temp-20-49.rds'),readRDS('~/Dropbox/stellathecat/temp-22-32.rds'),
                    readRDS('~/Dropbox/stellathecat/temp-00-14.rds'),readRDS('~/Dropbox/stellathecat/temp-01-57.rds')), saveas = '~/Dropbox/stellathecat/temp123X.rds')

anyDuplicated(index(asd))

listformbind(check)
empty(where)
is.