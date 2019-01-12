head(currencyba) # welches file ist das?
currencyba['2005-12-26']
sort(c(as.Date(holiday(2005:2015, Holiday = listHolidays('US'))),as.Date(holiday(2005:2015, "ChristmasEve")),as.Date(DENewYearsEve(2005:2015))))
#EURUSD=readRDS('~/Dropbox/data/EURUSDmincorrecttime.rds') # nicht US time!!! MINcorrecttime
EURUSD=EURUSD[,8]-EURUSD[,4]
EURUSD['2013-12-26']

### EST 26 DEZ and JOB DATES ----
currencyba['2013-12-26']
options(scipen = 999)
currencybaa=currencybaperc[!as.Date(index(currencybaperc)) %in% seq(as.Date("2005/12/26"), by = "years", length.out = 11)]
currencybab=currencybaperc[as.Date(index(currencybaperc)) %in% seq(as.Date("2005/12/26"), by = "years", length.out = 11)]
EURUSDa=currencyba$EURUSD[!as.Date(index(currencyba)) %in% c(as.Date('2015-01-09'),as.Date('2015-02-06'),as.Date('2015-03-06'),as.Date('2015-04-03'),as.Date('2015-05-08'),as.Date('2015-06-05'),as.Date('2015-07-02'),as.Date('2015-08-07'),as.Date('2015-09-04'),as.Date('2015-10-02'),as.Date('2015-11-06'),as.Date('2015-12-04'))]
EURUSDb=currencyba$EURUSD[as.Date(index(currencyba)) %in% seq(as.Date("2005/12/26"), by = "years", length.out = 11)]
boxplot(coredata(currencybaa), main='no December 26th', las = 2)
boxplot(coredata(currencybab), main='December 26th', las = 2)
boxplot(coredata(EURUSDb), main='December 26th', las = 2, xlab='EURUSD')

EURUSDa=currencyba$EURUSD[!as.Date(index(currencyba)) %in% c(as.Date('2015-01-09'),as.Date('2015-02-06'),as.Date('2015-03-06'),as.Date('2015-04-03'),as.Date('2015-05-08'),as.Date('2015-06-05'),as.Date('2015-07-02'),as.Date('2015-08-07'),as.Date('2015-09-04'),as.Date('2015-10-02'),as.Date('2015-11-06'),as.Date('2015-12-04'))]
EURUSDc=currencyba$EURUSD[as.Date(index(currencyba)) %in% c(as.Date('2015-01-09'),as.Date('2015-02-06'),as.Date('2015-03-06'),as.Date('2015-04-03'),as.Date('2015-05-08'),as.Date('2015-06-05'),as.Date('2015-07-02'),as.Date('2015-08-07'),as.Date('2015-09-04'),as.Date('2015-10-02'),as.Date('2015-11-06'),as.Date('2015-12-04'))]
boxplot(coredata(EURUSDa), main='Job Dates', las = 2, xlab='EURUSD')
boxplot(coredata(EURUSDc), main='Job Dates', las = 2, xlab='EURUSD')

sum(abs(diff(EURUSDc['2015-01-09']))^2, na.rm=T)*10000000
sum(abs(diff(EURUSDa['2015-01-08']))^2, na.rm=T)*10000000
sum(abs(diff(EURUSDa['2015-01-06']))^2, na.rm=T)*10000000
plot(EURUSDa)
plot(EURUSDc['2015-01-09'])
plot(EURUSDa['2015-01-06'])

### TEST JAN REALIZED LIQ VARIATION ----
EURUSDa['2015-01-06']
weekdays(ymd(20150108))
Jan=Jan[,c(1,2,3,4,9,10,11,12)]
Jan=Jan[,8]-Jan[,4]
head(Jan)
plot(Jan['2016-08-01/2016-08-05'], main='2016-08-01 - 2016-08-05')
dev.off()
Jan2=Jan['T10:00/T22:00']
head(Jan2, 200) 
plot(Jan2['2016-08-01'])
barplot(c(sum(abs(diff(Jan['2016-08-01']))^2, na.rm=T)*10000000,
       sum(abs(diff(Jan['2016-08-02']))^2, na.rm=T)*10000000,
       sum(abs(diff(Jan['2016-08-03']))^2, na.rm=T)*10000000,
       sum(abs(diff(Jan['2016-08-04']))^2, na.rm=T)*10000000,
       sum(abs(diff(Jan['2016-08-05']))^2, na.rm=T)*10000000), ylab='', names.arg=c('1','2','3','4','5'), main='Realized Liquidity Variation (The Thora Measure)')

saveRDS(Jan, 'EURUSDAugust2016Jan.rds')

Jan2=readRDS('~/EURUSDAugust2016JanSecond.rds')
Jan2=Jan2[,c(1,2,3,4,9,10,11,12)]
Jan2=Jan2[,8]-Jan2[,4]
barplot(c(sum(abs(diff(Jan2['2016-08-01']))^2, na.rm=T)*10000000,
          sum(abs(diff(Jan2['2016-08-02']))^2, na.rm=T)*10000000,
          sum(abs(diff(Jan2['2016-08-03']))^2, na.rm=T)*10000000,
          sum(abs(diff(Jan2['2016-08-04']))^2, na.rm=T)*10000000,
          sum(abs(diff(Jan2['2016-08-05']))^2, na.rm=T)*10000000), ylab='', names.arg=c('1','2','3','4','5'), main='Realized Liquidity Variation (Second Data)')
plot(Jan2['2016-08-01/2016-08-05'], main='2016-08-01 - 2016-08-05')
plot(Jan2['2016-08-05'], main='2016-08-05 Job Report - Second Data')
head(Jan2['2016-08-04'], 100)