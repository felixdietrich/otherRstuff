# https://www.quandl.com/data/UIFS-United-Nations-International-Financial-Statistics/documentation/data-organization
startQuandl()
Quandl("UIFS/INT_ARM", type='xts', collapse='monthly')
Quandl("UIFS/INT_US", type='xts', collapse='monthly')
Quandl("UIFS/FIDR_PA", type='xts', collapse='monthly')

# https://www.quandl.com/data/ODA-IMF-Cross-Country-Macroeconomic-Statistics/documentation/introduction

OIL <- Quandl("CHRIS/CME_CL1", type='xts', collapse='daily')[,'Settle']
plot.zoo(OIL['1985/1987'])
### FRBNY/ACM_M
# ACM Treasury Term Premia (Monthly)
# https://www.newyorkfed.org/medialibrary/media/research/data_indicators/ACMTermPremium.xls

### EUR/USD vs. 2-10 USD yield curve
require(Quandl)
us10yr <- Quandl("FRED/DGS10", type='xts')
us2yr <- Quandl("FRED/DGS2", type='xts')
EURUSD <- Quandl("CURRFX/USDEUR", type='xts')
# FYFSGDA188S Federal Surplus or Deficit [-] as Percent of Gross Domestic Product 
# DFII10 10-Year Treasury Inflation-Indexed Security, Constant Maturity
plot.zoo(us10yr)
 # FRED/T5YIFR # 5-Year, 5-Year Forward Inflation Expectation Rate


###
adv <- read.csv('~/trading/advancedecline/NYSE_advn.csv')
adv <- xts(adv[,2], as.Date(as.character(adv[,1]), format='%Y%m%d'))
decl <- read.csv('~/trading/advancedecline/NYSE_decln.csv')
decl <- xts(decl[,2], as.Date(as.character(decl[,1]), format='%Y%m%d'))
all <- cbind(rollapply(adv, 60, mean),rollapply(decl, 60, mean))
all$diff <- all[,1]-all[,2]
plot.zoo(all$diff)


require(quantmod)
getSymbols('^SPGSIN', src='google') # funktioniert nicht

getSymbols('SPBUYUP', src='google')
getSymbols('ISRATIO', src='FRED')
getSymbols('NEWORDER', src='FRED') # Manufacturers New Orders
# HSTCMDODNS # Deleveraging # Households and Nonprofit Organizations; Credit Market Instruments; Liability, Level
# Employed, Usually Work Full Time  (LNS12500000)  /  Total Population: All Ages including Armed Forces Overseas  (POP)
LIVEX <- Quandl('LIVEX/LVX_INV', type='xts') 
plot.zoo(LIVEX, main='LIVEX')
### 
Japan_Inflation <- getSymbols('FPCPITOTLZGJPN', src='FRED', auto.assign = FALSE)
US_Inflation <- getSymbols('CPIAUCSL', src='FRED', auto.assign = FALSE)
test <- cbind(Japan_Inflation,US_Inflation[index(US_Inflation) %in% index(Japan_Inflation)])
USDJPY <- getSymbols('AEXJPUS', src='FRED', auto.assign = FALSE)
test2 <- cbind(Japan_Inflation,diff(log(test[,2]))*100,USDJPY)['1977/']
test2$diff <- test2[,1]-test2[,2]
test2$sim <- as.numeric(test2[1,3])*cumprod(test2$diff/100+1)
test2$diff/100+1
plot.zoo(test2[,c('AEXJPUS','sim')], plot.type = 'single', col=c(1:2))

Swiss_Inflation <- getSymbols('FPCPITOTLZGCHE', src='FRED', auto.assign = FALSE)
US_Inflation <- getSymbols('CPIAUCSL', src='FRED', auto.assign = FALSE)
test <- cbind(Swiss_Inflation,US_Inflation[index(US_Inflation) %in% index(Swiss_Inflation)])['1971/']
USDCHF <- getSymbols('DEXSZUS', src='FRED', auto.assign = FALSE)
USDCHF <- apply.yearly(USDCHF, mean, na.rm=T)['1960/2016']
index(USDCHF) <- index(test)
test2 <- cbind(test[,1],diff(log(test[,2]))*100,USDCHF)['1975/']
test2$diff <- test2[,1]-test2[,2]
test2$sim <- as.numeric(test2[1,3])*cumprod(test2$diff/100+1)
test2$diff/100+1
test2
plot.zoo(test2[,c('DEXSZUS','sim')], plot.type = 'single', col=c(1:2))


### NEW AUGUST 
xx = cbind(Quandl('CHRIS/EUREX_FGBL1', type='xts')[,4],Quandl('CHRIS/EUREX_FGBL2', type='xts')[,4])
plot.zoo(xx[,c(1:2)], plot.type='single', ylab='', main='GBL')
xx$diff = xx[,2]-xx[,1]
xx$perc = round(xx[,2]/xx[,1]-1, 4)
xx$log = round((log(xx[,2])-log(xx[,1])), 4) # *100
# xx[diff(abs(xx$diff))>0.1 & (xx[,1]-lag(xx[,2]))<0.1]
xx['2017']
xxx <- as.Date(c('2015-03-06','2015-06-08','2015-09-08','2015-12-08','2016-03-08','2016-06-08','2016-09-08','2016-12-08','2017-03-08','2017-06-08'))
xxxx = split(xx, cut(index(xx), xxx))
xxxx = lapply(xxxx, na.omit)
xxxxx = lapply(xxxx, function(x) xts(round(cumsum(na.omit(diff(x$diff))), 3)))
for (i in c(1:9)) plot.zoo(xxxxx[[i]]) # hmmm
### END NEW AUGUST

require(Quandl) #; require(gdata)
Quandl.api_key('dyYHhrVEtyv9u_Hm7FhN')
Quandl.search(query = 'Switzerland current account')
Quandl('SGE/CHECA', type='xts') # PREMIUM DATA FROM Trading Economics

sw1=Quandl('SNB/TAB_UEB_M1', type='xts') # Overview of The Swiss Balance of Payments - Current Account
sw1=Quandl('SNB/BOPSERVQ', type='xts') # Swiss Balance of Payments – Current Account Services, by Country – Quarter
# SNB/BOPCURRQ Swiss Balance of Payments – Current Account – Quarter
# 
sw1=Quandl('SNB/Q1_M1_Q', type='xts') # Latest: 2015-01-31
# sw2=Quandl('SNB/BBILSUM_D01_M1', type='xts') #  Balance Sheet Totals ALL BANKS
tail(sw1)
plot(sw2['2000/'])
head(sw2)

sw1=read.xls('~/R/snb-data-bopoverq.xlsx', skip=1)
date=as.Date(as.yearqtr(sw1$Component, format="%Y-Q%q"))
sw1=as.xts(sw1[,2:ncol(sw1)], date)
plot(sw1$Current.account...Net)
plot(cumsum(sw1$Current.account...Net))
plot(sw1$Capital.account...Net)
plot(sw1$Financial.account..excluding.derivatives....Net)

plot(sw1$Financial.account..excluding.derivatives....Reserve.assets...Net)
plot(cumsum(sw1$Financial.account..excluding.derivatives....Reserve.assets...Net))

plot(cumsum(sw1$Financial.account..excluding.derivatives....Net))
FF=cumsum(sw1$Financial.account..excluding.derivatives....Net)-cumsum(sw1$Financial.account..excluding.derivatives....Reserve.assets...Net)
plot(FF)

plot(cumsum(sw1$Financial.account..excluding.derivatives....Net.acquisition.of.financial.assets))
plot(cumsum(sw1$Financial.account..excluding.derivatives....Net.incurrence.of.liabilities))

plot(cumsum(sw1$Financial.account..excluding.derivatives....Direct.investment...Net))

plot(cumsum(sw1$Financial.account..excluding.derivatives....Portfolio.investment...Net))

plot(cumsum(sw1$Financial.account..excluding.derivatives....Other.investment...Net))
plot(cumsum(sw1$Financial.account..excluding.derivatives....Other.investment...Net.acquisition.of.financial.assets))
plot(cumsum(sw1$Financial.account..excluding.derivatives....Other.investment...Net.incurrence.of.liabilities))

plot(cumsum(sw1$Statistical.difference))

yearqtr()
head(sw1)
 