source('functions.R')
startQuandl()
what <- paste0(c('CME/CLF','CME/CLG','CME/CLH','CME/CLJ','CME/CLK','CME/CLM','CME/CLN','CME/CLQ','CME/CLU','CME/CLV','CME/CLX','CME/CLZ'),
               rep(c(1984:2017),each=12))

# http://www.cmegroup.com/month-codes.html
what <- paste0(c('CME/CLH','CME/CLM','CME/CLU','CME/CLZ'),
               rep(c(1984:2019),each=4))

# lapply(what, function(x) Quandl(x,type='xts'))
data <- lapply(what, function(x) Quandl(x,type='xts')$Settle)
xn <- do.call(cbind, data)
names(xn) <- what

dates <- lapply(data, function(x) index(last(x)))
dates2 <- xts(rep(1, length(unique(dates))), as.Date(unlist(unique(dates))))

xnn = make_front(xn, 5)
miau <- cbind(xnn, dates2) # [paste0('2000/',Sys.Date())]
colnames(miau)[6] <- 'roll'
head(miau, 10)
saveRDS(miau, '~/Dropbox/shiny/townsend/data.rds')

runApp('~/Dropbox/shiny/townsend/app.R')
rsconnect::deployApp('~/Dropbox/shiny/townsend', appName='cl_spreads')

depl
final <- as.zoo(cbind(miau[,2]-miau[,1],miau[,5]-miau[,4],miau[,1],miau$roll))

tail(final)

plot(final[,1])
lines(final[,2], col='red')
xblocks(final[,3])
abline(v=final[,3])

###
.hE$make_front <- function(x,y=1,option='correct') {
  if(y==1) return( xts(apply(coredata(x), 1, function(row) { row[!is.na(row)][1] }), order.by = index(x)) )
  if(y>1) {
    if(option=='correct') {
      temp <- xts(t(apply(coredata(x), 1, function(row) { 
        row[which(!is.na(row))[1]:(which(!is.na(row))[1]+(y-1))] })), index(x)) 
      colnames(temp) <- c(1:y)
      return(temp) }
    if(option=='mix') {
      temp <- xts(t(apply(coredata(x), 1, function(row) { row[!is.na(row)][1:y] })), index(x))
      colnames(temp) <- c(1:y)
      return(temp) }
  }
}
