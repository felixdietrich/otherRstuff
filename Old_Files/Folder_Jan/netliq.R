
# new = data.frame(matrix(data=NA,nrow=250,ncol=9))
# i = 1
colnames(new)=c('Time','Net Liq','Change','EURUSD un','EURUSD re','GBPCHF un','GBPCHF re','SEKJPY un','SEKJPY re',
                'margin','EURUSD sum','GBPCHF sum','SEKJPY sum','EURUSD pos','GBPCHF pos','SEKJPY pos','EURUSD','GBPCHF','SEKJPY','accrued')

source('/Users/Maschine/Dropbox/jan/functionjan2 copy.R')
getwd()
setwd('/Users/Maschine/Dropbox/jan')
new = readRDS('tkdemo666_final.rds')
head(new, 10)
i = readRDS('ind.rds')
i
i = i+1
tkdemo  = reqAccountUpdates(tws, subscribe = TRUE, acctCode = "1", eventWrapper = eWrapper(), CALLBACK=twsCALLBACK) # 
print(paste(tkdemo[[2]][[1]]$contract$local, tkdemo[[2]][[2]]$contract$local, tkdemo[[2]][[3]]$contract$local))
new[i,1]= as.character(strftime(Sys.time(), format = '%Y-%m-%d %H:%M'))
new[i,2]= as.numeric(tkdemo[[1]][["NetLiquidation"]][["value"]])
new[i,3]= round(new[i,2]-new[i-1,2], digits=0)
new[i,4]= round(tkdemo[[2]][[1]]$portfolioValue$unrealizedPNL, digits=0) # EURUSD
new[i,5]= round(tkdemo[[2]][[1]]$portfolioValue$realizedPNL, digits=0)
new[i,6]= round(tkdemo[[2]][[2]]$portfolioValue$unrealizedPNL, digits=0) # GBPCHF
new[i,7]= round(tkdemo[[2]][[2]]$portfolioValue$realizedPNL, digits=0)
new[i,8]= round(tkdemo[[2]][[3]]$portfolioValue$unrealizedPNL/120, digits=0) # SEKJPY
new[i,9]= round(tkdemo[[2]][[3]]$portfolioValue$realizedPNL/120, digits=0)
new[i,10]= round(as.numeric(tkdemo[[1]][["MaintMarginReq"]][["value"]]), digits=0)
new[i,11]= round(sum(new[,5], na.rm=T)+new[i,4], digits=0) # if all positive
new[i,12]= round(sum(new[,7], na.rm=T)+new[i,6], digits=0) # trends are already 
new[i,13]= round(sum(new[,9], na.rm=T)+new[i,8], digits=0) # made up
new[i,14]= format(tkdemo[[2]][[1]]$portfolioValue$position,big.mark=".",scientific=FALSE)
new[i,15]= format(tkdemo[[2]][[2]]$portfolioValue$position,big.mark=".",scientific=FALSE)
new[i,16]= format(tkdemo[[2]][[3]]$portfolioValue$position,big.mark=".",scientific=FALSE)
new[i,17]= round(tkdemo[[2]][[1]]$portfolioValue$marketPrice, digits=4)
new[i,18]= round(tkdemo[[2]][[2]]$portfolioValue$marketPrice, digits=4)
new[i,19]= round(tkdemo[[2]][[3]]$portfolioValue$marketPrice, digits=2)
new[i,20]= round(as.numeric(tkdemo[[1]]$AccruedCash[1]), digits=2)
# AccruedCash.S ist negativ obwohl AccruedCash 0 --- was ist der unterschied?

head(new, 15)

saveRDS(new, 'tkdemo666_final.rds')
saveRDS(i, 'ind.rds')
write.xls(new, 'tkdemo666_b.xls', row.names = FALSE, col.names = TRUE)



### FDDEMO
fddemo  = reqAccountUpdates(tws, subscribe = TRUE, acctCode = "1", eventWrapper = eWrapper(), CALLBACK=twsCALLBACK) # 
netliq  = as.numeric(fddemo[[1]][["NetLiquidation"]][["value"]])
saveRDS(netliq, paste(dt, 'fd987demo.rds', sep=''))


### GET NET LIQUIDATION VALUE EVERY DAY 
# umschreiben, dass er herunterzaehlt bis 23:15 oder so
for(period in 1:365){ # jeden tag
  tws <- twsConnect() 
  testjan=reqAccountUpdates(tws,
                            subscribe = TRUE,
                            acctCode = "1",
                            eventWrapper = eWrapper(),
                            CALLBACK=twsCALLBACK) # 
  lok=as.numeric(testjan[[1]][["NetLiquidation"]][["value"]])
  
  print(reqCurrentTime(tws))
  print(lok)
  Sys.sleep(  60 )  
}
as.Date(reqCurrentTime(tws)) # !


### versuch dez 2015, aber zu umstaendlich
a=paste('maschine',as.character(tempZ))
assign(a, lok)
b=1; if(weekdays(Sys.Date())=="Monday") b=3
tempZ = as.Date(Sys.Date())-b
tempZ = Sys.Date()-b

dt=as.Date(Sys.Date())
saveRDS(lok, paste(dt, 'tkdemo666_b.rds', sep=''))
readRDS(paste(dt, 'tkdemo666_b.rds', sep=''))
