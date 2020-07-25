#AUTHOR
#Jan Cuonz
#cuonzj@gmail.com

############ forth and back strategy ############

rm(list = ls()) #clear workspace

#packages
library(xts)
library(highfrequency)




#---------------------------------------------------------
#read data
data_5min = read.csv2("C:/Users/J/Data/Dropbox/PhD/Data/EBS data/R_inputs_liqvar/input_fiveMinData_3.csv",
                      header=TRUE, dec = ".")

#convert into xts time series
data_5min <- xts(data_5min[,-1],as.POSIXct(as.character(data_5min[,1]),format="%Y-%m-%d %H:%M"))


#TODO: filter outliers and holidays

#---------------------------------------------------------
#calculate log mid prices (P) and relative bid-ask spreads (based on original 5min data)

#mid prices
P <- (data_5min[,2]+data_5min[,1])/2
P <- cbind(P,(data_5min[,5]+data_5min[,4])/2)
P <- cbind(P,(data_5min[,8]+data_5min[,7])/2)
P <- cbind(P,(data_5min[,11]+data_5min[,10])/2)
P <- cbind(P,(data_5min[,14]+data_5min[,13])/2)
P <- cbind(P,(data_5min[,17]+data_5min[,16])/2)
P <- cbind(P,(data_5min[,20]+data_5min[,19])/2)
P <- cbind(P,(data_5min[,23]+data_5min[,22])/2)
P <- cbind(P,(data_5min[,26]+data_5min[,25])/2)
names(P) <- cbind("EURCHF","USDJPY","EURUSD","EURJPY","USDCHF","USDCAD","EURGBP",
                  "GBPUSD","AUDUSD")


#---------------------------------------------------------
#mid price log returns
rLP_5 <- diff(log(P))

#delete unused arrays
rm(data_5min)


#---------------------------------------------------------
#trading signal
tsignal1 <- rLP_5
tsignal2 <- rLP_5
tsignal1[tsignal1 < 0] <-  1            #sell signal
tsignal1[tsignal != 1] <-  0
tsignal2[tsignal2 > 0] <- -1            #buy signal
tsignal2[tsignal2 != -1] <- 0 
tsignal <- tsignal1 + tsignal2
rm(tsignal1,tsignal2)

#---------------------------------------------------------
#trading
balance      <- 0
pos_ec_long  <- matrix(88,1,1)      #long positions
pos_ec_short <- matrix(88,1,1)      #short positions


#for(i in 1:ncol(tsignal))
for(i in 1:1)
{
  for (j in 2:nrow(tsignal))
  {
    
    
    if(tsignal[j,i]==-1)  #last trade was up, so go short now
      {
      #if length pos_ec_long < 2 then just go short
      if(nrow(pos_ec_long)<2){pos_ec_short=rbind(coredata(P[j,i]),pos_ec_short)}
      
      #if that was true, dont execute the next if! > next 
      
      #if length of pos_ec_long > 1 then close the latest long position
      if(nrow(pos_ec_long)>1)
        {
          balance = balance + (coredata(P[j,i])-pos_ec_long[1,1])
          pos_ec_long <- t(t(pos_ec_short[-1,]))
        }
      }
    
    #if last if was true, dont execute the next if! > next j
    
    if(tsignal[j,i]==1)  #last trade was down, so go long now
      {
      #if length pos_ec_short < 2 then just go long
      if(nrow(pos_ec_short)<2){pos_ec_long=rbind(coredata(P[j,i]),pos_ec_long)}
      
      #if that was true, dont execute the next if!
      
      #if length of pos_ec > 1 then close the latest short position
      if(nrow(pos_ec_short)>1)
      {
        balance = balance + (pos_ec_short[1,1]-coredata(P[j,i]))
        pos_ec_short <- t(t(pos_ec_short[-1,]))
      }
        
        
      }
    
    
    
    if(tsignal[j,i]==0)  #hold signal
    {
      
      
    }
    
    
  }
}











