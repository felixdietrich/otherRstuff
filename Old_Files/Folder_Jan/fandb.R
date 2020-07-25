#AUTHOR
#Jan Cuonz
#cuonzj@gmail.com

############################ forth and back strategy ############################

rm(list = ls()) #clear workspace

#packages
library(xts)
library(highfrequency)

#--------------------------------------------------------------------------------
#settings

t_int  <- ""        #time interval, format:"2007-01-04/2007-01-08", leave empty = whole interval
freq   <- 5         #choose data frequency (choose from 5,15,30,60 minutes)
cpt    <- 2         #cost per trade (flat fee), USD
mult   <- 100000    #bet size
minpip <- 20


#################################################################################
#read data
data_5min = read.csv2("C:/Users/J/Data/Dropbox/trading/vergeldungswaffe/R_trading/input_fiveMinData_3_usdjpy.csv",
                      header=TRUE, dec = ".")

#convert into xts time series
data_5min <- xts(data_5min[,-1],as.POSIXct(as.character(data_5min[,1]),format="%Y-%m-%d %H:%M"))


#TODO: filter outliers and holidays


#########################################################################1########
#calculate log mid prices (P) and relative bid-ask spreads (based on original 5min data)

#mid prices
P_5 <- (data_5min[,2]+data_5min[,1])/2
#P_5 <- cbind(P,(data_5min[,5]+data_5min[,4])/2)
#P_5 <- cbind(P,(data_5min[,8]+data_5min[,7])/2)
#P_5 <- cbind(P,(data_5min[,11]+data_5min[,10])/2)
#P_5 <- cbind(P,(data_5min[,14]+data_5min[,13])/2)
#P_5 <- cbind(P,(data_5min[,17]+data_5min[,16])/2)
#P_5 <- cbind(P,(data_5min[,20]+data_5min[,19])/2)
#P_5 <- cbind(P,(data_5min[,23]+data_5min[,22])/2)
#P_5 <- cbind(P,(data_5min[,26]+data_5min[,25])/2)
#names(P_5) <- cbind("EURCHF","USDJPY","EURUSD","EURJPY","USDCHF","USDCAD","EURGBP",
#                  "GBPUSD","AUDUSD")
#names(P_5) <- cbind("EURCHF")
names(P_5) <- cbind("USDJPY")
P <- P_5

P <- P[t_int]    #cut data according to chosen time interval


#################################################################################
#adjust data frequency

if(freq == 5) {
  #do nothing
  
} else if(freq == 15) {
  #change to 15 min data
  P_15 <- to.minutes15(P_5[,1],indexAt="startof")[,1]
  names(P_15) <- cbind("EURCHF")
  P <- P_15
  
  } else if(freq == 30) {
    #change to 30 min data
    P_30 <- to.minutes30(P_5[,1],indexAt="startof")[,1]
    names(P_30) <- cbind("EURCHF")
    P <- P_30
    
    } else if(freq ==60) {
      #change to 60 min data
      P_60 <- to.minutes60(P_5[,1],indexAt="startof")[,1]
      names(P_60) <- cbind("EURCHF")
      P <- P_60
      
      } else
        print("Error: check data frequency settings")
        

#################################################################################
#mid price log returns
rLP <- diff(log(P))

#delete unused arrays
rm(data_5min)


#################################################################################
#trading signal
tsignal1 <- rLP
tsignal2 <- rLP
tsignal1[tsignal1 < 0] <-  1            #sell signal
tsignal1[tsignal1 != 1] <-  0
tsignal2[tsignal2 > 0] <- -1            #buy signal
tsignal2[tsignal2 != -1] <- 0 
tsignal <- tsignal1 + tsignal2
rm(tsignal1,tsignal2)


#################################################################################
#trading
tradec       <- 0           #trade counter
balance      <- xts()       #P&L    
balance_ib   <- xts()       #P&L InteractiveBrookers (avg rate)
# pos_ec_long  <- xts()       #long positions
# pos_ec_short <- xts()       #short positions

pos_ec_long  <- matrix()       #long positions
pos_ec_short <- matrix()       #short positions

#time measurement
start.time <- Sys.time()

#for(i in 1:ncol(tsignal)) #TODO
for(i in 1:1) {
  for (j in 2:nrow(tsignal)) {
    
    #last return was positive, so go short now
    if(tsignal[j,i]==-1) {  
      #if pos_ec_long is empty then just go short
      if(is.na(pos_ec_long)) {
        if(is.na(pos_ec_short)) {
          pos_ec_short <- as.matrix(P[j,i])
        } else if(length(pos_ec_short) > 0) {
            pos_ec_short <- rbind(as.matrix(P[j,i]),pos_ec_short)
          }
      #if there are open long positions then close the latest long position
      } else if(length(pos_ec_long) > 0) {
          if(length(balance) == 0) {
            balance <- P[j,i]-coredata(pos_ec_long[1,1])
            balance_ib <- 0 #TODO
          } else {
              balance <- rbind(balance,P[j,i]-coredata(pos_ec_long[1,1])) #TODO add time stamp
              balance_ib <- 0 #TODO
             }
          #take account of the case when length = 1 and will be 0 after deleting > make NA again
          if(length(pos_ec_long) == 1) {
            pos_ec_long <- matrix()
          } else if(length(pos_ec_long) > 1) {
              pos_ec_long <- t(t(pos_ec_long[-1,]))
              names(pos_ec_long) <- cbind("long pos.")
            }
        } else
          cat("Error at j = ",j)
      
    #last return was negative, so go long now
    } else if(tsignal[j,i] == 1){  
        #if pos_ec_short is empty then just go long
        if(is.na(pos_ec_short)) {
          if(is.na(pos_ec_long)) {
            pos_ec_long <- as.matrix(P[j,i])
          } else if(length(pos_ec_long) > 0) {
            pos_ec_long <- rbind(as.matrix(P[j,k]),pos_ec_long)
          }
        #if there are open short positions then close the latest short position
        } else if(length(pos_ec_short) > 0) {
            if(length(balance) == 0) {
              balance <- coredata(pos_ec_short[1,1])-P[j,i]
            } else {
                balance <- rbind(balance,coredata(pos_ec_short[1,1])-P[j,i])
                balance_ib <- 0 #TODO
              }
            #take account of the case when length = 1 and will be 0 after deleting > make NA again
            pos_ec_short <- pos_ec_short[-1]
            if(length(pos_ec_short) == 1) {
              pos_ec_short <- matrix()
            } else if(length(pos_ec_short) > 1) {
                pos_ec_short <- t(t(pos_ec_short[-1,]))
                names(pos_ec_long) <- cbind("short pos.")
              }
          } else
            cat("Error at j = ",j)
      
      # last return was zero, so do nothing (just hold positions)
      } else if (tsignal[j,i] == 0) {
        #do nothing
        } else
          print("Error: trading signal misspecified")
  }
}


#time measurement
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken


#################################################################################
#calculate total profit & loss

#P&L before trading costs
PL_before_tc <- sum(balance)
PL_ib_before_tc <- sum(balance_ib) #TODO

#trading costs
nrtrades <- sum(abs(tsignal[2:nrow(tsignal),1]))
tot_trading_costs <-  nrtrades * cpt

#P&L after trading costs
PL <- PL_before_tc * mult - tot_trading_costs
PL_ib <- PL_ib_before_tc - tot_trading_costs


#--------------------------------------------------------------------------------
#output
cat("P&L (traditional calc.) = ",PL)      #P&L (traditional calc.) =  3127073697P&L
cat("P&L (IBrokers calc.) = ",PL_ib)


#################################################################################
#TODO

#--> time stamp for balance
#--> implement variable threshold for trading signal
#--> use bid & ask prices instead of mid prices
#--> control for and limit maximum long/short position
#i.e., control for and limit for unrealised P&L
#--> implement IBrokers method for P&L calculation (avg rates)
#--> plot length of positions vector
#--> let user choose time interval
#--> stop trading when obvious trend (moving average?)

#--------------------------------------------------------------------------------
#done




















