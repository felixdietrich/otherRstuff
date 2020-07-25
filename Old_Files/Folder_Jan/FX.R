library(xts)
Sys.setenv(TZ="EST5EDT")

# inputs
increment <- 0.001
betsize <- 100000
trade_com <- 2
ON_interest <- 0.02 # not used
hold_period <- 0 # not used

# insert new trade data to stack
stack_push <- function(time, operation, price, act_price)
{
  new_stack <- trade_stack
  n <- nrow(new_stack)
  new_stack <- rbind(new_stack, data.frame(datetime = time, operation = operation, price = price, act_price = act_price))
  
  trade_stack <<- new_stack
}

# pop trade data from stack
stack_pop <- function()
{
  new_stack <- trade_stack
  n <- nrow(new_stack)
  pop_trade <- as.matrix(new_stack[n, -(1:2)])
  trade_stack <<- new_stack[-n, ]
  
  pop_trade
}

# calculate unrealized profit when necessary
stack_unrealized_profit <- function(last_price)
{
  n <- nrow(trade_stack)
  if (n>0) {
    trade_prices <- sum(trade_stack$price)
    profit <- ifelse(trade_stack$operation[1] == "buy", -betsize*(trade_prices-n*last_price), betsize*(trade_prices-n*last_price)) - n*trade_com
  } else
    profit <- 0
  
  profit
}


# read data
data <- readRDS('~/Dropbox/Public/old/EURUSDminfrom2015.rds')
data <- readRDS('SEKJPYminfrom2015.rds')
data <- readRDS('GBPCHFminfrom2015.rds')

# shift timezone
indexTZ(data) <- "EST5EDT"
index(data) <- index(data)+60*60*7

# add new columns to data
data$outstanding <- NA
data$realized_profit <- 0

# number of observations
N <- nrow(data)

# set initial indicative level
level <- coredata(data[1,4]+data[1,8])/2
initial <- level

# level <- 1.2965
# current outstanding
outstanding <- 0

# convert to list for speed up
data <- as.data.frame(data)
ind <- as.list(rownames(data))
data <- as.list(data)

# stack of current unrealized operations
trade_stack <- data.frame(datetime = numeric(0), operation = character(0), price = numeric(0), act_price = numeric(0), stringsAsFactors=F)

# if ask close < indicative level - increment, then buy and update indicative level
# if bid close > indicative level + increment, then sell and update indicative level
# running backtesting
for (i in 1:N)
{
  if (data[[7]][i]<=(level-increment)) { # buy
    
    # how many times to buy
    count_trades <- floor((level-data[[7]][i])/increment)
    # initialize
    realized_profit <- 0
    
    if (count_trades > 0) {
      # process for each buy operation
      for (j in 1:count_trades) {
        if (outstanding>=0) {
          # if no sell oprations, then put to protfolio
          stack_push(ind[[i]], "buy", level-j*increment, data[[7]][i])
        } else {
          # get last sell operation
          last_trade <- stack_pop()
          # calculate profit
          realized_profit <- realized_profit + betsize*(last_trade[1]-(level-j*increment)) - 2*trade_com
          
        }
        # update oustanding
        outstanding <- outstanding + betsize
      }
      
      # update indicative level
      level <- level-count_trades*increment
    }
    
    # update data
    data[[9]][i] <- outstanding
    if (realized_profit > 0)
      data[[10]][i] <- realized_profit
    
    
  } else if (data[[2]][i] >= (level+increment)) { # sell
    
    # how many times to sell
    count_trades <- floor((data[[2]][i]-level)/increment)
    # initialize
    realized_profit <- 0
    
    if (count_trades > 0) {
      # process for each sell operation
      for (j in 1:count_trades) {
        if (outstanding>0) {
          # get last buy operation
          last_trade <- stack_pop()
          # calculate profit
          realized_profit <- realized_profit + betsize*((level+j*increment)-last_trade[1]) - 2*trade_com
        } else {
          # if no buy oprations, then put to protfolio
          trade_stack <- stack_push(ind[[i]], "sell", level+j*increment, data[[2]][i])
        }
        # update oustanding
        outstanding <- outstanding - betsize
      }
      
      # update indicative level
      level <- level+count_trades*increment
    }
    
    # update data
    data[[9]][i] <- outstanding
    if (realized_profit > 0)
      data[[10]][i] <- realized_profit
    
  } else { # do nothing
    
  }
  
  # do some processing in the end of the day
  if (strftime(ind[[i]], format="%H:%M:%S") == "23:59:00") { # process after day end
    
  }
  
}

# convert back to ts
data <- xts(do.call(cbind, data), order.by=as.POSIXct(unlist(ind)))

# update outstanding for all timestamps
if (is.na(data$outstanding[1]))
  data$outstanding[1] <- 0
data$outstanding <- na.locf(data$outstanding, na.rm=F)
