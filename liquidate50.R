sd = 0.06
length = 20
time = 4/52
time = 5*4/260
length
set.seed(1000)
generator = function(n, ...) sd_var * rnorm(n)
check <- replicate(100000, {
  strad <- GBSOption('c', 1, 1, time, 0, 0, sd)@price+GBSOption('p', 1, 1, time, 0, 0, sd)@price
  sd_var <- sqrt(1/260)*sd 
  # asd = rnorm(length, mean=0, sd=sd_var)
  random = arima.sim(n = length, list(ar = c(-0.1)), rand.gen = generator)
  # random = rnorm(length, mean=0, sd=sd_var)
  # rnorm(length-1, mean=0, sd=sd_var) # gibt falsch
  # end <- sum(asd)+1
  asd = data.frame('t'=(length-1):0,'rnorm'=random)
  asd$S <- cumprod(1+asd$rnorm)
  # asd$S <- cumsum(1+asd$rnorm)
  # end <- last(asd$S)
  asd$Strad <- strad
  asd$Strad2 <- GBSOption('c', asd$S, 1, asd$t/260, 0, 0, sd)@price+GBSOption('p', asd$S, 1, asd$t/260, 0, 0, sd)@price
  # asd$Diff <- asd$Strad-asd$Strad2
  asd$Cond <- asd$Strad>2*asd$Strad2
  asd$Profit <- asd$Strad-asd$Strad2
  asd$Profit2 <- c(asd$Profit[1:min(which(asd$Cond), nrow(asd))], rep(NA,20))[1:nrow(asd)]
  asd$Profit3 <- na.locf(asd$Profit2)
  # strad2 <- GBSOption('c', end, 1, 0, 0, 0, sd)@price+GBSOption('p', end, 1, 0, 0, 0, sd)@price
  return(asd)
  
  # checkx <<- asd
  # # return(strad-strad2)
  # return(last(asd$Strad2))
}, simplify = FALSE)

test <- check[[88]]
test

0.05093543*2
options(scipen=999)
checkx <- lapply(check, function(x) last(x$Strad)-last(x$Strad2))
checkx <- lapply(check, function(x) last(x$Profit))
checkx <- lapply(check, function(x) last(x$Profit3))
mean(unlist(checkx)) # almost 0

mad1 <- unlist(lapply(check, function(x) abs(last(x$S)-1)))
mean(mad1)
0.06*sqrt(4/52)*sqrt(2/pi)


rnorm(length, mean=0, sd=sd_var)
data.frame('hitler'=(length-1):0,'rapper'=rnorm(length, mean=0, sd=sd_var))
cumprod(1+rnorm(22, 0, sd_var))
