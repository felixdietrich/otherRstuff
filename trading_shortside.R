plot(hist(rnorm(1000)))

require(SuppDists)
a <- rnorm( 5000, 0, 2 )
b <- rnorm( 1000, -2, 4 )
c <- rnorm( 3000,  4, 4 )
babyGotKurtosis <- c( a, b, c )
hist( babyGotKurtosis , freq=FALSE)

# https://www.mathworks.com/help/stats/generating-data-using-flexible-families-of-distributions.html
parms <- JohnsonFit(babyGotKurtosis, moment="quant")
parms
plot(function(x)dJohnson(x,parms), -20, 20, add=TRUE, col="red")

### 
xx<-rnorm(50000)
xx<-rt(1000,30)
parms<-JohnsonFit(xx)
sJohnson(parms)
options(scipen = 999)
(rt(1000,5)/100)+1
max(rt(1000,5)/100)
hist(rt(1000,10))
plot(cumprod((rt(1000,5)/100)+1)) # !
plot(100+(rt(1000,10)+100))
plot(cumsum(rt(1000,10)))

dat <- replicate(100, cumprod((rt(1000,5)/100)+1), simplify = FALSE)
dat <- lapply(dat, function(x) xts(x, as.Date(1:1000)))

check <- dat[[100]]
ind <- sign(lag(diff(log(check))))
size <- 1/check
true <- diff(check)
true_s <- true*size
true_m <- true_s*ind
ret <- diff(log(check))
ret_m <- ret*ind
cumsum(na.omit(true_m))
cumsum(na.omit(ret_m))
