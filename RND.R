install.packages('RND')
require(RND)
r = 0.05
te = 60/365
s0 = 1000
sigma = 0.25
y = 0.01
call.strikes = seq(from = 500, to = 1500, by = 25)
market.calls = price.bsm.option(r =r, te = te, s0 = s0, k = call.strikes, sigma = sigma, y = y)$call
put.strikes = seq(from = 510, to = 1500, by = 25)
market.puts = price.bsm.option(r =r, te = te, s0 = s0, k = put.strikes, sigma = sigma, y = y)$put
#
# Get extract the parameter of the density
#
extract.bsm.density(r = r, y = y, te = te, s0 = s0, market.calls = market.calls, call.strikes = call.strikes, market.puts = market.puts, put.strikes = put.strikes, lambda = 1, hessian.flag = FALSE)
asd=extract.bsm.density(r = r, y = y, te = te, s0 = s0, market.calls = market.calls, call.strikes = call.strikes, market.puts = market.puts, put.strikes = put.strikes, lambda = 1, hessian.flag = FALSE)

exp(6.91)
exp(0.103)

# The extracted parameters should be close to these actual values:
actual.mu = log(s0) + ( r - y - 0.5 * sigma^2) * te
actual.zeta = sigma * sqrt(te)
actual.mu
actual.zeta

data(vix.2013.06.25)
head(vix.2013.06.25)

### ### 

r = 0.05
y = 0.03
s0 = 1000
sigma = 0.25
te = 100/365
strikes = seq(from=600, to = 1400, by = 1)
v = sqrt(exp(sigma^2 * te) - 1)
ln.skew = 3 * v + v^3
ln.kurt = 16 * v^2 + 15 * v^4 + 6 * v^6 + v^8
skew.4 = ln.skew * 1.50
kurt.4 = ln.kurt * 1.50
skew.5 = ln.skew * 0.50
kurt.5 = ln.kurt * 2.00
ew.density.4 = dew(x=strikes, r=r, y=y, te=te, s0=s0, sigma=sigma, skew=skew.4, kurt=kurt.4)
ew.density.5 = dew(x=strikes, r=r, y=y, te=te, s0=s0, sigma=sigma, skew=skew.5, kurt=kurt.5)
bsm.density = dlnorm(x = strikes, meanlog = log(s0) + (r - y - (sigma^2)/2)*te, sdlog = sigma*sqrt(te), log = FALSE)
matplot(strikes, cbind(bsm.density, ew.density.4, ew.density.5), type="l", lty=c(1,1,1), col=c("black","red","blue"), main="Black = BSM, Red = EW 1.5 Times, Blue = EW 0.50 & 2")

### ###
te = 60/365
s0 = 100
k = seq(from = 50, to = 150, by = 1)
sigma = 0.25
y = 0.01
bsm.calls = price.bsm.option(r = r, te = te, s0 = s0, k = k, sigma = sigma, y = y)$call
density.est = get.point.estimate(market.calls = bsm.calls, call.strikes = k, r = r , te = te)
len = length(k)-1
### Note, estimates at two data points (smallest and largest strikes) are lost
plot(density.est ~ k[2:len], type = "l")

### ### 
VXXdata <- read.csv("~/Downloads/VXX_2014.csv", stringsAsFactors=FALSE)
head(VXXdata, 20)
VXXdata2 = VXXdata[which(VXXdata$quotedate=='01/02/2014'),]
VXXdata2 = VXXdata2[which(VXXdata2$exchange=='*'),]
VXXdata2[which(VXXdata2$expiration=='01/16/2016'),]
VXXdata3 = VXXdata2[which(VXXdata2$expiration=='01/18/2014'),]
VXXdata3 = VXXdata3[which(VXXdata3$type=='call'),]

a=difftime(as.Date(VXXdata3$expiration[1], format='%m/%d/%Y'),as.Date(VXXdata3$quotedate[1], format='%m/%d/%Y'))
head(VXXdata3,50)
bsm.calls=((VXXdata3$bid+VXXdata3$ask)/2)[1:30]
bsm.calls=bsm.calls[1:30]
length(bsm.calls)
te = as.numeric(a)/365
s0 = VXXdata3$underlying_last[1]
k = seq(from = 30, to = 59, by = 1)
k = VXXdata3$strike[1:30] 

sigma = 0.25
y = 0.01
bsm.calls = price.bsm.option(r = r, te = te, s0 = s0, k = k, sigma = sigma, y = y)$call # hier CALL
bsm.calls
density.est = get.point.estimate(market.calls = bsm.calls, call.strikes = k, r = r , te = te)
sum(density.est)
len = length(k)-1
### Note, estimates at two data points (smallest and largest strikes) are lost
plot(density.est ~ k[2:len], type = "l")
abline(v=VXXdata3$underlying_last[1], col='red')

### Mother of all Extractions ### 
VXXdata3c = VXXdata3[which(VXXdata3$type=='call'),]
VXXdata3p = VXXdata3[which(VXXdata3$type=='put'),]
VXXdata3p

r = 0.01; te = 60/365; s0 = 1000; sigma = 0.25; y = 0.01 # komplett auf null geht nicht
strikes = seq(from = 500, to = 1500, by = 5)
strikes = seq(from = 30, to = 59, by = 1)

te = as.numeric(a)/365
s0 = VXXdata3$underlying_last[1]
k = seq(from = 30, to = 59, by = 1)
k = VXXdata3$strike[1:30] 

bsm.prices = price.bsm.option(r =r, te = te, s0 = s0, k = strikes, sigma = sigma, y = y)
bsm.prices
calls=((VXXdata3c$bid+VXXdata3c$ask)/2)[1:30]
calls
puts=((VXXdata3p$bid+VXXdata3p$ask)/2)[1:30]
calls = bsm.prices$call
puts = bsm.prices$put
MOE(market.calls = calls, call.strikes = strikes, market.puts = puts, put.strikes = strikes, call.weights = 1, put.weights = 1, lambda = 1, s0 = s0, r = r, te = te, y = y, file.name = "myfile")

plot(strikes,puts)
plot(strikes,calls)
