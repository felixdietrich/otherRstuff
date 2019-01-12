# require(sos)
# findFn("hurst exponent")
# The most full set of methods (nine) is given in package fArma' '(LrdModelling) function hurstSlider.
# http://finzi.psych.upenn.edu/cgi-bin/namazu.cgi?query=hurst&max=100&result=normal&sort=score&idxname=functions&idxname=views
# https://www.r-bloggers.com/exploring-the-market-with-hurst/
# What I found is that the Hurst exponent calculation is an estimation and could vary widely based on the technique.

data <- arima.sim(n = 63, list(ar = c(0.8897, -0.4858) # , ma = c(-0.2279, 0.2488)
                               ), sd = sqrt(0.1796))
plot(data)
data_2 <- lapply(seq(0.05,0.50,0.05), function(x) arima.sim(n = 100, list(ar = c(0.8897, -0.4858)), sd = sqrt(x)))
set.seed(1337)
data_2 <- lapply(seq(-0.05,-0.95,-0.05), function(x) arima.sim(n = 100, list(ar = c(0.8897, x)), sd = sqrt(0.20)))
data_2 <- lapply(seq(-0.05,-0.95,-0.05), function(x) arima.sim(n = 1000, list(ar = c(0.8897, x)), sd = sqrt(0.20)))
lapply(data_2, plot)

### PERFORMANCE ANALYTICS ----
PerformanceAnalytics::HurstIndex(data)
log( (max(data) - min(data))/sd(data) ) / log( length(data) )
hurst <- data.frame('Hurst' = unlist(lapply(data_2, PerformanceAnalytics::HurstIndex)), 
                    'SD' = unlist(lapply(data_2, sd)),
                    'Range' = unlist(lapply(data_2, function(x) diff(range(x)))))
hurst$Fcalc <- hurst$SD/hurst$Range
hurst

### PRACMA ----
pracma::hurstexp(data)
pracma::hurstexp

### FRACTAL ----
fractal::RoverS(data)

### PACKAGE FARMA ----
fArma::rsFit(data)
# Fractional Gaussian noise with H < 0.5 demonstrates negatively autocorrelated or anti-persistent behaviour
fArma::rsFit
fArma::hurstSlider(data)
# fArma::hurstSlider(cumsum(data))

# require(fArma)
# hurstit = function(x) {
#   c(aggvarFit(x, levels = 50, minnpts = 3, cut.off = 10^c(0.7, 2.5), doplot = FALSE, trace = FALSE, title = NULL, description = NULL)@hurst$H,
#     rsFit(x, levels = 50, minnpts = 3, cut.off = 10^c(0.7, 2.5), doplot = FALSE, trace = FALSE, title = NULL, description = NULL)@hurst$H,
#     absvalFit(x, levels = 50, minnpts = 3, cut.off = 10^c(0.7, 2.5), moment = 1, doplot = FALSE, trace = FALSE, title = NULL, description = NULL)@hurst$H,
#     higuchiFit(x, levels = 50, minnpts = 2, cut.off = 10^c(0.7, 2.5), doplot = FALSE, trace = FALSE, title = NULL, description = NULL)@hurst$H,
#     pengFit(x, levels = 50, minnpts = 3, cut.off = 10^c(0.7, 2.5),
#             method = c("mean", "median"), doplot = FALSE, trace = FALSE, title = NULL, description = NULL)@hurst$H,
#     rsFit(x, levels = 50, minnpts = 3, cut.off = 10^c(0.7, 2.5), doplot = FALSE, trace = FALSE, title = NULL, description = NULL)@hurst$H,
#     perFit(x, cut.off = 0.1, method = c("per", "cumper"), doplot = FALSE, title = NULL, description = NULL)@hurst$H,
#     boxperFit(x, nbox = 100, cut.off = 0.10, doplot = FALSE, trace = FALSE, title = NULL, description = NULL)@hurst$H)
# }

### DETRENDED FLUCTUATION
require(sos)
findFn("detrended fluctuation")
require(nonlinearTseries)
# nonlinearTseries.pdf page 9
x=lorenz(sigma=10, rho = 28, beta =8/3, start = c(-10, -11, 47), time = seq(0, 70, by = 0.01), do.plot = FALSE)$x
cd=corrDim(time.series=x,min.embedding.dim=3,max.embedding.dim=6,
           time.lag=10,min.radius=1e-3,max.radius=50,
           n.points.radius=100,theiler.window=100,
           number.boxes=100,do.plot=F)
plot(cd,type="l")
plotLocalScalingExp(cd,cex=0.5,xlim=c(1e-1,5))
cd.est = estimate(cd,regression.range=c(0.2,2))
cat("expected: 2.05 --- estimate: ",cd.est,"\n")

# nonlinearTseries.pdf page 12
white.noise = rnorm(5000)
dfa.analysis = dfa(time.series = white.noise, npoints = 10,
                   window.size.range=c(10,1000), do.plot=FALSE)
white.estimation = estimate(dfa.analysis,do.plot=TRUE)
cat("Theorical: 0.5---Estimated: ",white.estimation ,"\n")

library(fArma)
fgn = as.numeric(fArma::fgnSim(n = 2000, H = 0.75))
dfa.analysis = dfa(time.series = fgn, npoints = 30,
                   window.size.range=c(10,1000),
                   do.plot=FALSE)
fgn.estimation = estimate(dfa.analysis, do.plot = TRUE,
                          fit.col="blue",fit.lwd=2,fit.lty=2,
                          main="Fitting DFA to fGn")
cat("Theorical: 0.75---Estimated: ",fgn.estimation ,"\n")
fbm = as.numeric(fArma::fbmSim(n = 2000, H = 0.25))
dfa.analysis = dfa(time.series = fbm, npoints = 50,
                   window.size.range=c(10,300),
                   do.plot=FALSE)
fbm.estimation = estimate(dfa.analysis,do.plot = TRUE, add.legend=F, main="DFA of fBm")
cat("Theorical: 1.25 ---Estimated: ",fbm.estimation ,"\n")

###
# https://www.quantopian.com/posts/pair-trade-with-cointegration-and-mean-reversion-tests
# http://epchan.blogspot.de/2016/04/mean-reversion-momentum-and-volatility.html
# https://quant.stackexchange.com/questions/7666/using-variance-ratios-to-test-for-mean-reversion/7671#7671
sd(arima.sim(model=list(ar=.5),sd=1,n=1000))
sd(arima.sim(model=list(ar=-.5),sd=1,n=1000))

### 
DBCFHX=readRDS('~/Dropbox/newdiss/files/Oct_DBCFHX.rds') 
ret=na.omit(diff(log(DBCFHX)))
plot.zoo(DBCFHX)
nonlinearityTest(ret, verbose = TRUE)

###
# https://robotwealth.com/demystifying-the-hurst-exponent-part-1/
# approximate entropy // package RHRV

### SIGNATURE PLOTS ----
# from CurrencyMaschine.R
coast <- midpoint(readRDS('~/data/AAPLmincorrecttime.rds')['2016'])
coast <- midpoint(readRDS('~/Dropbox/data/universe/SPYamin.rds')['2016'])
coast <- coast['T09:31/T15:59']
squared <- function(x) { x^2 }
coastline_from_1min = function(x, func) {
  data <- x
  if(ncol(x)!=1) data <- (x[,8]+x[,4])/2
  min_1 <- sum(na.omit(func(diff(data))))
  min_2 <- sum(na.omit(func(diff(to.minutes(data, k=2, OHLC=FALSE)))))
  min_3 <- sum(na.omit(func(diff(to.minutes(data, k=3, OHLC=FALSE)))))
  min_4 <- sum(na.omit(func(diff(to.minutes(data, k=4, OHLC=FALSE)))))
  min_5 <- sum(na.omit(func(diff(to.minutes5(data, OHLC=FALSE)))))
  min_15 <- sum(na.omit(func(diff(to.minutes15(data, OHLC=FALSE)))))
  min_30 <- sum(na.omit(func(diff(to.minutes30(data, OHLC=FALSE)))))
  min_60 <- sum(na.omit(func(diff(to.hourly(data, OHLC=FALSE)))))
  daily <- sum(na.omit(func(diff(to.daily(data, OHLC=FALSE)))))
  c(min_1,min_2,min_3,min_4,min_5,min_15,min_30,min_60,daily)
}
asd = coastline_from_1min(coast, abs)
asd = coastline_from_1min(log(coast), squared)
plot(c(1,2,3,4,5,15,30,60,1440),asd, log="xy")

asd = lapply(c(1:250), function(x) {
  sum(na.omit(diff(log(coast[seq(1,nrow(coast),by=x),]))^2)) })
asd = lapply(c(1:250), function(x) {
  sum(na.omit(sd(diff(log(coast[seq(1,nrow(coast),by=x),])), na.rm=T))) })

coast <- quantmod::getSymbols('AAPL', auto.assign = F)[,6]
coast <- quantmod::getSymbols('SPY', auto.assign = F)[,6]

asd = lapply(c(1:250), function(x) {
  sum(na.omit(diff(log(coast[seq(1,nrow(coast),by=x),]))^2)) })
asd = lapply(c(1:250), function(x) {
  sum(na.omit(sd(diff(log(coast[seq(1,nrow(coast),by=x),])), na.rm=T))) })
asd = lapply(c(1:250), function(x) {
  sum(na.omit(var(diff(log(coast[seq(1,nrow(coast),by=x),])), na.rm=T))) })
asd = lapply(c(1:250), function(interval) { # ONLY WORKS IF LOTS OF DATA (HIGH FREQUENCY)
  statistics <- function(x) { sum(na.omit(var(diff(log(x)), na.rm=T))) }
  # test <<- coast[seq(1,nrow(coast),by=interval),]
  mean(tsboot(coast[seq(1,nrow(coast),by=interval),], statistics, R = 100, l = 50, n.sim = 50, sim = "fixed")$t) 
  })

test
plot(c(1:250),unlist(asd), log="xy")
plot(log(c(250:1)),log(unlist(asd)))

plot(log(c(1:250)),log(unlist(asd)))
xx = lm(log(unlist(asd)) ~ log(c(1:250)))
xx
# xx = lm(log(unlist(asd)) ~ log(c(1:250)) + 0)
abline(xx, col='red')
xx$coefficients/2

plot.zoo(cumprod(1+dataq[[1]][,'diff']))
plot.zoo(cumsum(dataq[[1]][,'diff'])+1)
signature_plot(cumsum(dataq[[1]][,'diff'])+1, c(1:30))
signature_plot(cumsum(dataq[[2]][,'diff'])+1, c(1:30))
signature_plot(cumsum(dataq[[3]][,'diff'])+1, c(1:30))
signature_plot(cumsum(dataq[[4]][,'diff'])+1, c(1:30))
signature_plot(cumsum(dataq[[5]][,'diff'])+1, c(1:30))
signature_plot(cumprod(dataq[[1]][,'diff']+1), c(1:30))
signature_plot2(cumprod(dataq[[1]][,'diff']+1), c(1:30))
signature_plot(cumprod(dataq[[5]][,'diff']+1), c(1:30))

signature_plot(cumprod(dataq[[2]][,'diff']+1), c(1:30))
signature_plot(cumprod(dataq[[3]][,'diff']+1), c(1:30))
signature_plot(cumprod(dataq[[4]][,'diff']+1), c(1:30))
signature_plot(cumprod(dataq[[5]][,'diff']+1), c(1:30))

signature_plot = function(coast,interval) {
  asd = lapply(interval, function(x) {
    sum(na.omit(var(diff(log(coast[seq(1,nrow(coast),by=x),])), na.rm=T))) })
  plot(log(interval),log(unlist(asd)), xlab='log(t)', ylab='log(variance)')
  xx = lm(log(unlist(asd)) ~ log(interval))
  abline(xx, col='red')
  mtext(round(xx$coefficients[2]/2,4))
}
signature_plot2 = function(coast,interval) {
  statistics <- function(x) { sum(na.omit(var(diff(log(x)), na.rm=T))) }
  # asd = lapply(interval, function(x) { seq_block(coast,statistics,c(1:20),x) }) # FUNKTIONIERT
  asd = lapply(interval, function(x) { 
    test <<- coast[seq(1,nrow(coast),by=x),]
    mean(tsboot(coast[seq(1,nrow(coast),by=x),], statistics, R = 100, l = 50, n.sim = 50, sim = "fixed")$t) })
  plot(log(interval),log(unlist(asd)), xlab='log(t)', ylab='log(variance)')
  xx = lm(log(unlist(asd)) ~ log(interval))
  abline(xx, col='red')
  mtext(round(xx$coefficients[2]/2,4))
}

test
warnings()
test[seq(2,nrow(test),30),]
xx$coefficients
seq_block = function(data,statistics,blocks,by) { # blocks
  if(is.character(statistics)) {
    B <- function(z) {}
    body(B) <- parse(text = statistics) }
  if(!is.character(statistics)) {
    B <- statistics
  }
  test <<- data
  blocks <<- blocks
  byx <<- by
  # all <- lapply(blocks, function(from) B(data[seq(from,nrow(data),by=by),]))
  # mean(unlist(all))
}

statistics = function(x) { x^2 }
statistics = function(x) { sum(x^2, na.rm = T) }
seq_block(4,'x^2')
seq_block(coast,mean,c(1:5),1)
seq_block(diff(log(coast)),statistics,c(1:5),1)

is.character('mean')
statistics(2)
x=2
coast[seq(1,nrow(coast),by=x),]
coast[seq(1+2,nrow(coast),by=x),]

### SIMULATIONS ----
source('~/Dropbox/newdiss/git/functions.R')
sim = arima.sim(list(ar=0.6), n=1000)
sim = arima.sim(list(ar=-0.6), n=1000)
plot(sim)
rssimple(sim)
