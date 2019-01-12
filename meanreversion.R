### http://www.rpubs.com/snowdj/ou ----
# https://renkun-ken.github.io/blog/2014/04/05/fit-an-ou-process-with-realized-time-series-data.html uses same code

require ( stats4 )
require ( sde )

# Two functions from page 114
# Functions dcOU evaluates the conditional density of the process. It generates the density of the x which from cerntain distribution
dcOU<- function (x, t, x0 , theta , log = FALSE ){
  Ex <- theta [1] / theta [2]+( x0 - theta [1] / theta [2]) * exp (- theta [2] *t)
  Vx <- theta [3]^2 *(1- exp (-2* theta [2] *t))/(2* theta [2])
  dnorm (x, mean =Ex , sd = sqrt (Vx), log = log )
}
# Function OU.lik generates the log likelihood function of X for the MLE process. 
OU.lik <- function ( theta1 , theta2 , theta3 ){
  n <- length (X)
  # deltat is the time interval between observations
  dt <- deltat (X)-sum (dcOU (X [2: n], dt , X [1:(n -1)] , c( theta1 , theta2 , theta3 ), log = TRUE ))
}
# The function OU.lik needs as input the three parameters and assumes that sample observations of the process are available in the current R workspace in the ts object X.

ouFit.ML = function(spread) {
  n = length(spread)
  delta = 1  # delta 
  Sx = sum(spread[1:n - 1])
  Sy = sum(spread[2:n])
  Sxx = sum((spread[1:n - 1])^2)
  Syy = sum((spread[2:n])^2)
  Sxy = sum((spread[1:n - 1]) * (spread[2:n]))
  mu = (Sy * Sxx - Sx * Sxy)/((n - 1) * (Sxx - Sxy) - (Sx^2 - Sx * Sy))
  theta = -log((Sxy - mu * Sx - mu * Sy + (n - 1) * mu^2)/(Sxx - 2 * mu * 
                                                             Sx + (n - 1) * mu^2))/delta
  a = exp(-theta * delta)
  sigmah2 = (Syy - 2 * a * Sxy + a^2 * Sxx - 2 * mu * (1 - a) * (Sy - a * 
                                                                   Sx) + (n - 1) * mu^2 * (1 - a)^2)/(n - 1)
  sigma = sqrt((sigmah2) * 2 * theta/(1 - a^2))
  theta = list(theta = theta, mu = mu, sigma = sigma, sigmah2 = sigmah2)
  return(theta)
}

### https://robotwealth.com/exploring-mean-reversion-and-cointegration-part-2/ ----
# Calculate half life of mean reversion
y <- na.omit(quantmod::getSymbols('AUDNZD=X', auto.assign = F)[,4])
plot.zoo(y)
y.lag <- lag(y, 1)
delta.y <- diff(y)

df <- cbind(y, y.lag, delta.y)
head(df)
df <- df[-1 ,] #remove first row with NAs

regress.results <- lm(delta.y ~ y.lag, data = df) # WIESO HIER NICHT MEAN?
lambda <- summary(regress.results)$coefficients[2]
half.life <- -log(2)/lambda

### DISCRETE 2 ----
# see in MeanReversionHalf.R das ist das korrekte glaub ich
y <- na.omit(quantmod::getSymbols('AUDNZD=X', auto.assign = F)[,4])
