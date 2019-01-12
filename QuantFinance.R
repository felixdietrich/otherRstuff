require(quantmod)
getSymbols("SPY")
spy <- SPY$SPY.Adjusted
require(urca)
test <- ur.kpss(as.numeric(spy))
class(test)
test@teststat
test@cval
spy_returns <- diff(log(spy))
test_returns <- ur.kpss(as.numeric(spy_returns))
test_returns@teststat
test_returns@cval
test_post_2013 <- ur.kpss(as.numeric(spy_returns['2013::']))
test_post_2013@teststat


pepsi <- getSymbols('PEP', from = '2013-01-01', to = '2014-01-01', adjust = T, auto.assign = FALSE)
coke <- getSymbols('COKE', from = '2013-01-01', to = '2014-01-01', adjust = T, auto.assign = FALSE)
Sys.setenv(TZ = "UTC")
prices <- cbind(pepsi[, 6], coke[, 6])
price_changes <- apply(prices, 2, diff)
plot(price_changes[, 1], price_changes[, 2], xlab = "Coke price changes", ylab = "Pepsi price changes", main = "Pepsi vs. Coke", cex.main = 0.8, cex.lab = 0.8, cex.axis = 0.8)
grid()
ans <- lm(price_changes[, 1] ~ price_changes[, 2])
beta <- ans$coefficients[2]
beta

SPY <- getSymbols('SPY', from = '2011-01-01', to = '2012-12-31', adjust = T, auto.assign = FALSE)
AAPL <- getSymbols('AAPL', from = '2011-01-01', to = '2012-12-31', adjust = T, auto.assign = FALSE)
# Compute price differences
x <- diff(as.numeric(SPY[, 4]))
y <- diff(as.numeric(AAPL[, 4]))
plot(x, y, main = "Scatter plot of returns. SPY vs. AAPL", cex.main = 0.8, cex.lab = 0.8, cex.axis = 0.8)
abline(lm(y ~ x))
abline(lm(x ~ y), lty = 2)
grid()
# Total least squares regression
r <- prcomp( ~ x + y )
slope <- r$rotation[2, 1] / r$rotation[1, 1]
intercept <- r$center[2] - slope * r$center[1]
# Show the first principal component on the plot
abline(a = intercept, b = slope, lty = 3)


# Function to calculate the spread
calculate_spread <- function(x, y, beta) {
  return(y - beta * x)
}
# Function to calculate the beta and level
# given start and end dates
calculate_beta_and_level <- function(x, y,
                                     start_date, end_date) {
  require(xts)
  time_range <- paste(start_date, "::",
                      end_date, sep = "")
  x <- x[time_range]
  y <- y[time_range]
  dx <- diff(x[time_range])
  dy <- diff(y[time_range])
  r <- prcomp( ~ dx + dy)
  beta <- r$rotation[2, 1] / r$rotation[1, 1]
  spread <- calculate_spread(x, y, beta)
  names(spread) <- "spread"
  level <- mean(spread, na.rm = TRUE)
  outL <- list()
  outL$spread <- spread
  outL$beta <- beta
  outL$level <- level
  return(outL)
}

# Function to calculate buy and sell signals
# with upper and lower threshold
calculate_buy_sell_signals <- function(spread, beta,
                                       level, lower_threshold, upper_threshold) {
  buy_signals <- ifelse(spread <= level -
                          lower_threshold, 1, 0)
  sell_signals <- ifelse(spread >= level +
                           upper_threshold, 1, 0)
  # bind these vectors into a matrix
  output <- cbind(spread, buy_signals,
                  sell_signals)
  colnames(output) <- c("spread", "buy_signals",
                        "sell_signals")
  return(output)
}
