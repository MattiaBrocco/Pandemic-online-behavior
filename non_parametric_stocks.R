library(sm)
library(gam)
library(fpp2)
library(lmtest)
library(DIMORA)
#library(Metrics)
library(splines)
library(ggplot2)
library(forecast)
library(lubridate)

source("support.R")

loess.bias.var <- function(dX){
  bias.v <- c()
  var.v <- c()
  error.v <- c()
  res.v <- c()
  for (i in seq(0.1, 3, length.out = 300))
  {
    loessMod <- loess(dX~seq(1, length(dX)), span = i, degree = 2)
    
    res <- resid(loessMod)
    bias2 <- sum((mean(loessMod$fitted)-dX)^2)/length(dX)
    var <- sum((mean(loessMod$fitted)-loessMod$fitted)^2)/length(dX)
    
    bias.v <- append(bias.v, bias2)
    var.v <- append(var.v, var)
    error.v <- append(error.v, bias2+var)
    res.v <- append(res.v, sum(res))
  }
  
  out <- cbind(bias.v, var.v, error.v, res.v)
  out <- data.frame(out)
  names(out) <- c("Bias", "Variance", "Total.Error", "SSR")
  
  best.span <- seq(0.1, 3,
                   length.out = 300)[which(out$Total.Error == min(out$Total.Error))]
  
  plot(seq(0.1, 3, length.out = 300), out$Variance,
       type = "l", ylim = c(min(out[,-4])*0.95, 1.05*max(out[,-4])))
  lines(seq(0.1, 3, length.out = 300), out$Bias, col = 2)
  legend(2.2, 2770, legend=c("Variance", "Bias"),
         col = c("black", "red"), lty=1:1, cex=0.6)
  abline(v = best.span, lty = 2)
  
  
  return(out)
}

AMZN_weekly <- read.timeseries.stocks("./yahoo_data/AMZN_weekly.csv")

loess.AMZN <- loess.bias.var(AMZN_weekly$Close)

# Best Loess fit for AMZN
loess.AMZN.best <- loess(AMZN_weekly$Close~seq(1, length(AMZN_weekly$Close)),
                         span = seq(0.1, 3,
                                    length.out = 300)
                         [which(loess.AMZN$Total.Error ==
                                  min(loess.AMZN$Total.Error))],
                         degree = 2)

plot(AMZN_weekly$Time, AMZN_weekly$Close, type = "l",
     lwd = 2, xlab = "Time", ylab = "Amazon close")
j.AMZN <- order(AMZN_weekly$Time)
lines(AMZN_weekly$Time[j],loess.AMZN.best$fitted[j], col = 2, lwd = 2)






#arr <- sapply(X = seq(0.1, 1, length.out = 100), FUN = calcSSE,
#              dX = AMZN_weekly$Close)

#lo1 <- loess(AMZN_weekly$Close~seq(1, length(AMZN_weekly$Time)), span = .2)

#plot(AMZN_weekly$Time, AMZN_weekly$Close, type = "l",
#     lwd = 2, xlab="Time", ylab="Amazon close")
#j <- order(AMZN_weekly$Time)
#lines(AMZN_weekly$Time[j],lo1$fitted[j], col = 2, lwd = 2)
