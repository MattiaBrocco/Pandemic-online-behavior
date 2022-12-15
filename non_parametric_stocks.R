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
source("complexitynonparam.R")



# Import data
AMZN_weekly <- read.timeseries.stocks("./yahoo_data/AMZN_weekly.csv")
NFLX_weekly <- read.timeseries.stocks("./yahoo_data/NFLX_weekly.csv")
ZOOM_weekly <- read.timeseries.stocks("./yahoo_data/ZM_weekly.csv")

# Best Loess
loess.AMZN <- loess.bias.var(AMZN_weekly$Close)
loess.NFLX <- loess.bias.var(NFLX_weekly$Close)
loess.ZOOM <- loess.bias.var(ZOOM_weekly$Close)

# Best Loess fit for AMZN
loess.AMZN.best <- loess(AMZN_weekly$Close~seq(1, length(AMZN_weekly$Close)),
                         span = loess.AMZN$Span[which(loess.AMZN$Total.Error ==
                                                        min(loess.AMZN$Total.Error))],
                         degree = 2)

plot(AMZN_weekly$Time, AMZN_weekly$Close, type = "l",
     lwd = 2, xlab = "Time", ylab = "",
     main = "Amazon close price")
j.AMZN <- order(AMZN_weekly$Time)
lines(AMZN_weekly$Time[j.AMZN],loess.AMZN.best$fitted[j.AMZN],
      col = 2, lwd = 2)


# Best Loess fit for NFLX
loess.NFLX.best <- loess(NFLX_weekly$Close~seq(1, length(NFLX_weekly$Close)),
                         span = loess.NFLX$Span[which(loess.NFLX$Total.Error ==
                                                        min(loess.NFLX$Total.Error))],
                         degree = 2)

plot(NFLX_weekly$Time, NFLX_weekly$Close, type = "l",
     lwd = 2, xlab = "Time", ylab = "",
     main = "Netflix close price")
j.NFLX <- order(NFLX_weekly$Time)
lines(NFLX_weekly$Time[j.NFLX],
      loess.NFLX.best$fitted[j.NFLX], col = 2, lwd = 2)


# Best Loess fit for ZOOM
loess.ZOOM.best <- loess(ZOOM_weekly$Close~seq(1, length(ZOOM_weekly$Close)),
                         span = seq(0.1, 3,
                                    length.out = 300)
                         [which(loess.ZOOM$Total.Error ==
                                  min(loess.ZOOM$Total.Error))],
                         degree = 2)

plot(ZOOM_weekly$Time, ZOOM_weekly$Close, type = "l",
     lwd = 2, xlab = "Time", ylab = "",
     main = "ZOOM close price")
j.ZOOM <- order(ZOOM_weekly$Time)
lines(ZOOM_weekly$Time[j.ZOOM],
      loess.ZOOM.best$fitted[j.ZOOM], col = 2, lwd = 2)


# MISS THE SAME FOR LOCAL REGRESSION AND SPLINES



