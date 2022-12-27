library(sm)
library(gam)
library(fpp2)
library(lmtest)
library(DIMORA)
#library(rpanel)
library(splines)
library(ggplot2)
library(forecast)
library(lubridate)


source("support.R")
source("complexitynonparam.R")


# Import data----
Google_Trends <- read_csv("./gtrend_data/Google Trends Data.csv", na = "0", skip = 1)

AMZN_weekly = Google_Trends[1:260,c(1,2)]
colnames(AMZN_weekly) = c('Time', 'Close')
NFLX_weekly = Google_Trends[1:260,c(1,3)]
colnames(NFLX_weekly) = c('Time', 'Close')
ZOOM_weekly = Google_Trends[1:260,c(1,4)]
colnames(ZOOM_weekly) = c('Time', 'Close')
ZOOM_weekly$Close[55] = as.numeric(0)
ZOOM_weekly$Close = as.double(ZOOM_weekly$Close)

# Loess----
loess.AMZN <- loess.bias.var(AMZN_weekly$Close)
loess.NFLX <- loess.bias.var(NFLX_weekly$Close)
loess.ZOOM <- loess.bias.var(ZOOM_weekly$Close)

### AMZN----
loess.AMZN.best <- loess(AMZN_weekly$Close~seq(1, length(AMZN_weekly$Close)),
                         span = loess.AMZN$Span[which(loess.AMZN$MSE ==
                                                        min(loess.AMZN$MSE))],
                         degree = 2)

plot(AMZN_weekly$Time, AMZN_weekly$Close,
     type = "l", lwd = 2, xlab = "Time", ylab = "",
     main = "Amazon trend (loess)")
j.AMZN <- order(AMZN_weekly$Time)
lines(AMZN_weekly$Time[j.AMZN],loess.AMZN.best$fitted[j.AMZN],
      col = 2, lwd = 2)


### NFLX----
loess.NFLX.best <- loess(NFLX_weekly$Close~seq(1, length(NFLX_weekly$Close)),
                         span = loess.NFLX$Span[which(loess.NFLX$MSE ==
                                                        min(loess.NFLX$MSE))],
                         degree = 2)

plot(NFLX_weekly$Time, NFLX_weekly$Close,
     type = "l", lwd = 2, xlab = "Time", ylab = "",
     main = "Netflix trend (loess)")
j.NFLX <- order(NFLX_weekly$Time)
lines(NFLX_weekly$Time[j.NFLX],
      loess.NFLX.best$fitted[j.NFLX], col = 2, lwd = 2)


### ZOOM----
loess.ZOOM.best <- loess(ZOOM_weekly$Close~seq(1, length(ZOOM_weekly$Close)),
                         span = seq(0.1, 3,
                                    length.out = 300)
                         [which(loess.ZOOM$MSE ==
                                  min(loess.ZOOM$MSE))],
                         degree = 2)

plot(ZOOM_weekly$Time, ZOOM_weekly$Close, type = "l",
     lwd = 2, xlab = "Time", ylab = "",
     main = "ZOOM trend (loess)")
j.ZOOM <- order(ZOOM_weekly$Time)
lines(ZOOM_weekly$Time[j.ZOOM],
      loess.ZOOM.best$fitted[j.ZOOM], col = 2, lwd = 2)

# Local Regression----
locreg.AMZN <- locreg.bias.var(AMZN_weekly)
locreg.NFLX <- locreg.bias.var(NFLX_weekly)
locreg.ZOOM <- locreg.bias.var(ZOOM_weekly)

best.h.AMZN <- locreg.AMZN$h.param[which(locreg.AMZN$MSE
                                         == min(locreg.AMZN$MSE))]
best.h.NFLX <- locreg.NFLX$h.param[which(locreg.NFLX$MSE
                                         == min(locreg.NFLX$MSE))]
best.h.ZOOM <- locreg.ZOOM$h.param[which(locreg.ZOOM$MSE
                                         == min(locreg.ZOOM$MSE))]

### AMZN----
plot(AMZN_weekly$Time, AMZN_weekly$Close,
     xlab = "Time", main = "Amazon trend (local regr.)",
     ylab = "", lwd = 2, type = "l")
sm.regression(AMZN_weekly$Time, AMZN_weekly$Close, h = best.h.AMZN,
              add = T, col = 2,  display = "se", lwd = 2)

### NFLX----
plot(NFLX_weekly$Time, NFLX_weekly$Close,
     xlab = "Time", main = "Netflix trend (local regr.)",
     ylab = "", lwd = 2, type = "l")
sm.regression(NFLX_weekly$Time, NFLX_weekly$Close, h = best.h.NFLX,
              add = T, col = 2,  display = "se", lwd = 2)

### ZOOM----
plot(ZOOM_weekly$Time, ZOOM_weekly$Close,
     xlab = "Time", main = "ZOOM trend (local regr.)",
     ylab = "", lwd = 2, type = "l")
sm.regression(ZOOM_weekly$Time, ZOOM_weekly$Close, h = best.h.ZOOM,
              add = T, col = 2,  display = "se", lwd = 2)

# Splines----

#tt = as.Date(Google_Trends$Time, format = "%d/%m/%Y")
#cc= unite(tt, Google_Trends$Amazon)

sspline.AMZN <- sspline.bias.var(AMZN_weekly)
sspline.NFLX <- sspline.bias.var(NFLX_weekly)
sspline.ZOOM <- sspline.bias.var(ZOOM_weekly)

best.lambda.AMZN <- sspline.AMZN$lambda[which(sspline.AMZN$MSE
                                              == min(sspline.AMZN$MSE))]
best.lambda.NFLX <- sspline.NFLX$lambda[which(sspline.NFLX$MSE
                                              == min(sspline.NFLX$MSE))]
best.lambda.ZOOM <- sspline.ZOOM$lambda[which(sspline.ZOOM$MSE
                                              == min(sspline.ZOOM$MSE))]

### AMZN----
plot(AMZN_weekly$Time, AMZN_weekly$Close,
     xlab = "Time", main = "Amazon trend (spline)",
     ylab = "", type = "l", lwd = 2)
lines(smooth.spline(AMZN_weekly$Time, AMZN_weekly$Close,
                    lambda = best.lambda.AMZN),
      col = 2, lwd = 2)

### NFLX----
plot(NFLX_weekly$Time, NFLX_weekly$Close,
     xlab = "Time", main = "Netflix trend (spline)",
     ylab = "", type = "l", lwd = 2)
lines(smooth.spline(NFLX_weekly$Time, NFLX_weekly$Close,
                    lambda = best.lambda.NFLX),
      col = 2, lwd = 2)


### ZOOM----
plot(ZOOM_weekly$Time, ZOOM_weekly$Close,
     xlab = "Time", main = "ZOOM trend (spline)",
     ylab = "", type = "l", lwd = 2)
lines(smooth.spline(ZOOM_weekly$Time, ZOOM_weekly$Close,
                    lambda = best.lambda.ZOOM),
      col = 2, lwd = 2)

# BEST MODELS ----

### AMZN----
setNames(c(min(loess.AMZN$MSE), min(locreg.AMZN$MSE),
           min(sspline.AMZN$MSE)),
         c("Loess", "Loc. Regr.", "Spline"))
best.h.AMZN
cat("For AMAZON trends, best model is LOESS with h =", best.h.AMZN, "\n")
# best model for AMZN: LOESS, with h = 10


### NFLX----
setNames(c(min(loess.NFLX$MSE), min(locreg.NFLX$MSE),
           min(sspline.NFLX$MSE)),
         c("Loess", "Loc. Regr.", "Spline"))
best.h.NFLX
cat("For NETFLIX trends, best model is Local Regression with h =", best.h.NFLX, "\n")
# best model for NFLX: local regression, with h = 10

### ZOOM----
setNames(c(min(loess.ZOOM$MSE), min(locreg.ZOOM$MSE),
           min(sspline.ZOOM$MSE)),
         c("Loess", "Loc. Regr.", "Spline"))

best.h.ZOOM
cat("For ZOOM trends, best model is Local Regression with h =", best.h.ZOOM, "\n")
# best model for ZOOM: local regression, with h = 7

