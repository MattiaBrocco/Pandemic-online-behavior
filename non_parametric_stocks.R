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
AMZN_weekly <- read.timeseries.stocks("./data/AMZN_weekly.csv")
NFLX_weekly <- read.timeseries.stocks("./data/NFLX_weekly.csv")
ZOOM_weekly <- read.timeseries.stocks("./data/ZM_weekly.csv")
ZOOM_weekly <- backcast.ZOOM(ZOOM_weekly)

# Loess----
loess.AMZN <- loess.bias.var(AMZN_weekly$Close)
loess.NFLX <- loess.bias.var(NFLX_weekly$Close)
loess.ZOOM <- loess.bias.var(ZOOM_weekly$Close)

### AMZN----
loess.AMZN.best <- loess(AMZN_weekly$Close~seq(1, length(AMZN_weekly$Close)),
                         span = loess.AMZN$Span[which(loess.AMZN$RMSE ==
                                                        min(loess.AMZN$RMSE))],
                         degree = 2)

plot(AMZN_weekly$Time, AMZN_weekly$Close,
     type = "l", lwd = 2, xlab = "Time", ylab = "",
     main = "Amazon close price (loess)")
j.AMZN <- order(AMZN_weekly$Time)
lines(AMZN_weekly$Time[j.AMZN],loess.AMZN.best$fitted[j.AMZN],
      col = "#FF9B00", lwd = 2)


### NFLX----
loess.NFLX.best <- loess(NFLX_weekly$Close~seq(1, length(NFLX_weekly$Close)),
                         span = loess.NFLX$Span[which(loess.NFLX$RMSE ==
                                                        min(loess.NFLX$RMSE))],
                         degree = 2)

plot(NFLX_weekly$Time, NFLX_weekly$Close,
     type = "l", lwd = 2, xlab = "Time", ylab = "",
     main = "Netflix close price (loess)")
j.NFLX <- order(NFLX_weekly$Time)
lines(NFLX_weekly$Time[j.NFLX],
      loess.NFLX.best$fitted[j.NFLX], col = "#E50914", lwd = 2)


### ZOOM----
loess.ZOOM.best <- loess(ZOOM_weekly$Close~seq(1, length(ZOOM_weekly$Close)),
                         span = seq(0.1, 3,
                                    length.out = 300)
                         [which(loess.ZOOM$RMSE ==
                                  min(loess.ZOOM$RMSE))],
                         degree = 2)

plot(ZOOM_weekly$Time, ZOOM_weekly$Close, type = "l",
     lwd = 2, xlab = "Time", ylab = "",
     main = "ZOOM close price (loess)")
j.ZOOM <- order(ZOOM_weekly$Time)
lines(ZOOM_weekly$Time[j.ZOOM],
      loess.ZOOM.best$fitted[j.ZOOM], col = "#2D8CFF", lwd = 2)


# Local Regression----
locreg.AMZN <- locreg.bias.var(AMZN_weekly)
locreg.NFLX <- locreg.bias.var(NFLX_weekly)
locreg.ZOOM <- locreg.bias.var(ZOOM_weekly)

best.h.AMZN <- locreg.AMZN$h.param[which(locreg.AMZN$RMSE
                                         == min(locreg.AMZN$RMSE))]
best.h.NFLX <- locreg.NFLX$h.param[which(locreg.NFLX$RMSE
                                         == min(locreg.NFLX$RMSE))]
best.h.ZOOM <- locreg.ZOOM$h.param[which(locreg.ZOOM$RMSE
                                         == min(locreg.ZOOM$RMSE))]

### AMZN----

#### Pandemic related events exhibited ----

# - Start of the pandemic (beginning of 2020)
# - Price of the actions is not too much influenced by
#   all the subsequent waves (second - beginning of 2021,
#   third - end of April 2021, fourth - August 2021) since
#   the price kept fairly high throughout all that period.
# - Closely following the days of fifth wave
#   (beginning of maximum daily cases), there is another increase.

#### Financial related events exhibited ----
#The **fiscal year** of AMZN corresponds to the calendar year.

# - COVID is mentioned in the letter to shareholders immediately,
#   from Q1 2020. Increase in subscription is explicitly
#   blamed on home confinement (lock down).
# - Initial increase is higher than all S&P 500
#   (Source: [REUTERS](https://www.reuters.com/article/us-amazon-stocks-idUSKBN22B2ZU))
#   and it is mainly due to the leading position in retailing
#   (B2C online marketplace).
# - Prime Day 2020 (mid October) record for SME (3.5 billion $).
# - Q2 2021, strengthened partnership between AWS and Salesforce.
# - Q3 2021 agreement to stream UEFA Champions League.

plot(AMZN_weekly$Time, AMZN_weekly$Close,
     xlab = "Time", main = "Amazon close (local regr.)",
     ylab = "", lwd = 2, type = "l")
sm.regression(AMZN_weekly$Time, AMZN_weekly$Close, h = best.h.AMZN,
              add = T, col = "#FF9B00",  display = "se", lwd = 2)

### NFLX----
#### Pandemic related events exhibited----

# - Start of the pandemic (beginning of 2020)
# - Second wave (beginning of 2021) - a little bit
# - Third wave (end of April 2021)
# - Fourth wave (August 2021)
# - Fifth wave (beginning of 2023) shows a sudden increase
#   in a period of steep decline of the price.

#### Financial related events exhibited----
# The **fiscal year** of NFLX corresponds to the calendar year.

# - Decrease in stock price right before the pandemic
#   may also be connected to the launch of Disney+ (november 2019).
# - COVID is mentioned in the letter to shareholders immediately,
#   from Q1 2020. Increase in subscription is explicitly blamed
#   on home confinement (lock down).
# - Squid Game released in September 2021.
# - Q1 2022 spike during downfall may be related to the ESG
#   commitment done by Netflix (especially to fight climate change).
# - In general, the business was carried out as usual, with no
#   drastic policies and no M&A or actions in the stock market.
# - Competitors have all been struggling and the market shares
#   of top players didn't change too much from the beginning of
#   the pandemic up to its "end" (even though the market has become
#   more concentrated).

plot(NFLX_weekly$Time, NFLX_weekly$Close,
     xlab = "Time", main = "Netflix close (local regr.)",
     ylab = "", lwd = 2, type = "l")
sm.regression(NFLX_weekly$Time, NFLX_weekly$Close, h = best.h.NFLX,
              add = T, col = "#E50914",  display = "se", lwd = 2)

### ZOOM----
#### Pandemic related events exhibited----

# - Start of the pandemic (beginning of 2020)
# - Second wave (beginning of 2021)
# - Third wave (end of April 2021)
# - *RMK*: shows insensitivity to Fifth wave (beginning of 2023),
#   period of drastic increase of daily cases.

#### Financial related events exhibited----
# The **fiscal year** of ZOOM starts on February,
# 1st and ends on January, 31st of the following year.

# - In Q1 (February-April) 2020, there is the launch of Zoom Phone.
#   This may have accelerated the growth in that period.
# - Major event of 2020, is partnership with Verizon (Q2 2020)
# - Q1 2021 is the first quarter in which COVID is mentioned
#   in the report to shareholders.
# - The peak in the Q2 2021 may be associated with the launch of
#   two new products (Zoom for Home and Zoom Hardware as a Service).
# - Q1 2022 sees also the launch of the app ecosystem (Zoom Apps).

plot(ZOOM_weekly$Time, ZOOM_weekly$Close,
     xlab = "Time", main = "ZOOM close (local regr.)",
     ylab = "", lwd = 2, type = "l")
sm.regression(ZOOM_weekly$Time, ZOOM_weekly$Close, h = best.h.ZOOM,
              add = T, col = "#2D8CFF",  display = "se", lwd = 2)

# Splines----
sspline.AMZN <- sspline.bias.var(AMZN_weekly)
sspline.NFLX <- sspline.bias.var(NFLX_weekly)
sspline.ZOOM <- sspline.bias.var(ZOOM_weekly)

best.lambda.AMZN <- sspline.AMZN$lambda[which(sspline.AMZN$RMSE
                                              == min(sspline.AMZN$RMSE))]
best.lambda.NFLX <- sspline.NFLX$lambda[which(sspline.NFLX$RMSE
                                              == min(sspline.NFLX$RMSE))]
best.lambda.ZOOM <- sspline.ZOOM$lambda[which(sspline.ZOOM$RMSE
                                              == min(sspline.ZOOM$RMSE))]

### AMZN----
plot(AMZN_weekly$Time, AMZN_weekly$Close,
     xlab = "Time", main = "Amazon close (spline)",
     ylab = "", type = "l", lwd = 2)
lines(smooth.spline(AMZN_weekly$Time, AMZN_weekly$Close,
                    lambda = best.lambda.AMZN),
      col = "#FF9B00", lwd = 2)

### NFLX----
plot(NFLX_weekly$Time, NFLX_weekly$Close,
     xlab = "Time", main = "Netflix close (spline)",
     ylab = "", type = "l", lwd = 2)
lines(smooth.spline(NFLX_weekly$Time, NFLX_weekly$Close,
                    lambda = best.lambda.NFLX),
      col = "#E50914", lwd = 2)

### ZOOM----
plot(ZOOM_weekly$Time, ZOOM_weekly$Close,
     xlab = "Time", main = "ZOOM close (spline)",
     ylab = "", type = "l", lwd = 2)
lines(smooth.spline(ZOOM_weekly$Time, ZOOM_weekly$Close,
                    lambda = best.lambda.ZOOM),
      col = "#2D8CFF", lwd = 2)

# BEST MODELS ----
#X11()
#plot(sm.regression(NFLX_weekly$Time, NFLX_weekly$Close, h = best.h.ZOOM,
#                   display = "none")$estimate,
#     type = "l", lwd = 2, ylim = c(0, 700), col = "#E50914", xaxt = "n",
#     ylab = "Adjusted Close Price", xlab = "")
#lines(sm.regression(AMZN_weekly$Time, AMZN_weekly$Close, h = best.h.ZOOM,
#                    display = "none")$estimate,
#      type = "l", lwd = 2, col = "#FF9B00")
#lines(sm.regression(ZOOM_weekly$Time, ZOOM_weekly$Close, h = best.h.ZOOM,
#                    display = "none")$estimate,
#      type = "l", lwd = 2, col = "#2D8CFF")
#legend(0, 700, c("NFLX", "AMZN", "ZOOM"),
#       col = c("#E50914", "#FF9B00", "#2D8CFF"), lty = 1, lwd = 2)
#axis(1, at = seq(0, 50, length.out = 6),
#     labels = NFLX_weekly$Time[round(seq(1, 260, length.out = 6))])

### AMZN----
#### visual comparison----
plot(AMZN_weekly$Time, AMZN_weekly$Close,
     xlab = "Time", main = "AMZN - Confronting Models",
     ylab = "", lwd = 2, type = "l")

lines(smooth.spline(AMZN_weekly$Time, AMZN_weekly$Close,
                    lambda = best.lambda.AMZN),
      col = "#FFD700", lwd = 2)
sm.regression(AMZN_weekly$Time, AMZN_weekly$Close,
              h = best.h.AMZN,
              add = T, col = "#FFA500", lwd = 2)
lines(AMZN_weekly$Time[j.AMZN],loess.AMZN.best$fitted[j.AMZN],
      col = "#FF0000", lwd = 2)

legend("topleft", legend=c("Smooth splines", "Local Regression",
                            "LOESS"),
       col=c("#FFA500", "#FFD700", "#FF0000"), lty=1, cex=0.55)

####table----
round(rbind(setNames(c(min(loess.AMZN$RMSE), min(locreg.AMZN$RMSE),
                       min(sspline.AMZN$RMSE)),
                     c("Loess", "Loc. Regr.", "Spline")),
            setNames(c(min(loess.AMZN$MAPE), min(locreg.AMZN$MAPE),
                       min(sspline.AMZN$MAPE)),
                     c("Loess", "Loc. Regr.", "Spline"))), 3)
best.h.AMZN
# best model for AMZN: local regression, with h = 10


### NFLX----
#### visual comparison----
plot(NFLX_weekly$Time, NFLX_weekly$Close, type = "l",
     xlab = "Time", ylab = "",lwd = 2,
     main = "NFLX - Confronting Models")
lines(smooth.spline(NFLX_weekly$Time, NFLX_weekly$Close,
                    lambda = best.lambda.NFLX),
      col = "#FFD700", lwd = 2)
sm.regression(NFLX_weekly$Time, NFLX_weekly$Close,
              h = best.h.AMZN,
              add = T, col = "#FFA500", lwd = 2)
lines(NFLX_weekly$Time[j.NFLX], loess.NFLX.best$fitted[j.NFLX],
      col = "#FF0000", lwd = 2)
legend("topleft",
       legend=c("Smooth splines", "Local Regression", "LOESS"),
       col=c("#FFA500", "#FFD700", "#FF0000"), lty=1, cex=0.55)

####table----
round(rbind(setNames(c(min(loess.NFLX$RMSE), min(locreg.NFLX$RMSE),
                       min(sspline.NFLX$RMSE)),
                     c("Loess", "Loc. Regr.", "Spline")),
            setNames(c(min(loess.NFLX$MAPE), min(locreg.NFLX$MAPE),
                       min(sspline.NFLX$MAPE)),
                     c("Loess", "Loc. Regr.", "Spline"))), 3)
best.h.NFLX
# best model for NFLX: local regression, with h = 10

### ZOOM----
#### visual comparison----
plot(ZOOM_weekly$Time, ZOOM_weekly$Close, type = "l",
     xlab = "Time", ylab = "",lwd = 2,
     main = "ZM - Confronting Models")
lines(smooth.spline(ZOOM_weekly$Time, ZOOM_weekly$Close,
                    lambda = best.lambda.NFLX),
      col = "#FFD700", lwd = 2)
sm.regression(ZOOM_weekly$Time, ZOOM_weekly$Close,
              h = best.h.AMZN,
              add = T, col = "#FFA500", lwd = 2)
lines(ZOOM_weekly$Time[j.NFLX], loess.ZOOM.best$fitted[j.NFLX],
      col = "#FF0000", lwd = 2)
legend("topleft",
       legend=c("Smooth splines", "Local Regression", "LOESS"),
       col=c("#FFA500", "#FFD700", "#FF0000"), lty=1, cex=0.55)

####table----
round(rbind(setNames(c(min(loess.ZOOM$RMSE), min(locreg.ZOOM$RMSE),
                       min(sspline.ZOOM$RMSE)),
                     c("Loess", "Loc. Regr.", "Spline")),
            setNames(c(min(loess.ZOOM$MAPE), min(locreg.ZOOM$MAPE),
                       min(sspline.ZOOM$MAPE)),
                     c("Loess", "Loc. Regr.", "Spline"))), 3)
best.h.ZOOM
# best model for ZOOM: local regression, with h = 7



