library(sm)
library(gam)
library(fpp2)
library(lmtest)
library(DIMORA)
library(splines)
library(ggplot2)
library(forecast)
library(lubridate)

source("support.R")

# Import Data and Preprocessing--------------------------------------------------------------------
#AMAZON

AMZN_weekly <- read.timeseries.stocks("./yahoo_data/AMZN_weekly.csv")
#str(AMZN_weekly)
AMZN <- get.tseries(AMZN_weekly, "2017-12-04")
#str(AMZN)
boxplot(AMZN, col="blue")
AMZN_weekly_2 <- AMZN_weekly[2:261,]
AMZN_2 <- get.tseries(AMZN_weekly, "2017-12-11")

#NETFLIX
NFLX_weekly <- read.timeseries.stocks("./yahoo_data/NFLX_weekly.csv")
NFLX <- get.tseries(NFLX_weekly, "2017-12-04")
boxplot(NFLX, col="dark red")

#ZOOM
ZM_weekly = read.timeseries.stocks("./yahoo_data/ZM_weekly.csv")
empty_data = data.frame(AMZN_weekly[1:71,1], rep(0, 71)) 
colnames(empty_data) = c('Time', 'Close')
ZM_weekly = rbind(empty_data, ZM_weekly)
ZM = get.tseries(ZM_weekly, "2017-12-04")
boxplot(ZM_weekly$Close, col="light blue")

#GoogleTrends Data
Google_Trends <- read.timeseries.google("./gtrend_data/Google Trends Data.csv")

plot(ts(Google_Trends[2:4], freq=365.25/7,
        start = decimal_date(ymd("2017-12-10"))),
     main = "Google Trends")

amazon = get.ts.google(Google_Trends$Amazon, "2017-12-10")
netflix = get.ts.google(Google_Trends$Netflix, "2017-12-10")
zoom = get.ts.google(Google_Trends$Netflix, "2017-12-10")

plot(amazon)

#Plot all stock dataset
gg.comparison1()
gg.comparison2()

# Amazon - Autocorrelation and TSLM------------------------------------------------------------------------------

acf(AMZN)
#there is a trend but not a seasonality

#TSLM
fit1<- tslm(AMZN~trend) 
summary(fit1)
#not a good model in term of R squared
fitted(fit1)
plot(AMZN)
lines(fitted(fit1), col=2)
plot(residuals(fit1))
#positive autocorrelation

#TSLM with NFLX
fit2<- tslm(AMZN~ trend+NFLX) 
summary(fit2)
fitted(fit2)
plot(AMZN)
lines(fitted(fit2), col=2)
plot(residuals(fit2))

#TSLM with ZM
fit3<- tslm(AMZN~ trend+ZM) 
summary(fit3)
fitted(fit3)
plot(AMZN)
lines(fitted(fit3), col=2)
plot(residuals(fit3))

#TSLM with NFLX and ZM
fit4<- tslm(AMZN~ trend+NFLX+ZM) 
summary(fit4)
fitted(fit4)
plot(AMZN)
lines(fitted(fit4), col=2)
plot(residuals(fit4))

#TSLM with google_trends
fit5<- tslm(AMZN_2~ trend+amazon)
summary(fit5)
fitted(fit5)
plot(amazon)
plot(AMZN)
lines(fitted(fit5), col=2)
plot(residuals(fit5))

# Amazon - Models --------------------------------------------------------------------------------------------

####BM----
amzn_bm<-BM(AMZN_weekly$Close,display = T)
summary(amzn_bm)

pred_bmamzn<- predict(amzn_bm, newx=c(1:270))
pred.bminstamzn<- make.instantaneous(pred_bmamzn)

plot(AMZN_weekly$Close, type= "b",  pch=16, lty=3, cex=0.6,
     xlim=c(1,270), ylab="AMZN_Close")
lines(pred.bminstamzn, lwd=2, col=2)

####GGM RECTANGULAR----
GGM_amzn<- GGM(AMZN, prelimestimates=c(5.588426e+04, 0.001, 0.01,
                                       1.022318e-03, 9.127817e-03))
summary(GGM_amzn)

pred_GGM_amzn<- predict(GGM_amzn, newx=c(1:270))
pred_GGM_amzn.inst<- make.instantaneous(pred_GGM_amzn)

plot(AMZN_weekly$Close, type= "b",  pch=16, lty=3, cex=0.6, xlim=c(1,270),
     ylab="AMZN_Close")
lines(pred_GGM_amzn.inst, lwd=2, col=2)

####ARIMA MODELS----
Acf(AMZN)
Pacf(AMZN)
#there is an exponential decline in Acf and there is a significant spike at lag 1 in PACF and nothing else
#so we consider an Arima(p,d,0)
arima1_amzn<- Arima(AMZN, order=c(2,1,0))
fitted(arima1_amzn)

resid1_amzn<- residuals(arima1_amzn)
tsdisplay(resid1_amzn)

plot(AMZN, ylab="AMZN_Close", type="l")
lines(fitted(arima1_amzn), col=2)

for1_amzn<- forecast(arima1_amzn, h = 10)
plot(for1_amzn)
AIC(arima1_amzn)

auto_arima_amzn<- auto.arima(AMZN)
ress = residuals(auto_arima_amzn)
Acf(ress)
AIC(auto_arima_amzn)

plot(AMZN, ylab="AMZN_Close", type="l")
lines(fitted(auto_arima_amzn), col=2)
# AIC of auto_arima_amzn is obv lower than the "handmande" 


####NON PARAMETRIC MODELS----

#####Local Regression----
plot(AMZN_weekly$Time, AMZN_weekly$Close, type="l", xlab="Time",
     ylab="Amazon close", lwd=2)
sm.regression(AMZN_weekly$Time, AMZN_weekly$Close, h = 10,
              add = T, col=2,  display="se", lwd=2, ngrid=200)

#####Loess----
plot(AMZN_weekly$Time, AMZN_weekly$Close, xlab="Time",
     ylab="Amazon close", type="l", lwd = 2)
lo1 <- loess.smooth(AMZN_weekly$Time, AMZN_weekly$Close, span=0.2)
lines(lo1, col=2, lwd = 2)
#vedi meglio

#####Regression splines----
plot(AMZN_weekly$Time, AMZN_weekly$Close, xlab="Time",
     ylab="Amazon close", type="l", lwd = 2)
m1<-lm(AMZN_weekly$Close~bs(AMZN_weekly$Time, df=15, degree=3)) 
xxx<-seq(min(AMZN_weekly$Time),max(AMZN_weekly$Time),length=262)
fit1<-predict(m1, data.frame(x=xxx))
lines(xxx,fit1,col=2, lwd = 2)

#####Smoothing splines----
#0.0001 o 0.00001?
plot(AMZN_weekly$Time, AMZN_weekly$Close,
     xlab="Time", ylab="Amazon close", type="l", lwd = 2)
s <- smooth.spline(AMZN_weekly$Time, AMZN_weekly$Close, lambda=0.0001)
lines(s, col=2, lwd=2)


# NETFLIX - Models --------------------------------------------------------------------------------------------
#TSLM
fit1_nflx <- tslm(NFLX~trend) 
summary(fit1_nflx)

#TSLM with Amazon
# - Adding season produces more seasonal residuals
#   residuals with season are modeled with SARIMA(1,0,0)(0,0,1)[52]
# - Using ZOOM instead, worsens the fit.
fit2_nflx <- tslm(NFLX~ trend+AMZN)
summary(fit2_nflx)
plot(NFLX)
lines(fitted(fit2_nflx), col=2)

fit2_res_autoarima <- auto.arima(residuals(fit2_nflx)) # -> ARIMA(1,0,1) w/ zero mean
summary(fit2_res_autoarima) 
plot(residuals(fit2_nflx),
     ylab="Residual fit2", type = "l")
lines(fitted(fit2_res_autoarima), col = 2)


nflx_bm<-BM(NFLX, prelimestimates = c(1000, 0.001, 0.1),
            display = T)
# Exponential shocks make the fit worse. Any combination of
# parameter sign for rectangular shock (and thus also GGM) gives
# error related to the Cholesky decomposition of the hessian (CHECK !)
summary(nflx_bm)

####ARIMA MODELS----
Acf(NFLX)
Pacf(NFLX)
# there is an exponential decline in Acf and there is
# a significant spike at lag 1 in PACF and nothing else
# so we would opt for an Arima(p,d,0) - same as AMZN
auto_arima_nflx<- auto.arima(NFLX)
summary(auto_arima_nflx)
res_nflx_aa = residuals(auto_arima_nflx)
Acf(res_nflx_aa)
AIC(auto_arima_nflx)

plot(NFLX, ylab="NFLX_Close", type="l") # almost perfect fit !
lines(fitted(auto_arima_nflx), col=2)

####NON PARAMETRIC MODELS----

#####Local Regression----
plot(NFLX_weekly$Time, NFLX_weekly$Close, type="l", xlab="Time",
     ylab="Netflix close", lwd=2)
locreg_nflx <- sm.regression(NFLX_weekly$Time, NFLX_weekly$Close,
                             h = 10, add = T, col=2,  display="se",
                             lwd = 2, ngrid = 262)
# MSE:
sum((NFLX_weekly$Close - locreg_nflx$estimate)^2)/length(NFLX_weekly$Time)

#####Regression splines----
plot(NFLX_weekly$Time, NFLX_weekly$Close, xlab="Time",
     ylab="Amazon close", type="l", lwd = 2)
reg_spline_nflx <- lm(NFLX_weekly$Close~bs(NFLX_weekly$Time,
                                           df = 30, degree = 3)) 
seq_nflx <-seq(min(NFLX_weekly$Time),
               max(NFLX_weekly$Time), length = 260)

fit_regsp_nflx <- predict(reg_spline_nflx,
                          data.frame(x = seq_nflx))
lines(seq_nflx, fit_regsp_nflx, col = 2, lwd = 2)

AIC(reg_spline_nflx)

#####Smoothing splines----
#0.0001 o 0.00001?
plot(NFLX_weekly$Time, NFLX_weekly$Close,
     xlab="Time", ylab="Netflix close", type="l", lwd = 2)
nflx_ss <- smooth.spline(NFLX_weekly$Time, NFLX_weekly$Close,
                         lambda=0.00005)
lines(nflx_ss, col=2, lwd=2)
# MSE:
sum((NFLX_weekly$Close - nflx_ss$y)^2)/length(NFLX_weekly$Time)
