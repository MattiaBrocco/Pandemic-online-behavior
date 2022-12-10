library(ggplot2)
library(lubridate)
library(fpp2)
library(forecast)
library(lmtest) 
# Import Data and Preprocessing--------------------------------------------------------------------
#AMAZON
AMZN_weekly = read.csv("C:\\Users\\flavi\\OneDrive\\Desktop\\UNIPD DATA SCIENCE\\SECOND YEAR\\Business Economic and Financial Data\\progetto\\data\\AMZN_weekly.csv", sep = ";")

AMZN_weekly = AMZN_weekly[, c(1, 6)]
colnames(AMZN_weekly) = c('Time', 'Close')
AMZN_weekly$Time = as.Date(AMZN_weekly$Time, format = "%d/%m/%Y")
str(AMZN_weekly)

#NETFLIX
NFLX_weekly = read.csv("C:\\Users\\flavi\\OneDrive\\Desktop\\UNIPD DATA SCIENCE\\SECOND YEAR\\Business Economic and Financial Data\\progetto\\data\\NFLX_weekly.csv", sep = ";")

NFLX_weekly = NFLX_weekly[, c(1, 6)]
colnames(NFLX_weekly) = c('Time', 'Close')
NFLX_weekly$Time = as.Date(NFLX_weekly$Time, format = "%d/%m/%Y")
str(NFLX_weekly)

#ZOOM
ZM_weekly = read.csv("C:\\Users\\flavi\\OneDrive\\Desktop\\UNIPD DATA SCIENCE\\SECOND YEAR\\Business Economic and Financial Data\\progetto\\data\\ZM_weekly.csv", sep = ";")

ZM_weekly = ZM_weekly[, c(1, 6)]
colnames(ZM_weekly) = c('Time', 'Close')
ZM_weekly$Time = as.Date(ZM_weekly$Time, format = "%d/%m/%Y")
str(ZM_weekly)

empty_data = data.frame(AMZN_weekly[1:71,1], rep(0, 71)) 
colnames(empty_data) = c('Time', 'Close')
ZM_weekly = rbind(empty_data, ZM_weekly)

# Plot with old dataset -------------------------------------------------------------------------
Close_Data = data.frame(NFLX_weekly$Time, AMZN_weekly$Close, NFLX_weekly$Close, ZM_weekly$Close)
colnames(Close_Data) = c('Time', 'Amazon', 'Netflix', 'Zoom')

ggplot() +
  geom_line(data = Close_Data, aes(x = Time, y = Amazon), size = 1, color="dark blue") +
  geom_line(data = Close_Data, aes(x = Time, y = Netflix), size = 1, color="dark red") +
  geom_line(data = Close_Data, aes(x = Time, y = Zoom), size = 1, color="light blue") +
  theme_bw()

plot(ts(Close_Data[c(2,3,4)], freq=365.25/7, start = decimal_date(as.Date("2017-12-04"))), main = "Close values")

# Amazon - Autocorrelation and LM------------------------------------------------------------------------------
acf(AMZN_weekly$Close)
#there is a trend but not a seasonality
fit1<- lm(AMZN_weekly$Close~AMZN_weekly$Time)
summary(fit1)
#not a good model
plot(AMZN_weekly$Time, AMZN_weekly$Close)
abline(fit1, col=3)
dwtest(fit1)
#dw = 0.049 
resfit1<- residuals(fit1)
plot(resfit1, xlab="time", ylab="residuals")
#we are in presence of positive autocorrelation

#another way to do the same
amzn.ts<- ts(AMZN_weekly$Close, frequency=365.25/7)
amzn.ts
ts.plot(amzn.ts, type="o")
#having a time series object I can fit a model in a very easy way
fitts<- tslm(amzn.ts~trend)
summary(fitts)

#LM with log
log_amzn = log(AMZN_weekly$Close)
acf(log_amzn)
fit1<- lm(log_amzn~AMZN_weekly$Time)
summary(fit1)
#not a good model
plot(AMZN_weekly$Time, log_amzn)
abline(fit1, col=3)
dwtest(fit1)
#dw = 0.049 
resfit1<- residuals(fit1)
plot(resfit1, xlab="time", ylab="residuals")


# Amazon - Models --------------------------------------------------------------------------------------------
 
####BM----
amzn_bm<-BM(AMZN_weekly$Close,display = T)
summary(amzn_bm)

pred_bmamzn<- predict(amzn_bm, newx=c(1:60))
pred.bminstamzn<- make.instantaneous(pred_bmamzn)

plot(AMZN_weekly$Close, type= "b",  pch=16, lty=3, cex=0.6, xlim=c(1,60))
lines(pred.bminstamzn, lwd=2, col=2)

####LOG BM----
amzn_bm_l<-BM(log_amzn,display = T)
summary(amzn_bm)

pred_bmamzn_l<- predict(amzn_bm_l, newx=c(1:60))
pred.bminstamzn_l<- make.instantaneous(pred_bmamzn_l)

plot(log_amzn, type= "b",  pch=16, lty=3, cex=0.6, xlim=c(1,60))
lines(pred.bminstamzn_l, lwd=2, col=2)

####GGM RECTANGULAR----
GGM_amzn<- GGM(AMZN_weekly$Close, prelimestimates=c(5.588426e+04, 0.001, 0.01, 1.022318e-03, 9.127817e-03))
summary(GGM_amzn)
pred_GGM_amzn<- predict(GGM_amzn, newx=c(1:60))
pred_GGM_amzn.inst<- make.instantaneous(pred_GGM_amzn)

plot(AMZN_weekly$Close, type= "b",  pch=16, lty=3, cex=0.6, xlim=c(1,60))
lines(pred_GGM_amzn.inst, lwd=2, col=2)

####ARIMA MODELS----
Acf(AMZN_weekly$Close)
Pacf(AMZN_weekly$Close)
#there is an exponential decline in Acf and there is a significant spike at lag 1 in PACF and nothing else
#so we consider an Arima(p,d,0)
arima1_amzn<- Arima(AMZN_weekly$Close, order=c(2,0,0))
fitted(arima1_amzn)

resid1_amzn<- residuals(arima1_amzn)
tsdisplay(resid1_amzn)

plot(AMZN_weekly$Close)
lines(fitted(arima1_amzn), col=2)

for1_amzn<- forecast(arima1_amzn)
plot(for1_amzn)
AIC(arima1_amzn)

auto_arima_amzn<- auto.arima(AMZN_weekly$Close)
ress = residuals(auto_arima_amzn)
Acf(ress)
AIC(auto_arima_amzn)
# AIC of auto_arima_amzn is obv lower than the "handmande" so this is the best solution

####NON PARAMETRIC MODELS----

#####Local Regression----
library(sm)
plot(AMZN_weekly$Time, AMZN_weekly$Close, type="l")
sm.regression(AMZN_weekly$Time, AMZN_weekly$Close,   h = 10, add = T, col=3,  display="se")

#####Loess----
plot(AMZN_weekly$Time, AMZN_weekly$Close, xlab="Time", ylab="Amazon close", type="l")
lo1 <- loess.smooth(AMZN_weekly$Time, AMZN_weekly$Close, span=0.2)
lines(lo1, col=4)
#vedi meglio

#####Regression splines----
library(splines)
plot(AMZN_weekly$Time, AMZN_weekly$Close, xlab="Time", ylab="Amazon close")
m1<-lm(AMZN_weekly$Close~bs(AMZN_weekly$Time, df=15, degree=3)) 
xxx<-seq(min(AMZN_weekly$Time),max(AMZN_weekly$Time),length=262)
fit1<-predict(m1, data.frame(x=xxx))
lines(xxx,fit1,col=2)

#####Smoothing splines----
#0.0001 o 0.00001?
plot(AMZN_weekly$Time, AMZN_weekly$Close, xlab="Time", ylab="Amazon close")
s <- smooth.spline(AMZN_weekly$Time, AMZN_weekly$Close, lambda=0.0001)
lines(s, col=2)

