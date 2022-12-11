library(ggplot2)
library(lubridate)
library(fpp2)
library(forecast)
library(lmtest) 
library(DIMORA)
library(gam)

# Import Data and Preprocessing--------------------------------------------------------------------
#AMAZON
AMZN_weekly = read.csv("AMZN_weekly.csv", sep=";")
AMZN_weekly = AMZN_weekly[, c(1, 6)]
colnames(AMZN_weekly) = c('Time', 'Close')
AMZN_weekly$Time = as.Date(AMZN_weekly$Time, format = "%d/%m/%Y")
str(AMZN_weekly)
AMZN = ts(AMZN_weekly$Close, freq=52, start=decimal_date(ymd("2017-12-04")))
str(AMZN)
boxplot(AMZN, col="blue")
AMZN_weekly_2 = AMZN_weekly[2:261,]
AMZN_2 = ts(AMZN_weekly_2$Close, freq=52, start=decimal_date(ymd("2017-12-11")))


#NETFLIX
NFLX_weekly = read.csv("NFLX_weekly.csv", sep=";")
NFLX_weekly = NFLX_weekly[, c(1, 6)]
colnames(NFLX_weekly) = c('Time', 'Close')
NFLX_weekly$Time = as.Date(NFLX_weekly$Time, format = "%d/%m/%Y")
str(NFLX_weekly)
NFLX = ts(NFLX_weekly$Close, freq=52, start=decimal_date(ymd("2017-12-04")))
boxplot(NFLX, col="dark red")

#ZOOM
ZM_weekly = read.csv("ZM_weekly.csv", sep=";")
ZM_weekly = ZM_weekly[, c(1, 6)]
colnames(ZM_weekly) = c('Time', 'Close')
ZM_weekly$Time = as.Date(ZM_weekly$Time, format = "%d/%m/%Y")
str(ZM_weekly)
empty_data = data.frame(AMZN_weekly[1:71,1], rep(0, 71)) 
colnames(empty_data) = c('Time', 'Close')
ZM_weekly = rbind(empty_data, ZM_weekly)
ZM = ts(ZM_weekly$Close, freq=52, start=decimal_date(ymd("2017-12-04")))
boxplot(ZM_weekly$Close, col="light blue")

#GoogleTrends Data
Google_Trends <- read.csv("Google Trends Data.csv", na = "0", skip = 1)
colnames(Google_Trends) <- c('Time','Amazon','Netflix', 'Zoom')
plot(ts(Google_Trends[2:4], freq=365.25/7, start=decimal_date(ymd("2017-12-10"))), main = "Google Trends")
amazon = ts(Google_Trends$Amazon, freq=52, start=decimal_date(ymd("2017-12-10")))
plot(amazon)
netflix = ts(Google_Trends$Netflix, freq=52, start=decimal_date(ymd("2017-12-10")))
z = as.numeric(Google_Trends$Zoom)
z[is.na(z)] <- 0
zoom = ts(z, freq=52, start=decimal_date(ymd("2017-12-10")))

#Plot all stock dataset
Close_Data = data.frame(NFLX_weekly$Time, AMZN_weekly$Close, NFLX_weekly$Close, ZM_weekly$Close)
Close_Data_ts = data.frame(AMZN, NFLX, ZM)
colnames(Close_Data) = c('Time', 'Amazon', 'Netflix', 'Zoom')

ggplot() +
  geom_line(data = Close_Data, aes(x = Time, y = Amazon, color="Amazon"), size = 1) +
  geom_line(data = Close_Data, aes(x = Time, y = Netflix, color="Netflix"), size = 1) +
  geom_line(data = Close_Data, aes(x = Time, y = Zoom, color="Zoom"), size = 1) +
  theme_bw() +
  labs(x="Time", y="Close value", color = "Legend") +
  scale_colour_manual("", 
                      breaks = c("Amazon", "Netflix", "Zoom"),
                      values = c("dark blue", "dark red", "light blue"))

plot(ts(Close_Data[c(2,3,4)], freq=365.25/7, start = decimal_date(as.Date("2017-12-04"))), main = "Close values")

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

plot(AMZN_weekly$Close, type= "b",  pch=16, lty=3, cex=0.6, xlim=c(1,270), ylab="AMZN_Close")
lines(pred.bminstamzn, lwd=2, col=2)

####GGM RECTANGULAR----
GGM_amzn<- GGM(AMZN, prelimestimates=c(5.588426e+04, 0.001, 0.01, 1.022318e-03, 9.127817e-03))
summary(GGM_amzn)

pred_GGM_amzn<- predict(GGM_amzn, newx=c(1:270))
pred_GGM_amzn.inst<- make.instantaneous(pred_GGM_amzn)

plot(AMZN_weekly$Close, type= "b",  pch=16, lty=3, cex=0.6, xlim=c(1,270), ylab="AMZN_Close")
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
library(sm)
plot(AMZN_weekly$Time, AMZN_weekly$Close, type="l", xlab="Time", ylab="Amazon close", lwd=2)
sm.regression(AMZN_weekly$Time, AMZN_weekly$Close, h = 10, add = T, col=2,  display="se", lwd=2, ngrid=200)

#####Loess----
plot(AMZN_weekly$Time, AMZN_weekly$Close, xlab="Time", ylab="Amazon close", type="l", lwd = 2)
lo1 <- loess.smooth(AMZN_weekly$Time, AMZN_weekly$Close, span=0.2)
lines(lo1, col=2, lwd = 2)
#vedi meglio

#####Regression splines----
library(splines)
plot(AMZN_weekly$Time, AMZN_weekly$Close, xlab="Time", ylab="Amazon close", type="l", lwd = 2)
m1<-lm(AMZN_weekly$Close~bs(AMZN_weekly$Time, df=15, degree=3)) 
xxx<-seq(min(AMZN_weekly$Time),max(AMZN_weekly$Time),length=262)
fit1<-predict(m1, data.frame(x=xxx))
lines(xxx,fit1,col=2, lwd = 2)

#####Smoothing splines----
#0.0001 o 0.00001?
plot(AMZN_weekly$Time, AMZN_weekly$Close, xlab="Time", ylab="Amazon close", type="l", lwd = 2)
s <- smooth.spline(AMZN_weekly$Time, AMZN_weekly$Close, lambda=0.0001)
lines(s, col=2, lwd=2)
