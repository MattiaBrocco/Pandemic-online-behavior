library(sm)
library(gam)
library(fpp2)
library(lmtest)
library(DIMORA)
library(splines)
library(ggplot2)
library(forecast)
library(lubridate)
library(Metrics)


# Mattia --------------------------------------------------------------------
source("support.R")
AMZN_weekly <- read.timeseries.stocks("./yahoo_data/AMZN_weekly.csv")
AMZN <- get.tseries(AMZN_weekly, "2017-12-04")
AMZN_weekly_2 <- AMZN_weekly[2:261,]
AMZN_2 <- get.tseries(AMZN_weekly, "2017-12-11")
typeof(AMZN_weekly$Time)

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

# Import Data --------------------------------------------------------------------
#AMAZON
AMZN_weekly = read.csv("./yahoo_data/AMZN_weekly.csv", sep=";")
AMZN_weekly = AMZN_weekly[2:261, c(1, 6)]
colnames(AMZN_weekly) = c('Time', 'Close')
AMZN_weekly$Time = as.Date(AMZN_weekly$Time, "%d/%m/%Y")
AMZN = ts(AMZN_weekly$Close, freq=52, start=decimal_date(ymd("2017-12-11")))

#NETFLIX
NFLX_weekly = read.csv("./yahoo_data/NFLX_weekly.csv", sep=";")
NFLX_weekly = NFLX_weekly[2:261, c(1, 6)]
colnames(NFLX_weekly) = c('Time', 'Close')
NFLX_weekly$Time = as.Date(NFLX_weekly$Time, format = "%d/%m/%Y")
NFLX = ts(NFLX_weekly$Close, freq=52, start=decimal_date(ymd("2017-12-11")))

#ZOOM
ZM_weekly_c = read.csv("./yahoo_data/ZM_weekly.csv", sep=";")
ZM_weekly_c = ZM_weekly_c[1:190, c(1, 6)]
colnames(ZM_weekly_c) = c('Time', 'Close')
ZM_weekly_c$Time = as.Date(ZM_weekly_c$Time, format = "%d/%m/%Y")
ZM_c = ts(ZM_weekly_c$Close, freq=52, start =decimal_date(ymd("2019-04-15")))
# Reverse time
revZM <- ts(rev(ZM_c), frequency=52, start=decimal_date(ymd("2019-04-15")))
# Forecast
auto_arima_zm<- auto.arima(revZM, D = 1)
auto_arima_zm
fc <- forecast(auto_arima_zm, h=70)
plot(fc)
# Reverse time again
fc$mean <- ts(rev(fc$mean),end=tsp(ZM_c)[1] - 1/52, frequency=52)
fc$upper <- fc$upper[70:1,]
fc$lower <- fc$lower[70:1,]
fc$x <- ZM_c
# Plot result
plot(fc, xlim=c(tsp(ZM_c)[1]-70/52, tsp(ZM_c)[2]))
# Final time series
backcasting_ZM = fc$mean %>% as.vector
bkzm = data.frame(NFLX_weekly[1:70,1], backcasting_ZM) 
colnames(bkzm) = c('Time', 'Close')
ZM_weekly = rbind(bkzm, ZM_weekly_c)
ZM_weekly$Close = pmax(ZM_weekly$Close, 0)
ZM = ts(ZM_weekly$Close, freq=52, start=decimal_date(ymd("2017-12-11")))

#GoogleTrends Data
Google_Trends <- read.csv("./gtrend_data/Google Trends Data.csv", na = "0", skip = 1)
colnames(Google_Trends) <- c('Time','Amazon','Netflix', 'Zoom')
plot(ts(Google_Trends[2:4], freq=365.25/7, start=decimal_date(ymd("2017-12-11"))), main = "Google Trends")
amazon = ts(Google_Trends$Amazon, freq=52, start=decimal_date(ymd("2017-12-11")))
netflix = ts(Google_Trends$Netflix, freq=52, start=decimal_date(ymd("2017-12-11")))
z = as.numeric(Google_Trends$Zoom)
z[is.na(z)] <- 0
zoom = ts(z, freq=52, start=decimal_date(ymd("2017-12-11")))

# First plots --------------------------------------------------------------------
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

acf(AMZN, lag.max=120)

plot(decompose(AMZN))
ggseasonplot(AMZN)
ggseasonplot(AMZN, polar=TRUE)
#there is a trend but not a seasonality(?)

#TSLM
tslm_amzn<- tslm(AMZN~trend)
summary(tslm_amzn)
#not a good model in term of R squared
plot(AMZN)
lines(fitted(tslm_amzn), col=2)
plot(residuals(tslm_amzn))
#positive autocorrelation
rmse_amzn_tslm = rmse(AMZN, fitted(tslm_amzn))
rmse_amzn_tslm

#TSLM with seas
tslm_s_amzn<- tslm(AMZN~trend+season) 
summary(tslm_s_amzn)
#not a good model in term of R squared
plot(AMZN)
lines(fitted(tslm_s_amzn), col=2)
plot(residuals(tslm_s_amzn))
#positive autocorrelation
rmse_amzn_tslm_s = rmse(AMZN, fitted(tslm_s_amzn))
rmse_amzn_tslm_s

#TSLM with NFLX
tslm_n_amzn<- tslm(AMZN~trend+NFLX) 
summary(tslm_n_amzn)
plot(AMZN)
lines(fitted(tslm_n_amzn), col=2)
plot(residuals(tslm_n_amzn))
rmse_amzn_tslm_n = rmse(AMZN, fitted(tslm_n_amzn))
rmse_amzn_tslm_n

#TSLM with ZM
tslm_z_amzn<- tslm(AMZN~trend+ZM) 
summary(tslm_z_amzn)
plot(AMZN)
lines(fitted(tslm_z_amzn), col=2)
plot(residuals(tslm_z_amzn))
rmse_amzn_tslm_z = rmse(AMZN, fitted(tslm_z_amzn))
rmse_amzn_tslm_z

#TSLM with NFLX and ZM
tslm_n_z_amzn<- tslm(AMZN~trend+ZM+NFLX) 
summary(tslm_n_z_amzn)
plot(AMZN)
lines(fitted(tslm_n_z_amzn), col=2)
plot(residuals(tslm_n_z_amzn))
rmse_amzn_tslm_n_z = rmse(AMZN, fitted(tslm_n_z_amzn))
rmse_amzn_tslm_n_z

#TSLM with google_trends
tslm_gtamzn_amzn<- tslm(AMZN~trend+amazon) 
summary(tslm_gtamzn_amzn)
plot(AMZN)
lines(fitted(tslm_gtamzn_amzn), col=2)
plot(residuals(tslm_gtamzn_amzn))
rmse_amzn_tslm_gtamzn = rmse(AMZN, fitted(tslm_gtamzn_amzn))
rmse_amzn_tslm_gtamzn

# Amazon - Models --------------------------------------------------------------------------------------------

####ARIMA MODELS----
Acf(AMZN)
Pacf(AMZN)
#there is an exponential decline in Acf and there is a significant spike at lag 1 in PACF and nothing else
#so we consider an Arima(p,d,0)
arima1_amzn<- Arima(AMZN, order=c(1,1,0))
resid1_amzn<- residuals(arima1_amzn)
tsdisplay(resid1_amzn)
plot(AMZN, ylab="AMZN_Close", type="l")
lines(fitted(arima1_amzn), col=2)
for1_amzn<- forecast(arima1_amzn, h = 10)
plot(for1_amzn)
aic_amzn_arima = AIC(arima1_amzn)
aic_amzn_arima
rmse_amzn_arima = rmse(AMZN, fitted(arima1_amzn))
rmse_amzn_arima

#AUTO_ARIMA with s
auto_arima_s_amzn<- auto.arima(AMZN, D = 1)
auto_arima_s_amzn
plot(AMZN, ylab="AMZN_Close", type="l")
lines(fitted(auto_arima_s_amzn), col=2)
ress = residuals(auto_arima_s_amzn)
Acf(ress)
aic_amzn_autoarima_s = AIC(auto_arima_s_amzn)
aic_amzn_autoarima_s
rmse_amzn_autoarima_s = rmse(AMZN, fitted(auto_arima_s_amzn))
rmse_amzn_autoarima_s
#AIC of auto_arima_s_amzn is obv lower than the "handmande" 

#AUTO_ARIMA
auto_arima_amzn<- auto.arima(AMZN)
auto_arima_amzn
plot(AMZN, ylab="AMZN_Close", type="l")
lines(fitted(auto_arima_amzn), col=2)
ress = residuals(auto_arima_amzn)
Acf(ress)
aic_amzn_autoarima = AIC(auto_arima_amzn)
aic_amzn_autoarima
rmse_amzn_autoarima = rmse(AMZN, fitted(auto_arima_amzn))
rmse_amzn_autoarima
#AIC of auto_arima_amzn is obv lower than the "handmande" 

#SARIMA
sarima1_amzn<- Arima(AMZN, order=c(0,1,0), seasonal=c(1,1,0))
resid1_amzn<- residuals(sarima1_amzn)
plot(AMZN, ylab="AMZN_Close", type="l")
lines(fitted(sarima1_amzn), col=2)
for1_s_amzn<- forecast(sarima1_amzn, h = 10)
plot(for1_s_amzn)
aic_amzn_sarima = AIC(sarima1_amzn)
aic_amzn_sarima
rmse_amzn_sarima = rmse(AMZN, fitted(sarima1_amzn))
rmse_amzn_sarima

####NON PARAMETRIC MODELS----

#####Local Regression----
plot(AMZN_weekly$Time, AMZN_weekly$Close, type="l", xlab="Time", ylab="Amazon close", lwd=2)
loc_r_amzn = sm.regression(AMZN_weekly$Time, AMZN_weekly$Close, h = 10, add = T, col=2,  display="se", lwd=2, ngrid=260)
rmse_amzn_locr = rmse(AMZN_weekly$Close, loc_r_amzn$estimate)
rmse_amzn_locr

#####Loess----
plot(AMZN_weekly$Time, AMZN_weekly$Close, xlab="Time", ylab="Amazon close", type="l", lwd = 2)
lo1 <- loess.smooth(AMZN_weekly$Time, AMZN_weekly$Close, span=0.2)
lines(lo1, col=2, lwd = 2)
#vedi meglio

#####Regression splines----
plot(AMZN_weekly$Time, AMZN_weekly$Close, xlab="Time", ylab="Amazon close", type="l", lwd = 2)
m1<-lm(AMZN_weekly$Close~bs(AMZN_weekly$Time, df=15, degree=3)) 
xxx<-seq(min(AMZN_weekly$Time),max(AMZN_weekly$Time),length=260)
regspl_amzn <-predict(m1, data.frame(x=xxx))
lines(xxx,regspl_amzn,col=2, lwd = 2)
rmse_amzn_regspl = rmse(AMZN_weekly$Close, regspl_amzn)
rmse_amzn_regspl

#####Smoothing splines----
plot(AMZN_weekly$Time, AMZN_weekly$Close, xlab="Time", ylab="Amazon close", type="l", lwd = 2)
smospl_amzn <- smooth.spline(AMZN_weekly$Time, AMZN_weekly$Close, lambda=0.00001)
lines(smospl_amzn, col=2, lwd=2)
rmse_amzn_smospl = rmse(AMZN_weekly$Close, smospl_amzn$y)
rmse_amzn_smospl

####GAM----
tt<- (1:length(AMZN))

g0 <- gam(AMZN~(tt)+(NFLX)+(ZM)+(amazon))
summary(g0)
plot(g0, se=T)
aic_amzn_gam = AIC(g0)
aic_amzn_gam
rmse_amzn_gam = rmse(AMZN, g0$fitted.values)
rmse_amzn_gam

g1 <- gam(AMZN~s(tt)+s(NFLX))
summary(g1)
plot(g1, se=T)
aic_amzn_gam_n = AIC(g1)
aic_amzn_gam_n
rmse_amzn_gam_n = rmse(AMZN, g1$fitted.values)
rmse_amzn_gam_n

g2 <- gam(AMZN~s(tt)+s(NFLX)+s(ZM))
summary(g2)
plot(g2, se=T)
aic_amzn_gam_nz = AIC(g2)
aic_amzn_gam_nz
rmse_amzn_gam_nz = rmse(AMZN, g2$fitted.values)
rmse_amzn_gam_nz

g3 <- gam(AMZN~s(tt)+s(NFLX)+s(ZM)+s(amazon))
summary(g3)
plot(g3, se=T)
aic_amzn_gam_nza = AIC(g3)
aic_amzn_gam_nza
rmse_amzn_gam_nza = rmse(AMZN, g3$fitted.values)
rmse_amzn_gam_nza

g4 <- gam(AMZN~lo(tt)+lo(NFLX))
summary(g4)
plot(g4, se=T)
aic_amzn_gam_lo_n = AIC(g4)
aic_amzn_gam_lo_n
rmse_amzn_gam_lo_n = rmse(AMZN, g4$fitted.values)
rmse_amzn_gam_lo_n

g5 <- gam(AMZN~lo(tt)+lo(NFLX)+lo(ZM))
summary(g5)
plot(g5, se=T)
aic_amzn_gam_lo_n_z = AIC(g5)
aic_amzn_gam_lo_n_z
rmse_amzn_gam_lo_n_z = rmse(AMZN, g5$fitted.values)
rmse_amzn_gam_lo_n_z


# Netflix - Autocorrelation and TSLM------------------------------------------------------------------------------

acf(NFLX, lag.max=120)

plot(decompose(NFLX))
ggseasonplot(NFLX)
ggseasonplot(NFLX, polar=TRUE)
#there is a trend but not a seasonality(?)

#TSLM
tslm_nflx<- tslm(NFLX~trend)
summary(tslm_nflx)
#not a good model in term of R squared
plot(NFLX)
lines(fitted(tslm_nflx), col=2)
plot(residuals(tslm_nflx))
#positive autocorrelation
rmse_nflx_tslm = rmse(NFLX, fitted(tslm_nflx))
rmse_nflx_tslm

#TSLM with seas
tslm_s_nflx<- tslm(NFLX~trend+season) 
summary(tslm_s_nflx)
#not a good model in term of R squared
plot(NFLX)
lines(fitted(tslm_s_nflx), col=2)
plot(residuals(tslm_s_nflx))
#positive autocorrelation
rmse_nflx_tslm_s = rmse(AMZN, fitted(tslm_s_nflx))
rmse_nflx_tslm_s

#TSLM with AMZN
tslm_a_nflx<- tslm(NFLX~trend+AMZN) 
summary(tslm_a_nflx)
plot(NFLX)
lines(fitted(tslm_a_nflx), col=2)
plot(residuals(tslm_a_nflx))
rmse_nflx_tslm_a = rmse(NFLX, fitted(tslm_a_nflx))
rmse_nflx_tslm_a

#TSLM with ZM
tslm_z_nflx<- tslm(NFLX~trend+ZM) 
summary(tslm_z_nflx)
plot(NFLX)
lines(fitted(tslm_z_nflx), col=2)
plot(residuals(tslm_z_nflx))
rmse_nflx_tslm_z = rmse(NFLX, fitted(tslm_z_nflx))
rmse_nflx_tslm_z

#TSLM with AMZN and ZM
tslm_a_z_nflx<- tslm(NFLX~trend+ZM+AMZN) 
summary(tslm_a_z_nflx)
plot(NFLX)
lines(fitted(tslm_a_z_nflx), col=2)
plot(residuals(tslm_a_z_nflx))
rmse_nflx_tslm_a_z = rmse(NFLX, fitted(tslm_a_z_nflx))
rmse_nflx_tslm_a_z

#TSLM with google_trends
tslm_gtnflx_nflx<- tslm(NFLX~trend+netflix) 
summary(tslm_gtnflx_nflx)
plot(NFLX)
lines(fitted(tslm_gtnflx_nflx), col=2)
plot(residuals(tslm_gtnflx_nflx))
rmse_nflx_tslm_gtnflx = rmse(NFLX, fitted(tslm_gtnflx_nflx))
rmse_nflx_tslm_gtnflx

# Netflix - Models --------------------------------------------------------------------------------------------
####ARIMA MODELS----
Acf(NFLX)
Pacf(NFLX)
#there is an exponential decline in Acf and there is a significant spike at lag 1 in PACF and nothing else
#so we consider an Arima(p,d,0)
arima1_nflx<- Arima(NFLX, order=c(1,1,0))
resid1_nflx<- residuals(arima1_nflx)
tsdisplay(resid1_nflx)
plot(NFLX, ylab="NFLX_Close", type="l")
lines(fitted(arima1_nflx), col=2)
for1_nflx<- forecast(arima1_nflx, h = 10)
plot(for1_nflx)
aic_nflx_arima = AIC(arima1_nflx)
aic_nflx_arima
rmse_nflx_arima = rmse(NFLX, fitted(arima1_nflx))
rmse_nflx_arima

#AUTO_ARIMA
auto_arima_nflx<- auto.arima(NFLX, D=1)
auto_arima_nflx
plot(NFLX, ylab="NFLX_Close", type="l")
lines(fitted(auto_arima_nflx), col=2)
for2_nflx<- forecast(auto_arima_nflx, h = 10)
plot(for2_nflx)
ress = residuals(auto_arima_nflx)
Acf(ress)
aic_nflx_autoarima = AIC(auto_arima_nflx)
aic_nflx_autoarima
rmse_nflx_autoarima = rmse(NFLX, fitted(auto_arima_nflx))
rmse_nflx_autoarima
#AIC of auto_arima_amzn is obv lower than the "handmande" 

#SARIMA
sarima1_nflx<- Arima(NFLX, order=c(0,1,0), seasonal=c(1,1,0))
resid1_nflx<- residuals(sarima1_nflx)
plot(NFLX, ylab="NFLX_Close", type="l")
lines(fitted(sarima1_nflx), col=2)
for1_s_nflx<- forecast(sarima1_nflx, h = 10)
plot(for1_s_nflx)
aic_nflx_sarima = AIC(sarima1_nflx)
aic_nflx_sarima
rmse_nflx_sarima = rmse(NFLX, fitted(sarima1_nflx))
rmse_nflx_sarima


####NON PARAMETRIC MODELS----

#####Local Regression----
plot(NFLX_weekly$Time, NFLX_weekly$Close, type="l", xlab="Time", ylab="Netflix close", lwd=2)
loc_r_nflx = sm.regression(NFLX_weekly$Time, NFLX_weekly$Close, h = 10, add = T, col=2,  display="se", lwd=2, ngrid=260)
rmse_nflx_locr = rmse(NFLX_weekly$Close, loc_r_nflx$estimate)
rmse_nflx_locr

#####Loess----
plot(NFLX_weekly$Time, NFLX_weekly$Close, xlab="Time", ylab="Netflix close", type="l", lwd = 2)
lo1 <- loess.smooth(NFLX_weekly$Time, NFLX_weekly$Close, span=0.2)
lines(lo1, col=2, lwd = 2)
#vedi meglio

#####Regression splines----
plot(NFLX_weekly$Time, NFLX_weekly$Close, xlab="Time", ylab="Netflix close", type="l", lwd = 2)
m1<-lm(NFLX_weekly$Close~bs(NFLX_weekly$Time, df=20, degree=3)) 
xxx<-seq(min(NFLX_weekly$Time),max(NFLX_weekly$Time),length=260)
regspl_nflx <-predict(m1, data.frame(x=xxx))
lines(xxx,regspl_nflx,col=2, lwd = 2)
rmse_nflx_regspl = rmse(NFLX_weekly$Close, regspl_nflx)
rmse_nflx_regspl

#####Smoothing splines----
plot(NFLX_weekly$Time, NFLX_weekly$Close, xlab="Time", ylab="Netflix close", type="l", lwd = 2)
smospl_nflx <- smooth.spline(NFLX_weekly$Time, NFLX_weekly$Close, lambda=0.00001)
lines(smospl_nflx, col=2, lwd=2)
rmse_nflx_smospl = rmse(NFLX_weekly$Close, smospl_nflx$y)
rmse_nflx_smospl

####GAM----
tt<- (1:length(NFLX))

g0_n <- gam(NFLX~(tt)+(AMZN))
summary(g0_n)
plot(g0_n, se=T)
aic_nflx_gam = AIC(g0_n)
aic_nflx_gam
rmse_nflx_gam = rmse(NFLX, g0_n$fitted.values)
rmse_nflx_gam

g1_n <- gam(NFLX~s(tt)+s(AMZN))
summary(g1_n)
plot(g1_n, se=T)
aic_nflx_gam_n = AIC(g1_n)
aic_nflx_gam_n
rmse_nflx_gam_n = rmse(NFLX, g1_n$fitted.values)
rmse_nflx_gam_n

g2_n <- gam(NFLX~s(tt)+s(AMZN)+s(ZM))
summary(g2_n)
plot(g2_n, se=T)
aic_nflx_gam_nz = AIC(g2_n)
aic_nflx_gam_nz
rmse_nflx_gam_nz = rmse(NFLX, g2_n$fitted.values)
rmse_nflx_gam_nz

g4_n <- gam(NFLX~lo(tt)+lo(AMZN))
summary(g4_n)
plot(g4_n, se=T)
aic_nflx_gam_lo_n = AIC(g4_n)
aic_nflx_gam_lo_n
rmse_nflx_gam_lo_n = rmse(NFLX, g4_n$fitted.values)
rmse_nflx_gam_lo_n

g5_n <- gam(NFLX~lo(tt)+lo(AMZN)+lo(ZM))
summary(g5_n)
plot(g5_n, se=T)
aic_nflx_gam_lo_n_z = AIC(g5_n)
aic_nflx_gam_lo_n_z
rmse_nflx_gam_lo_n_z = rmse(NFLX, g5_n$fitted.values)
rmse_nflx_gam_lo_n_z

# Zoom - Autocorrelation and TSLM------------------------------------------------------------------------------

acf(ZM_c, lag.max=120)

plot(decompose(ZM))
ggseasonplot(ZM)
ggseasonplot(ZM, polar=TRUE)
#there is a trend but not a seasonality(?)

#TSLM
tslm_zm<- tslm(ZM~trend)
summary(tslm_zm)
#not a good model in term of R squared
plot(ZM)
lines(fitted(tslm_zm), col=2)
plot(residuals(tslm_zm))
#positive autocorrelation
rmse_zm_tslm = rmse(ZM, fitted(tslm_zm))
rmse_zm_tslm

#TSLM with seas
tslm_s_zm<- tslm(ZM~trend+season) 
summary(tslm_s_zm)
#not a good model in term of R squared
plot(ZM)
lines(fitted(tslm_s_zm), col=2)
plot(residuals(tslm_s_zm))
#positive autocorrelation
rmse_nflx_tslm_s = rmse(ZM, fitted(tslm_s_zm))
rmse_nflx_tslm_s

#TSLM with NFLX
tslm_n_zm<- tslm(ZM~trend+NFLX) 
summary(tslm_n_zm)
plot(ZM)
lines(fitted(tslm_n_zm), col=2)
plot(residuals(tslm_n_zm))
rmse_zm_tslm_n = rmse(ZM, tslm_n_zm$fitted.values)
rmse_zm_tslm_n

#TSLM with AMZN
tslm_a_zm<- tslm(ZM~trend+AMZN) 
summary(tslm_a_zm)
plot(ZM)
lines(fitted(tslm_a_zm), col=2)
plot(residuals(tslm_a_zm))
rmse_zm_tslm_a = rmse(ZM[1:189], tslm_a_zm$fitted.values[1:189])
rmse_zm_tslm_a

#TSLM with NFLX and ZM
tslm_n_a_zm<- tslm(ZM~trend+AMZN+NFLX) 
summary(tslm_n_a_zm)
plot(ZM)
lines(fitted(tslm_n_a_zm), col=2)
plot(residuals(tslm_n_a_zm))
rmse_zm_tslm_n_z = rmse(ZM, tslm_n_a_zm$fitted.values)
rmse_zm_tslm_n_z

#TSLM with google_trends
tslm_gtzm_zm<- tslm(ZM~trend+zoom) 
summary(tslm_gtzm_zm)
plot(ZM)
lines(fitted(tslm_gtzm_zm), col=2)
plot(residuals(tslm_gtzm_zm))
rmse_zm_tslm_gtzm = rmse(ZM, fitted(tslm_gtzm_zm))
rmse_zm_tslm_gtzm


# Zoom - Models --------------------------------------------------------------------------------------------

####ARIMA MODELS----
Acf(ZM)
Pacf(ZM)
#there is an exponential decline in Acf and there is a significant spike at lag 1 in PACF and nothing else
#so we consider an Arima(p,d,0)
arima1_zm<- Arima(ZM, order=c(1,1,0))
resid1_zm<- residuals(arima1_zm)
tsdisplay(resid1_zm)
plot(ZM, ylab="ZM_Close", type="l")
lines(fitted(arima1_zm), col=2)
for1_zm<- forecast(arima1_zm, h = 10)
plot(for1_zm)
for1_zm
aic_zm_arima = AIC(arima1_zm)
aic_zm_arima
rmse_zm_arima = rmse(ZM, fitted(arima1_zm))
rmse_zm_arima

#AUTO_ARIMA
auto_arima_zm<- auto.arima(ZM, D=1)
auto_arima_zm
for1_zmwd<- forecast(auto_arima_zm, h = 10)
plot(for1_zmwd)
plot(ZM, ylab="ZM_Close", type="l")
lines(fitted(auto_arima_zm), col=2)
ress = residuals(auto_arima_zm)
Acf(ress)
aic_zm_autoarima = AIC(auto_arima_zm)
aic_zm_autoarima
rmse_zm_autoarima = rmse(ZM, fitted(auto_arima_zm))
rmse_zm_autoarima
#AIC of auto_arima_zm is obv lower than the "handmande" 

#SARIMA
sarima1_zm<- Arima(ZM, order=c(0,1,0), seasonal=c(1,1,0))
resid1_zm<- residuals(sarima1_zm)
plot(ZM, ylab="ZM_Close", type="l")
lines(fitted(sarima1_zm), col=2)
for1_s_zm<- forecast(sarima1_zm, h = 10)
plot(for1_s_zm)
aic_zm_sarima = AIC(sarima1_zm)
aic_zm_sarima
rmse_zm_sarima = rmse(ZM, fitted(sarima1_zm))
rmse_zm_sarima

####NON PARAMETRIC MODELS----

#####Local Regression----
plot(ZM_weekly$Time, ZM_weekly$Close, type="l", xlab="Time", ylab="Zoom close", lwd=2)
loc_r_zm = sm.regression(ZM_weekly$Time, ZM_weekly$Close, h = 10, add = T, col=2,  display="se", lwd=2, ngrid=190)
rmse_zm_locr = rmse(ZM_weekly$Close, loc_r_zm$estimate)
rmse_zm_locr

#####Loess----
plot(ZM_weekly$Time, ZM_weekly$Close, xlab="Time", ylab="Zoom close", type="l", lwd = 2)
lo1 <- loess.smooth(ZM_weekly$Time, ZM_weekly$Close, span=0.2)
lines(lo1, col=2, lwd = 2)
#vedi meglio

#####Regression splines----
plot(ZM_weekly$Time, ZM_weekly$Close, xlab="Time", ylab="Netflix close", type="l", lwd = 2)
m1<-lm(ZM_weekly$Close~bs(ZM_weekly$Time, df=20, degree=3)) 
xxx<-seq(min(ZM_weekly$Time),max(ZM_weekly$Time),length=260)
regspl_zm <-predict(m1, data.frame(x=xxx))
lines(xxx,regspl_zm,col=2, lwd = 2)
rmse_zm_regspl = rmse(ZM_weekly$Close, regspl_zm)
rmse_zm_regspl

#####Smoothing splines----
plot(ZM_weekly$Time, ZM_weekly$Close, xlab="Time", ylab="Netflix close", type="l", lwd = 2)
smospl_zm <- smooth.spline(ZM_weekly$Time, ZM_weekly$Close, lambda=0.00001)
lines(smospl_zm, col=2, lwd=2)
rmse_zm_smospl = rmse(ZM_weekly$Close, smospl_zm$y)
rmse_zm_smospl

####GAM----
tt<- (1:length(ZM))

g0_z <- gam(ZM~(tt)+(NFLX)+(AMZN)+(zoom))
summary(g0_z)
plot(g0_z, se=T)
aic_zm_gam = AIC(g0_z)
aic_zm_gam
rmse_zm_gam = rmse(ZM, g0_z$fitted.values)
rmse_zm_gam

g1_n <- gam(ZM~s(tt)+s(NFLX))
summary(g1_n)
plot(g1_n, se=T)
aic_zm_gam_n = AIC(g1_n)
aic_zm_gam_n
rmse_zm_gam_n = rmse(ZM, g1_n$fitted.values)
rmse_zm_gam_n

g6_n <- gam(ZM~s(tt)+s(AMZN))
summary(g6_n)
plot(g6_n, se=T)
aic_zm_gam_a = AIC(g6_n)
aic_zm_gam_a
rmse_zm_gam_a = rmse(ZM, g6_n$fitted.values)
rmse_zm_gam_a

g2_n <- gam(ZM~s(tt)+s(NFLX)+s(AMZN))
summary(g2_n)
plot(g2_n, se=T)
aic_zm_gam_na = AIC(g2_n)
aic_zm_gam_na
rmse_zm_gam_na = rmse(ZM, g2_n$fitted.values)
rmse_zm_gam_na

g3_n <- gam(ZM~s(tt)+s(NFLX)+s(AMZN)+s(zoom))
summary(g3_n)
plot(g3_n, se=T)
aic_zm_gam_nza = AIC(g3_n)
aic_zm_gam_nza
rmse_zm_gam_nza = rmse(ZM, g3_n$fitted.values)
rmse_zm_gam_nza

g4_n <- gam(ZM~lo(tt)+lo(NFLX))
summary(g4_n)
plot(g4_n, se=T)
aic_zm_gam_lo_n = AIC(g4_n)
aic_zm_gam_lo_n
rmse_zm_gam_lo_n = rmse(ZM, g4_n$fitted.values)
rmse_zm_gam_lo_n
