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
library(MLmetrics)

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

compl_ds = data.frame(as.numeric(Google_Trends$Amazon), as.numeric(Google_Trends$Netflix), as.numeric(Google_Trends$Zoom), AMZN_weekly$Close, NFLX_weekly$Close, ZM_weekly$Close)

library(GGally)
ggpairs(compl_ds, title="correlogram with ggpairs()") 

#library("PerformanceAnalytics")
#chart.Correlation(compl_ds, histogram=TRUE, pch=19)

par(mfrow=c(2,3))
boxplot(AMZN, col="dark blue", main="Amazon Stock")
boxplot(NFLX, col="dark red", main="Netflix Stock")
boxplot(ZM, col="light blue", main="Zoom Stock")
boxplot(amazon, col="dark blue", main="Amazon Google Trends")
boxplot(netflix, col="dark red", main="Netflix Google Trends")
boxplot(zoom, col="light blue", main="Zoom Google Trends")

# Amazon - Autocorrelation and TSLM------------------------------------------------------------------------------
plot(decompose(AMZN))
ggseasonplot(AMZN)
ggseasonplot(AMZN, polar=TRUE)
#there is a trend but not an importatnt seasonality

#TSLM
tslm_amzn<- tslm(AMZN~trend+NFLX+ZM+amazon) 
summary(tslm_amzn)
durbinWatsonTest(tslm_amzn)
par(mfrow=c(1,1))
plot(AMZN)
lines(fitted(tslm_amzn), col=2)
#tsdisplay(residuals(tslm_amzn))
rmse_amzn_tslm = rmse(AMZN, fitted(tslm_amzn))
aic_amzn_tslm = AIC(tslm_amzn)
mape_amzn_tslm = MAPE(AMZN, fitted(tslm_amzn))

# Amazon - Models --------------------------------------------------------------------------------------------

####ARIMA MODELS----
Acf(AMZN)
Pacf(AMZN)
#AR process
#A gradual geometrically declining ACF and a PACF that is significant for only a few lags indicate an AR 
# process. In the figures, we can see that ACF is geometrically declining with lags. The PACF has 1 
# significant lag followed by a drop in PACF values and they become insignificant. With 1 significant 
# PACF lags and gradually falling ACF, we can say that the series is an AR(1) process. 
# The lags of AR are determined by the number of significant lags of PACF.

#there is an exponential decline in Acf and there is a significant spike at lag 1 in PACF and nothing else
#so we consider an Arima(p,d,0)

#AUTO_ARIMA with diff
auto_arima_s_amzn<- auto.arima(AMZN, D = 1)
auto_arima_s_amzn
plot(AMZN, ylab="AMZN_Close", type="l")
lines(fitted(auto_arima_s_amzn), col=2)
ress = residuals(auto_arima_s_amzn)
tsdisplay(ress)
aic_amzn_autoarima_s = AIC(auto_arima_s_amzn)
aic_amzn_autoarima_s
rmse_amzn_autoarima_s = rmse(AMZN, fitted(auto_arima_s_amzn))
rmse_amzn_autoarima_s
mape_amzn_autoarima_s = MAPE(AMZN, fitted(auto_arima_s_amzn))
mape_amzn_autoarima_s

#AUTO_ARIMA
auto_arima_amzn<- auto.arima(AMZN)
auto_arima_amzn
plot(AMZN, ylab="AMZN_Close", type="l")
lines(fitted(auto_arima_amzn), col=2)
ress = residuals(auto_arima_amzn)
tsdisplay(ress)
aic_amzn_autoarima = AIC(auto_arima_amzn)
aic_amzn_autoarima
rmse_amzn_autoarima = rmse(AMZN, fitted(auto_arima_amzn))
rmse_amzn_autoarima
mape_amzn_autoarima = MAPE(AMZN, fitted(auto_arima_amzn))
mape_amzn_autoarima

#SARIMA
sarima1_amzn<- Arima(AMZN, order=c(0,1,0), seasonal=c(1,1,0))
resid1_amzn<- residuals(sarima1_amzn)
tsdisplay(resid1_amzn)
plot(AMZN, ylab="AMZN_Close", type="l")
lines(fitted(sarima1_amzn), col=2)
for1_s_amzn<- forecast(sarima1_amzn, h = 20)
plot(for1_s_amzn)
aic_amzn_sarima = AIC(sarima1_amzn)
aic_amzn_sarima
rmse_amzn_sarima = rmse(AMZN, fitted(sarima1_amzn))
rmse_amzn_sarima
mape_amzn_sarima = MAPE(AMZN, fitted(sarima1_amzn))
mape_amzn_sarima


####GAM----
tt<- (1:length(AMZN))

g3_a <- gam(AMZN~s(tt)+s(NFLX)+s(ZM)+s(amazon))
summary(g3_a)
par(mfrow=c(2,2))
plot(g3_a, se=T)
aic_amzn_gam_nza = AIC(g3_a)
aic_amzn_gam_nza
rmse_amzn_gam_nza = rmse(AMZN, g3_a$fitted.values)
rmse_amzn_gam_nza
mape_amzn_gam_nza = MAPE(AMZN, g3_a$fitted.values)
mape_amzn_gam_nza

# Netflix - Autocorrelation and TSLM------------------------------------------------------------------------------
par(mfrow=c(1,1))
acf(NFLX, lag.max=120)
pacf(NFLX, lag.max=120)

plot(decompose(NFLX))
ggseasonplot(NFLX)
ggseasonplot(NFLX, polar=TRUE)

#TSLM with AMAZON
tslm_gtnflx_nflx<- tslm(NFLX~trend+AMZN) 
summary(tslm_gtnflx_nflx)
plot(NFLX)
lines(fitted(tslm_gtnflx_nflx), col=2)
plot(residuals(tslm_gtnflx_nflx))
rmse_nflx_tslm_gtnflx = rmse(NFLX, fitted(tslm_gtnflx_nflx))
rmse_nflx_tslm_gtnflx
aic_nflx_tslm_gtnflx <- AIC(tslm_gtnflx_nflx)
mape_nflx_tslm_gtnflx <- MAPE(NFLX, fitted(tslm_gtnflx_nflx))

# Netflix - Models --------------------------------------------------------------------------------------------
####ARIMA MODELS----
Acf(NFLX)
Pacf(NFLX)
#there is an exponential decline in Acf and there is a significant spike at lag 1 in PACF and nothing else
#so we consider an Arima(p,d,0)

#A gradual geometrically declining ACF and a PACF that is significant for only a few lags indicate an AR 
# process. The lags of AR are determined by the number of significant lags of PACF.

#ARIMA
#c(1,1,0) o c(4,1,0)?
arima1_nflx<- Arima(NFLX, order=c(4,1,0))
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
mape_nflx_arima = MAPE(NFLX, fitted(arima1_nflx))
mape_nflx_arima

#AUTO_ARIMA
auto_arima_nflx<- auto.arima(NFLX)
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
mape_nflx_autoarima = MAPE(NFLX, fitted(auto_arima_nflx))
mape_nflx_autoarima

#AUTO_ARIMA with diff
auto_arima_nflx_d<- auto.arima(NFLX, D=1)
auto_arima_nflx_d
plot(NFLX, ylab="NFLX_Close", type="l")
lines(fitted(auto_arima_nflx_d), col=2)
for2_nflx_d<- forecast(auto_arima_nflx_d, h = 10)
plot(for2_nflx)
ress = residuals(auto_arima_nflx_d)
Acf(ress)
aic_nflx_autoarima_d = AIC(auto_arima_nflx_d)
aic_nflx_autoarima_d
rmse_nflx_autoarima_d = rmse(NFLX, fitted(auto_arima_nflx_d))
rmse_nflx_autoarima_d
mape_nflx_autoarima_d = MAPE(NFLX, fitted(auto_arima_nflx_d))
mape_nflx_autoarima_d

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
mape_nflx_sarima = MAPE(NFLX, fitted(sarima1_nflx))
mape_nflx_sarima

####GAM----
tt<- (1:length(NFLX))

g5_n <- gam(NFLX~lo(tt)+lo(AMZN)+lo(ZM))
summary(g5_n)
par(mfrow=c(2,2))
plot(g5_n, se=T)
aic_nflx_gam_lo_n_z = AIC(g5_n)
aic_nflx_gam_lo_n_z
rmse_nflx_gam_lo_n_z = rmse(NFLX, g5_n$fitted.values)
rmse_nflx_gam_lo_n_z
mape_nflx_gam_lo_n_z = MAPE(NFLX, g5_n$fitted.values)
mape_nflx_gam_lo_n_z

# Zoom - Autocorrelation and TSLM------------------------------------------------------------------------------

acf(ZM_c, lag.max=120)

plot(decompose(ZM))
ggseasonplot(ZM)
ggseasonplot(ZM, polar=TRUE)

#TSLM with AMZN
tslm_a_zm<- tslm(ZM~trend+AMZN) 
summary(tslm_a_zm)
par(mfrow=c(1,1))
plot(ZM)
lines(fitted(tslm_a_zm), col=2)
plot(residuals(tslm_a_zm))
rmse_zm_tslm_a = rmse(ZM, fitted(tslm_a_zm))
rmse_zm_tslm_a
aic_zm_tslm_a = AIC(tslm_a_zm)
aic_zm_tslm_a
mape_zm_tslm_a = MAPE(ZM, fitted(tslm_a_zm))
mape_zm_tslm_a

# Zoom - Models --------------------------------------------------------------------------------------------
####ARIMA MODELS----

Acf(ZM)
Pacf(ZM)
#there is an exponential decline in Acf and there is a significant spike at lag 1 in PACF and nothing else
#so we consider an Arima(p,d,0)

#A gradual geometrically declining ACF and a PACF that is significant for only a few lags indicate an AR 
# process. In the figures, we can see that ACF is geometrically declining with lags. The PACF has 2 
# significant lags followed by a drop in PACF values and they become insignificant. With 2 significant 
# PACF lags and gradually falling ACF, we can say that the series is an AR(2) process. 
# The lags of AR are determined by the number of significant lags of PACF.

#AUTO_ARIMA
auto_arima_zm<- auto.arima(ZM)
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

#SARIMA
sarima1_zm<- Arima(ZM, order=c(0,1,0), seasonal=c(1,1,0))
resid1_zm<- residuals(sarima1_zm)
plot(ZM, ylab="ZM_Close", type="l")
lines(fitted(sarima1_zm), col=2)
for1_s_zm<- forecast(sarima1_zm, h = 20)
plot(for1_s_zm)
aic_zm_sarima = AIC(sarima1_zm)
aic_zm_sarima
rmse_zm_sarima = rmse(ZM, fitted(sarima1_zm))
rmse_zm_sarima

####GAM----
tt<- (1:length(ZM))

g3_n <- gam(ZM~s(tt)+s(NFLX)+s(AMZN)+s(zoom))
summary(g3_n)
par(mfrow=c(2,2))
plot(g3_n, se=T)
aic_zm_gam_nza = AIC(g3_n)
aic_zm_gam_nza
rmse_zm_gam_nza = rmse(ZM, g3_n$fitted.values)
rmse_zm_gam_nza
mape_zm_gam_nza = MAPE(ZM, g3_n$fitted.values)
mape_zm_gam_nza

