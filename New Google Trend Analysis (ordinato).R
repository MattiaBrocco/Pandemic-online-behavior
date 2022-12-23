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
AMZN_weekly = read.csv("./yahoo_data/AMZN_weekly.csv", sep=",")
NFLX_weekly = read.csv("./yahoo_data/NFLX_weekly.csv", sep=",")
ZM_weekly_c = read.csv("./yahoo_data/ZM_weekly.csv", sep=",")

#AMAZON
#AMZN_weekly = read.csv("./yahoo_data/AMZN_weekly.csv", sep=";")
AMZN_weekly = AMZN_weekly[2:261, c(1, 6)]
colnames(AMZN_weekly) = c('Time', 'Close')
AMZN_weekly$Time = as.Date(AMZN_weekly$Time, "%d/%m/%Y")
AMZN = ts(AMZN_weekly$Close, freq=52, start=decimal_date(ymd("2017-12-11")))

#NETFLIX
#NFLX_weekly = read.csv("./yahoo_data/NFLX_weekly.csv", sep=";")
NFLX_weekly = NFLX_weekly[2:261, c(1, 6)]
colnames(NFLX_weekly) = c('Time', 'Close')
NFLX_weekly$Time = as.Date(NFLX_weekly$Time, format = "%d/%m/%Y")
NFLX = ts(NFLX_weekly$Close, freq=52, start=decimal_date(ymd("2017-12-11")))

#ZOOM
#ZM_weekly_c = read.csv("./yahoo_data/ZM_weekly.csv", sep=";")
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

#Google_Trends <- read_csv("Desktop/Business Data/Google Trends Data.csv", na = "0", skip = 1)
Google_Trends <- read_csv("./gtrend_data/Google Trends Data.csv", na = "0", skip = 1)
#Google_Trends <- read.csv("./gtrend_data/Google Trends Data.csv", na = "0", skip = 1)
colnames(Google_Trends) <- c('Time','Amazon','Netflix', 'Zoom')
plot(ts(Google_Trends[2:4], freq=52, start=decimal_date(ymd("2017-12-11"))), main = "Google Trends")
amazon = ts(Google_Trends$Amazon, freq=52, start=decimal_date(ymd("2017-12-11")))
netflix = ts(Google_Trends$Netflix, freq=52, start=decimal_date(ymd("2017-12-11")))
z = as.numeric(Google_Trends$Zoom)
z[is.na(z)] <- 0
zoom = ts(z, freq=52, start=decimal_date(ymd("2017-12-11")))

# First plots --------------------------------------------------------------------
seas_amz= ts(Google_Trends$Amazon[4:260], freq=52,
             start=decimal_date(ymd("2018-1-6")))
ggseasonplot(seas_amz, main = "Seasonal Plot: Amazon")
ggseasonplot(seas_amz, polar=TRUE) +
  ylab("Amazon Trends") +
  ggtitle("Amazon Trend: a weekly overview")

autoplot(amazon)

# Amazon - Autocorrelation and TSLM------------------------------------------------------------------------------

acf(amazon, lag.max = 260) #There is seasonality
pacf(amazon, lag.max = 104)

plot(decompose(amazon))


#TSLM
tslm_amzn<- tslm(amazon~trend)
summary(tslm_amzn)
#not a good model in term of R squared
plot(amazon)
lines(fitted(tslm_amzn), col=2)
plot(residuals(tslm_amzn), type = "o")
#positive autocorrelation
rmse_amzn_tslm = rmse(amazon, fitted(tslm_amzn))
rmse_amzn_tslm

#TSLM with seas
tslm_s_amzn<- tslm(amazon~trend+season) 
summary(tslm_s_amzn)
#not a good model in term of R squared
plot(amazon)
lines(fitted(tslm_s_amzn), col=2)
plot(residuals(tslm_s_amzn), type = "o")
#positive autocorrelation
rmse_amzn_tslm_s = rmse(AMZN, fitted(tslm_s_amzn))
rmse_amzn_tslm_s

#TSLM with NFLX
tslm_n_amzn<- tslm(amazon~trend+netflix) 
summary(tslm_n_amzn)
plot(amazon)
lines(fitted(tslm_n_amzn), col=2)
plot(residuals(tslm_n_amzn), type = "o")
rmse_amzn_tslm_n = rmse(AMZN, fitted(tslm_n_amzn))
rmse_amzn_tslm_n

#TSLM with ZM
tslm_z_amzn<- tslm(amazon~trend+zoom) 
summary(tslm_z_amzn)
plot(amazon)
lines(fitted(tslm_z_amzn), col=2)
plot(residuals(tslm_z_amzn))
rmse_amzn_tslm_z = rmse(amazon, fitted(tslm_z_amzn))
rmse_amzn_tslm_z

#TSLM with NFLX and ZM
tslm_n_z_amzn<- tslm(amazon~trend+zoom+netflix) 
summary(tslm_n_z_amzn)
plot(amazon)
lines(fitted(tslm_n_z_amzn), col=2)
plot(residuals(tslm_n_z_amzn))
rmse_amzn_tslm_n_z = rmse(amazon, fitted(tslm_n_z_amzn))
rmse_amzn_tslm_n_z

#TSLM with stocks
tslm_gtamzn_amzn<- tslm(amazon~trend+AMZN) 
summary(tslm_gtamzn_amzn)
plot(amazon)
lines(fitted(tslm_gtamzn_amzn), col=2)
plot(residuals(tslm_gtamzn_amzn))
rmse_amzn_tslm_gtamzn = rmse(amazon, fitted(tslm_gtamzn_amzn))
rmse_amzn_tslm_gtamzn

# Amazon - Models --------------------------------------------------------------------------------------------

####ARIMA MODELS----
Acf(amazon)
Pacf(amazon)

#We know our data as seasonal. So I need to differencing.

amazon %>% diff(lag=52) %>% ggtsdisplay() #dati ancora stagionali, bisogna differenziare ancora
amazon %>% diff(lag=52) %>% diff() %>% ggtsdisplay()#Now the ts is without seasonality.

diff_amz <- amazon %>% diff(lag=52) %>% diff() 

diff_amz %>%
  Arima(order=c(1,0,2), seasonal=c(1,0,0)) %>% residuals() %>% ggtsdisplay()

fit3 <- Arima(diff_amz, order=c(1,0,2), seasonal=c(1,0,0)) 
checkresiduals(fit3)

plot(diff_amz)
lines(fitted(fit3), col = 2)
fit3 %>% forecast(h=52) %>% autoplot()

#Now I try to use the auto.arima function on the original amazon time serie:

amazon %>%
  auto.arima() %>% residuals() %>% ggtsdisplay()


####NON PARAMETRIC MODELS----

#####Local Regression----
library(sm)
plot(Google_Trends$Time, amazon, type="l",xlab="Time", ylab="Amazon Trend", lwd=2)
loc_r_amzn = sm.regression(Google_Trends$Time, amazon,   h = 10, add = T, col=2,  display="se")
rmse_amzn_locr = rmse(amazon, loc_r_amzn$estimate)
rmse_amzn_locr

#####Loess----
plot(Google_Trends$Time, amazon, xlab="Time", ylab="Amazon Trend", type="l",lwd = 2)
lo1 <- loess.smooth(Google_Trends$Time, amazon, span=0.2)
lines(lo1, col=2)
rmse_amzn_loess = rmse(amazon, lo1$y)
rmse_amzn_loess

#####Regression splines----
library(splines)
plot(Google_Trends$Time, amazon, xlab="Time", ylab="Amazon close", type = "l",lwd = 2)
m1<-lm(amazon~bs(Google_Trends$Time, df=15, degree=3)) 
xxx<-seq(min(Google_Trends$Time),max(Google_Trends$Time),length=260)
regspl_amzn<-predict(m1, data.frame(x=xxx))
lines(xxx,regspl_amzn,col=2, lwd = 2)
rmse_amzn_regspl = rmse(amazon, regspl_amzn)
rmse_amzn_regspl

#####Smoothing splines----

#0.0001 o 0.00001?
plot(Google_Trends$Time, amazon, xlab="Time", ylab="Amazon close", type = "l",lwd = 2)
s <- smooth.spline(Google_Trends$Time, amazon, lambda=0.00001)
lines(s, col=2)
rmse_amzn_smospl = rmse(amazon, s$y)
rmse_amzn_smospl

####GAM----
tt<- (1:length(amazon))
seas <- factor(c(rep(1:52,length(amazon)/52))) 
length(seas)
seas

mod2 <- lm(amazon~ tt+seas+netflix)
summary(mod2)
AIC(mod2)

#g3 = gam(amazon~seas+netflix)
#g3 = gam(amazon~nflx.ts[2:261])
#g3 = gam(amazon~lo(netflix))
g3 = gam(amazon~seas+lo(netflix)+lo(tt)+lo(zoom)+lo(AMZN))
summary(g3)
#par(mfrow=c(3,5))
#plot(g3, se=T) 
#plot(g3)
#plot(g3, type = "l")
AIC(g3)

g3m = ts(g3$fitted.values, freq=52, start=decimal_date(ymd("2017-12-10")))
plot(amazon, lwd = 2)
lines(g3m, col = 2, lwd = 2, se = T)
plot(g3, se = T)

#RESIDUALS
plot(g3$residuals, type = "o", col = "blue")

#EVALUATION
AIC(g3)
rmse_amzn_gam = rmse(amazon, g3$fitted.values)
rmse_amzn_gam


# Netflix - Autocorrelation and TSLM------------------------------------------------------------------------------

acf(netflix, lag.max=220)
pacf(netflix, lag.max=220)

plot(decompose(netflix))
ggseasonplot(netflix)
ggseasonplot(netflix, polar=TRUE)

#TSLM
tslm_nflx<- tslm(netflix~trend)
summary(tslm_nflx) #c'è trend
#not a good model (ovviamente)
plot(netflix)
lines(fitted(tslm_nflx), col=2)
plot(residuals(tslm_nflx))
#positive autocorrelation
rmse_nflx_tslm = rmse(netflix, fitted(tslm_nflx))
rmse_nflx_tslm

#TSLM with seas
tslm_s_nflx<- tslm(netflix~trend+season) #c'è sia trend che stagionalità
summary(tslm_s_nflx)
#not a good model in term of R squared
plot(netflix)
lines(fitted(tslm_s_nflx), col=2)
plot(residuals(tslm_s_nflx))
#positive autocorrelation
rmse_nflx_tslm_s = rmse(AMZN, fitted(tslm_s_nflx))
rmse_nflx_tslm_s

#TSLM with AMZN
tslm_a_nflx<- tslm(netflix~trend+season+amazon) 
summary(tslm_a_nflx)
plot(netflix)
lines(fitted(tslm_a_nflx), col=2)
plot(residuals(tslm_a_nflx))
rmse_nflx_tslm_a = rmse(netflix, fitted(tslm_a_nflx))
rmse_nflx_tslm_a

#TSLM with ZM
tslm_z_nflx<- tslm(netflix~trend+season+zoom) 
summary(tslm_z_nflx)
plot(netflix)
lines(fitted(tslm_z_nflx), col=2)
plot(residuals(tslm_z_nflx))
rmse_nflx_tslm_z = rmse(netflix, fitted(tslm_z_nflx))
rmse_nflx_tslm_z

#TSLM with AMZN and ZM
tslm_a_z_nflx<- tslm(netflix~trend+season+zoom+amazon) 
summary(tslm_a_z_nflx)
plot(netflix, lwd = 2)
lines(fitted(tslm_a_z_nflx), col=2)
plot(residuals(tslm_a_z_nflx), type = "o")
rmse_nflx_tslm_a_z = rmse(netflix, fitted(tslm_a_z_nflx))
rmse_nflx_tslm_a_z

#TSLM with stock
tslm_gtnflx_nflx<- tslm(netflix~trend+season+NFLX) 
summary(tslm_gtnflx_nflx)
plot(netflix, lwd = 2)
lines(fitted(tslm_gtnflx_nflx), col=2)
plot(residuals(tslm_gtnflx_nflx), type = "o", cex = 0.3)
rmse_nflx_tslm_gtnflx = rmse(netflix, fitted(tslm_gtnflx_nflx))
rmse_nflx_tslm_gtnflx

#TSLM with google_trends+stock (con tutto per vedere cosa è significativo)
tslm_gtnflx_nflx<- tslm(netflix~trend+season+NFLX+zoom+amazon+ZM+AMZN) 
summary(tslm_gtnflx_nflx)
plot(netflix, lwd = 2)
lines(fitted(tslm_gtnflx_nflx), col=2)
plot(residuals(tslm_gtnflx_nflx), type = "o", cex = 0.3)
rmse_nflx_tslm_gtnflx = rmse(netflix, fitted(tslm_gtnflx_nflx))
rmse_nflx_tslm_gtnflx


# Netflix - Models --------------------------------------------------------------------------------------------
####ARIMA MODELS----
Acf(netflix)
Pacf(netflix)

#Provo a vedere quanto differenziare
netflix %>% diff(lag = 52) %>% diff(lag=52) %>% diff() %>% diff() %>% ggtsdisplay()
netflix %>% diff() %>% ggtsdisplay()
#una sola differenziazione può bastare
diff_nflx <- netflix %>% diff() %>% diff(lag = 52)

library(tseries) #per provare il Dickey-Fuller test
adf.test(diff_nflx)#serie stazionaria se p-value < 0.05

#Vedo la serie differenziata
plot(diff_nflx)
acf(diff_nflx, lag.max = 260)
pacf(diff_nflx, lag.max = 260)

#fit3 <- Arima(diff_nflx, order=c(1,0,2), seasonal=c(1,0,2)) 
#AIC(fit3)
#plot(diff_nflx)
#lines(fitted(fit3), col = 2)

#Provo con la serie originale netflix
fit3 <- Arima(netflix, order=c(1,1,2), seasonal=c(1,1,2)) 
AIC(fit3) #best AIC

plot(netflix)
lines(fitted(fit3), col = 2)
fit3 %>% forecast(h=10) %>% autoplot()

checkresiduals(fit3)

#Confonto con auto.arima
#Perchè auto.arima dà questo risultato?
auto.arima(netflix, seasonal = F)

aic_nflx_sarima = AIC(fit3)
aic_nflx_sarima
rmse_nflx_sarima = rmse(netflix, fitted(fit3))
rmse_nflx_sarima

####NON PARAMETRIC MODELS----
#####Local Regression----
plot(Google_Trends$Time, zoom, type="l",xlab="Time", ylab="Amazon Trend", lwd=2)
loc_r_zm = sm.regression(Google_Trends$Time, zoom,   h = 10, add = T, col=2,  display="se")
rmse_zm_locr = rmse(zoom, loc_r_zm$estimate)
rmse_zm_locr

#####Loess----
plot(Google_Trends$Time, netflix, xlab="Time", ylab="Amazon Trend", type="l",lwd = 2)
lo1 <- loess.smooth(Google_Trends$Time, netflix, span=0.2)
lines(lo1, col=2)
rmse_amzn_loess = rmse(netflix, lo1$y)
rmse_amzn_loess

#####Regression splines----
plot(Google_Trends$Time, netflix, xlab="Time", ylab="Amazon close", type = "l",lwd = 2)
m1<-lm(netflix~bs(Google_Trends$Time, df=15, degree=3)) 
xxx<-seq(min(Google_Trends$Time),max(Google_Trends$Time),length=260)
regspl_nflx <-predict(m1, data.frame(x=xxx))
lines(xxx,regspl_nflx,col=2, lwd = 2)
rmse_nflx_regspl = rmse(netflix, regspl_nflx)
rmse_nflx_regspl

#####Smoothing splines----
plot(Google_Trends$Time, netflix, xlab="Time", ylab="Amazon close", type = "l",lwd = 2)
s <- smooth.spline(Google_Trends$Time, netflix, lambda=0.00001)
lines(s, col=2)
rmse_amzn_smospl = rmse(netflix, s$y)
rmse_amzn_smospl

####GAM----
tt<- (1:length(netflix))
seas <- factor(c(rep(1:52,length(netflix)/52))) 

mod2 <- lm(netflix~ tt+seas+amazon)
summary(mod2)
AIC(mod2)

#g3 = gam(amazon~seas+netflix)
#g3 = gam(amazon~nflx.ts[2:261])
#g3 = gam(amazon~lo(netflix))
g3 = gam(netflix~seas+lo(amazon)+lo(tt)+lo(zoom)+lo(NFLX))
summary(g3)
#par(mfrow=c(3,5))
#plot(g3, se=T) 
#plot(g3)
#plot(g3, type = "l")
#AIC(g3)

g3m = ts(g3$fitted.values, freq=52, start=decimal_date(ymd("2017-12-10")))
plot(netflix, lwd = 2)
lines(g3m, col = 2, lwd = 2, se = T)
plot(g3, se = T)

#RESIDUALS
plot(g3$residuals, type = "o", col = "blue")

#EVALUATION
AIC(g3)
rmse_nflx_gam = rmse(netflix, g3$fitted.values)
rmse_nflx_gam


# Zoom - Autocorrelation and TSLM------------------------------------------------------------------------------

acf(zoom, lag.max=120)

plot(decompose(zoom))
ggseasonplot(zoom)
ggseasonplot(zoom, polar=TRUE)


#TSLM
tslm_zm<- tslm(zoom~trend)
summary(tslm_zm)
#c'è trend
plot(zoom)
lines(fitted(tslm_zm), col=2)
plot(residuals(tslm_zm))
#positive autocorrelation
rmse_zm_tslm = rmse(zoom, fitted(tslm_zm))
rmse_zm_tslm

#TSLM with seas
tslm_s_zm<- tslm(zoom~trend+season) 
summary(tslm_s_zm)
#non c'è stagionalità
plot(zoom)
lines(fitted(tslm_s_zm), col=2)
plot(residuals(tslm_s_zm))

rmse_nflx_tslm_s = rmse(zoom, fitted(tslm_s_zm))
rmse_nflx_tslm_s

#TSLM with NFLX
tslm_n_zm<- tslm(zoom~trend+netflix) 
summary(tslm_n_zm)
plot(zoom)
lines(fitted(tslm_n_zm), col=2)
plot(residuals(tslm_n_zm), type = "o", cex = 0.3)
rmse_zm_tslm_n = rmse(zoom, tslm_n_zm$fitted.values)
rmse_zm_tslm_n

#TSLM with amazon
tslm_a_zm<- tslm(zoom~trend+amazon) 
summary(tslm_a_zm)
plot(zoom)
lines(fitted(tslm_a_zm), col=2)
plot(residuals(tslm_a_zm))
rmse_zm_tslm_a = rmse(zoom, tslm_a_zm$fitted.values)
rmse_zm_tslm_a

#TSLM with NFLX and ZM
tslm_n_a_zm<- tslm(zoom~trend+season+amazon+netflix) 
summary(tslm_n_a_zm)
plot(zoom)
lines(fitted(tslm_n_a_zm), col=2)
plot(residuals(tslm_n_a_zm), type = "o")
rmse_zm_tslm_n_z = rmse(zoom, tslm_n_a_zm$fitted.values)
rmse_zm_tslm_n_z

#TSLM with google_trends
tslm_gtzm_zm<- tslm(zoom~trend+ZM) 
summary(tslm_gtzm_zm)
plot(zoom)
lines(fitted(tslm_gtzm_zm), col=2)
plot(residuals(tslm_gtzm_zm))
rmse_zm_tslm_gtzm = rmse(zoom, fitted(tslm_gtzm_zm))
rmse_zm_tslm_gtzm


# Zoom - Models --------------------------------------------------------------------------------------------

####ARIMA MODELS----

Acf(zoom)
Pacf(zoom)

zoom %>% diff() %>% ggtsdisplay() #dati ancora stagionali, bisogna differenziare ancora
zoom %>% diff(lag=52) %>% diff() %>% ggtsdisplay()#Now the ts is without seasonality.
zoom %>% diff(lag=52) %>% diff() %>% diff()%>% ggtsdisplay()

diff_zm <- zoom %>% diff()%>% diff(lag=52)
plot(diff_zm)

adf.test(diff_zm)

acf(diff_zm, lag.max = 260)
pacf(diff_zm, lag.max = 260)

#diff_amz %>%
 # Arima(order=c(1,0,2), seasonal=c(1,0,0)) %>% residuals() %>% ggtsdisplay()

fit3 <- Arima(diff_zm, order=c(0,0,0), seasonal=c(0,0,0)) 

plot(diff_zm)
lines(fitted(fit3), col = 2)
fit3 %>% forecast(h=10) %>% autoplot()

checkresiduals(fit3)



#fit3 <- Arima(zoom, order=c(2,1,1), seasonal=c(0,1,0)) 
#fit3 <- Arima(zoom, order=c(2,1,1))

fit3 <- Arima(zoom, order=c(2,1,1), seasonal=c(1,1,2)) 
AIC(fit3)

plot(zoom)
lines(fitted(fit3), col = 2)
fit3 %>% forecast(h=10) %>% autoplot()

checkresiduals(fit3)

#Confronto con auto.arima
auto.arima(zoom)

aic_zm_sarima = AIC(fit3)
aic_zm_sarima
rmse_nflx_sarima = rmse(zoom, fitted(fit3))
rmse_nflx_sarima

####NON PARAMETRIC MODELS----

#####Local Regression----
plot(ZM_weekly$Time, ZM_weekly$Close, type="l", xlab="Time", ylab="Zoom close", lwd=2)
loc_r_zm = sm.regression(ZM_weekly$Time, ZM_weekly$Close, h = 10, add = T, col=2,  display="se", lwd=2, ngrid=190)
rmse_zm_locr = rmse(ZM_weekly$Close, loc_r_zm$estimate)
rmse_zm_locr

#####Loess----
plot(Google_Trends$Time, zoom, xlab="Time", ylab="Amazon Trend", type="l",lwd = 2)
lo1 <- loess.smooth(Google_Trends$Time, zoom, span=0.2)
lines(lo1, col=2)
rmse_zm_loess = rmse(zoom, lo1$y)
rmse_zm_loess

#####Regression splines----
m1<-lm(zoom~bs(Google_Trends$Time, df=15, degree=3)) 
xxx<-seq(min(Google_Trends$Time),max(Google_Trends$Time),length=260)
regspl_zm<-predict(m1, data.frame(x=xxx))
plot(Google_Trends$Time, zoom, xlab="Time", ylab="Amazon close", type = "l",lwd = 2)
lines(xxx,regspl_zm,col=2, lwd = 2)
rmse_zm_regspl = rmse(zoom, regspl_zm)
rmse_zm_regspl

#####Smoothing splines----
plot(Google_Trends$Time, zoom, xlab="Time", ylab="Amazon close", type = "l",lwd = 2)
s <- smooth.spline(Google_Trends$Time, zoom, lambda=0.00001)
lines(s, col=2)
rmse_zm_smospl = rmse(zoom, s$y)
rmse_zm_smospl

####GAM----
tt<- (1:length(zoom))
seas <- factor(c(rep(1:52,length(zoom)/52))) 

mod2 <- lm(zoom~tt+amazon)
summary(mod2)
AIC(mod2)

g3 = gam(zoom~lo(amazon)+lo(tt)+lo(netflix)+lo(NFLX))
summary(g3)
#par(mfrow=c(3,5))
#plot(g3, se=T) 
#plot(g3)
#plot(g3, type = "l")
#AIC(g3)

g3m = ts(g3$fitted.values, freq=52, start=decimal_date(ymd("2017-12-10")))
plot(zoom, lwd = 2)
lines(g3m, col = 2, lwd = 2, se = T)
plot(g3, se = T)

#RESIDUALS
plot(g3$residuals, type = "o", col = "blue")

#EVALUATION
AIC(g3)
rmse_zm_gam = rmse(zoom, g3$fitted.values)
rmse_zm_gam
