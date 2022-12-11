library(forecast)
library(lubridate)
library(lmtest) 
library(DIMORA)
library(readr)

Google_Trends <- read_csv("Desktop/Business Data/Google Trends Data.csv", na = "0", skip = 1)
colnames(Google_Trends) <- c('Time','Amazon','Netflix', 'Zoom')

plot(ts(Google_Trends[2:4], freq=365.25/7, start=decimal_date(ymd("2017-12-10"))), main = "Google Trends")

amazon = ts(Google_Trends$Amazon, freq=52, start=decimal_date(ymd("2017-12-10")))
netflix = ts(Google_Trends$Netflix, freq=52, start=decimal_date(ymd("2017-12-10")))
z = as.numeric(Google_Trends$Zoom)
z[is.na(z)] <- 0
zoom = ts(z, freq=52, start=decimal_date(ymd("2017-12-10")))

###AMAZON

autoplot(amazon)
acf(amazon, lag.max = 260) #There is seasonality
pacf(amazon, lag.max = 104)

#Qui ho tolto il 2017 perchè avendo solo 4 osservazioni dava prroblemi di intepretabilità nel plot
seas_amz= ts(Google_Trends$Amazon[4:260], freq=52, start=decimal_date(ymd("2018-1-6")))
ggseasonplot(seas_amz, main = "Seasonal Plot: Amazon")
ggseasonplot(seas_amz, polar=TRUE) +
  ylab("Amazon Trends") +
  ggtitle("Amazon Trend: a weekly overview")

#Per maggiore interpretqbilità, potebbe essere un'idea fare questi plot su base mensile

###BM###
amzn = Google_Trends$Amazon
amzn_bm<-BM(amzn,display = T)
summary(amzn_bm)

pred_bmamzn<- predict(amzn_bm, newx=c(1:260))
pred.bminstamzn<- make.instantaneous(pred_bmamzn)

plot(amzn, type= "l", xlim=c(1,260))
lines(pred.bminstamzn, lwd=2, col=2)


###ARIMA model
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

#As we expected, there is one differencing component and one seasonal differencing component.

fit1 <- Arima(amazon, order=c(1,1,2), seasonal=c(1,1,0))
checkresiduals(fit1)
plot(amazon)
lines(fitted(fit1), col = 2)

#FORECASTING
amazon %>%
  Arima(order=c(1,1,2), seasonal=c(1,1,0), lambda=0) %>% forecast() %>%
  autoplot() +
  ylab("Amazon Trends") + xlab("Year")


###Holt Winters

HW1 <- HoltWinters(amazon, seasonal = "multiplicative")
#HW2 <- HoltWinters(amazon, seasonal = "additive")
#HW2 <- HoltWinters(amazon, alpha=0.2, beta=0.1, gamma=0.1)
plot(amazon, ylab="Amazon Trend")
lines(HW1$fitted[,1], col="red")
#lines(HW2$fitted[,1], col="red")
lines(fitted(fit1), col="blue") #Confronto con modello ARIMA

cat("RMSE of best ARIMA model is", rmse(amazon, fitted(fit1)))
cat("RMSE of HW model is", rmse(amazon, HW1$fitted[,1]))
