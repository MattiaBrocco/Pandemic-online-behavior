library(forecast)
library(lubridate)
library(lmtest) 
library(DIMORA)
library(readr)

Google_Trends <- read_csv("Desktop/Business Data/Google Trends Data.csv", na = "0", skip = 1)
colnames(Google_Trends) <- c('Time','Amazon','Netflix', 'Zoom')

goo = Google_Trends[2:4]
trends = ts(goo, freq=365.25/7, start=decimal_date(ymd("2017-12-10")))

plot(ts(Google_Trends[2:4], freq=365.25/7, start=decimal_date(ymd("2017-12-10"))), main = "Google Trends")

amazon = ts(Google_Trends$Amazon, freq=52, start=decimal_date(ymd("2017-12-10")))
netflix = ts(Google_Trends$Netflix, freq=52, start=decimal_date(ymd("2017-12-10")))
z = as.numeric(Google_Trends$Zoom)
z[is.na(z)] <- 0
zoom = ts(z, freq=52, start=decimal_date(ymd("2017-12-10")))

autoplot(netflix)
acf(netflix)
pacf(netflix)
