require(sm)
require(gam)
require(fpp2)
require(dplyr)
require(lmtest)
require(DIMORA)
require(splines)
require(ggplot2)
require(forecast)
require(lubridate)

#####0: Backcasting for ZOOM------
backcast.ZOOM <- function(data, data.ref = NFLX_weekly){
  
  start.ts <- ts(data$Close, freq=52,
                 start = decimal_date(ymd("2019-04-15")))
  rev.ts <- ts(rev(start.ts), frequency = 52,
               start=decimal_date(ymd("2019-04-15")))
  # Forecast
  at.arima <- auto.arima(rev.ts, D = 1)
  
  # Number of new data points
  new.x <- dim(data.ref)[1] - dim(data)[1]
  
  fc <- forecast(at.arima, h = new.x)
  
  # Reverse time again
  fc$mean <- ts(rev(fc$mean),end=tsp(start.ts)[1] - 1/52,
                frequency = 52)
  fc$upper <- fc$upper[new.x:1,]
  fc$lower <- fc$lower[new.x:1,]
  fc$x <- start.ts
  
  # Final time series
  bk.casted <- fc$mean %>% as.vector
  bk.df <- data.frame(data.ref[1:new.x, 1], bk.casted) 
  colnames(bk.df) = c('Time', 'Close')
  data.out <- rbind(bk.df, data)
  data.out$Close = pmax(data.out$Close, 0)
  
  return(data.out)
}

#####1: Clean stocks------
read.timeseries.stocks <- function(path, get190 = FALSE){
  data <- read.csv(path, sep=",")
  # separator
  if (dim(data)[2] == 1){
    data <- read.csv(path, sep=";")}
  else {data <- data}
  # columns selection
  data <- data[, c(1, 6)]
  colnames(data) = c("Time", "Close")
  # date format
  
  if (startsWith(as.character(as.Date(data$Time)[1][1]), "0")){
    data$Time <- as.Date(data$Time, format = "%d/%m/%Y")
  }
  else {data$Time <- as.Date(data$Time)}
  
  # remove first and last observations
  data <- slice(data, 2:(n() - 1))
  
  # AD-HOC SLICING FOR ZOOM
  if (get190 == TRUE){
    data <- data[data$Time >= "2019-04-15",]
    data <- data[data$Time <= "2022-11-28",]
  }
  
  return(data)
}

#####2: Clean Google------
read.timeseries.google <- function(path){
  #require("tidyverse")
  data <- read.csv(path, na = "0", skip = 1)
  colnames(data) <- c('Time','Amazon','Netflix', 'Zoom')
  
  if (typeof(data$Zoom) == "character"){
    data$Zoom <- as.numeric(data$Zoom)
    data$Zoom[is.na(data$Zoom)] <- 0
  }
  
  return(data)
}

#####3: Get ST Stocks------
get.tseries <- function(data, start.date){
  SERIES <- ts(data$Close, freq = 52,
               start = decimal_date(ymd(start.date)))
  return(SERIES)
}

#####3: Get ST Stocks------
get.ts.google <- function(inp, start.date){
  SERIES <- ts(inp, freq = 52,
               start = decimal_date(ymd(start.date)))
  return(SERIES)
}

#####4: First GGPLOT------
gg.comparison1 <- function(){
  Close_Data = data.frame(NFLX_weekly$Time, AMZN_weekly$Close,
                          NFLX_weekly$Close, ZM_weekly$Close)
  Close_Data_ts = data.frame(AMZN, NFLX, ZM)
  colnames(Close_Data) = c('Time', 'Amazon', 'Netflix', 'Zoom')
  
  return(
    ggplot() +
    geom_line(data = Close_Data, aes(x = Time, y = Amazon,
                                     color = "Amazon"), size = 1) +
    geom_line(data = Close_Data, aes(x = Time, y = Netflix,
                                     color = "Netflix"), size = 1) +
    geom_line(data = Close_Data, aes(x = Time, y = Zoom,
                                     color = "Zoom"), size = 1) +
    theme_bw() +
    labs(x="Time", y="Close value", color = "Legend") +
    scale_colour_manual("", 
                        breaks = c("Amazon", "Netflix", "Zoom"),
                        values = c("dark blue", "dark red", "light blue"))
  )
}

#####5: Second GGPLOT------
gg.comparison2 <- function(){
  Close_Data = data.frame(NFLX_weekly$Time, AMZN_weekly$Close,
                          NFLX_weekly$Close, ZM_weekly$Close)
  Close_Data_ts = data.frame(AMZN, NFLX, ZM)
  colnames(Close_Data) = c('Time', 'Amazon', 'Netflix', 'Zoom')
  return(plot(ts(Close_Data[c(2,3,4)], freq=365.25/7,
                 start = decimal_date(as.Date("2017-12-04"))),
              main = "Close values"))
}

#####6: COVID-19 data-----
read.covid.data <- function(path, get190 = FALSE){
  data <- read.csv(path, sep=",")
  # separator
  if (dim(data)[2] == 1){
    data <- read.csv(path, sep=";")}
  else {data <- data}
  
  # date format
  if (startsWith(as.character(as.Date(data$Date)[1][1]), "0")){
    data$Date <- as.Date(data$Date, format = "%d/%m/%Y")
  }
  else {data$Date <- as.Date(data$Date)}
  
  # remove first and last observations
  data <- slice(data, 2:(n() - 1))
  
  # AD-HOC SLICING FOR ZOOM
  if (get190 == TRUE){
    data <- data[data$Time >= "2019-04-15",]
    data <- data[data$Time <= "2022-11-28",]
  }
  
  return(data)
}

