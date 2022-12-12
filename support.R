library(sm)
library(gam)
library(fpp2)
library(lmtest)
library(DIMORA)
library(splines)
library(ggplot2)
library(forecast)
library(lubridate)

#####1: Clean stocks------
read.timeseries.stocks <- function(path){
  data <- read.csv(path, sep=",")
  # separator
  if (dim(data)[2] == 1){data <- read.csv(path, sep=";")}
  else {data <- data}
  # columns selection
  data <- data[, c(1, 6)]
  colnames(data) = c("Time", "Close")
  # date format
  data$Time <- as.Date(data$Time)
  if (typeof(data$Time) == "character"){
    data$Time <- as.Date(data$Time, format = "%d/%m/%Y")
  }
  else {data <- data}
  
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
  require("ggplot2")
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