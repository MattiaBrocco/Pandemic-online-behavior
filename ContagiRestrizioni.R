library(sm)
library(gam)
library(fpp2)
library(lmtest)
library(DIMORA)
#library(rpanel)
library(splines)
library(ggplot2)
library(forecast)
library(lubridate)


source("support.R")
source("complexitynonparam.R")


# Import data----
AMZN_weekly <- read.timeseries.stocks("./data/AMZN_weekly.csv")
NFLX_weekly <- read.timeseries.stocks("./data/NFLX_weekly.csv")
ZOOM_weekly <- read.timeseries.stocks("./data/ZM_weekly.csv")
ZOOM_weekly <- backcast.ZOOM(ZOOM_weekly)
covid.data <- read.covid.data("./data/Contagi_and_restrictions.csv")

### AMZN stock----
par(mar = c(5.1, 4.1, 4.1, 2.1) + 0.3)  # Leave space for z axis
plot(covid.data$Date, AMZN_weekly$Close,
     type = "l", lwd = 2, xlab = "Time", ylab = "",
     main = "XXX", col = "#FF9B00") # first plot
par(new = TRUE)
plot(covid.data$Date, covid.data$StringencyIndex_Average_ForDisplay,
     type = "l", axes = FALSE, bty = "n", xlab = "", ylab = "", lwd = 2)
axis(side=4, at = pretty(range(covid.data$StringencyIndex_Average_ForDisplay)))
mtext("z", side=4, line=3)


### NFLX stock---- BOZZA !!!!
par(mar = c(5.1, 4.1, 4.1, 2.1) + 0.3)  # Leave space for z axis
plot(covid.data$Date, NFLX_weekly$Close,
     type = "l", lwd = 2, xlab = "Time", ylab = "",
     main = "XXX", col = "#FF9B00") # first plot
par(new = TRUE)
plot(covid.data$Date, covid.data$StringencyIndex_Average_ForDisplay,
     type = "l", axes = FALSE, bty = "n", xlab = "", ylab = "", lwd = 2)
axis(side=4, at = pretty(range(covid.data$StringencyIndex_Average_ForDisplay)))
mtext("z", side=4, line=3)



