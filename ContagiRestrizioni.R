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

# - Amazon  #FF9B00
# - Netflix #E50914
# - Zoom    #2D8CFF


# Import data----
AMZN_weekly <- read.timeseries.stocks("./data/AMZN_weekly.csv")
NFLX_weekly <- read.timeseries.stocks("./data/NFLX_weekly.csv")
ZOOM_weekly <- read.timeseries.stocks("./data/ZM_weekly.csv")
ZOOM_weekly <- backcast.ZOOM(ZOOM_weekly)

Google_Trends <- read.csv("./data/Google Trends Data.csv",
                          na = "0", skip = 1)
names(Google_Trends) <- c("Date", "Amazon", "Netflix", "Zoom")

covid.data <- read.covid.data("./data/Contagi_and_restrictions.csv")
names(covid.data) <- c("Date", "New_cases", "AvgStringencyIndex")

### AMZN stock----
par(mar = c(5.1, 4.1, 4.1, 2.1) + 0.3)  # Leave space for z axis
plot(covid.data$Date, AMZN_weekly$Close,
     type = "l", lwd = 2, xlab = "Time", ylab = "",
     main = "Avg Stringency Index & AMZN", col = "#FF9B00") # first plot
par(new = TRUE)
plot(covid.data$Date, covid.data$AvgStringencyIndex,
     type = "l", axes = FALSE, bty = "n", xlab = "", ylab = "", lwd = 2)
axis(side=4, at = pretty(range(covid.data$AvgStringencyIndex)))
mtext("z", side=4, line=3)

par(mar = c(5.1, 4.1, 4.1, 2.1) + 0.3)  # Leave space for z axis
plot(covid.data$Date, AMZN_weekly$Close,
     type = "l", lwd = 2, xlab = "Time", ylab = "",
     main = "Weekly COVID cases & AMZN", col = "#FF9B00")
par(new = TRUE)
plot(covid.data$Date, covid.data$New_cases,
     type = "l", axes = FALSE, bty = "n", xlab = "", ylab = "", lwd = 2)
axis(side=4, at = pretty(range(covid.data$New_cases)))
mtext("z", side=4, line=3)

### NFLX stock----
par(mar = c(5.1, 4.1, 4.1, 2.1) + 0.3)  # Leave space for z axis
plot(covid.data$Date, NFLX_weekly$Close,
     type = "l", lwd = 2, xlab = "Time", ylab = "",
     main = "Avg Stringency Index & NFLX", col = "#E50914")
par(new = TRUE)
plot(covid.data$Date, covid.data$AvgStringencyIndex,
     type = "l", axes = FALSE, bty = "n", xlab = "", ylab = "", lwd = 2)
axis(side=4, at = pretty(range(covid.data$AvgStringencyIndex)))
mtext("z", side=4, line=3)

par(mar = c(5.1, 4.1, 4.1, 2.1) + 0.3)  # Leave space for z axis
plot(covid.data$Date, NFLX_weekly$Close,
     type = "l", lwd = 2, xlab = "Time", ylab = "",
     main = "Weekly COVID cases & NFLX", col = "#E50914") # first plot
par(new = TRUE)
plot(covid.data$Date, covid.data$New_cases,
     type = "l", axes = FALSE, bty = "n", xlab = "", ylab = "", lwd = 2)
axis(side=4, at = pretty(range(covid.data$New_cases)))
mtext("z", side=4, line=3)


### ZOOM stock----
par(mar = c(5.1, 4.1, 4.1, 2.1) + 0.3)  # Leave space for z axis
plot(covid.data$Date, ZOOM_weekly$Close,
     type = "l", lwd = 2, xlab = "Time", ylab = "",
     main = "Avg Stringency Index & ZM", col = "#2D8CFF")
par(new = TRUE)
plot(covid.data$Date, covid.data$AvgStringencyIndex,
     type = "l", axes = FALSE, bty = "n", xlab = "", ylab = "", lwd = 2)
axis(side=4, at = pretty(range(covid.data$AvgStringencyIndex)))
mtext("z", side=4, line=3)

par(mar = c(5.1, 4.1, 4.1, 2.1) + 0.3)  # Leave space for z axis
plot(covid.data$Date, ZOOM_weekly$Close,
     type = "l", lwd = 2, xlab = "Time", ylab = "",
     main = "Weekly COVID cases & ZM", col = "#2D8CFF") # first plot
par(new = TRUE)
plot(covid.data$Date, covid.data$New_cases,
     type = "l", axes = FALSE, bty = "n", xlab = "", ylab = "", lwd = 2)
axis(side=4, at = pretty(range(covid.data$New_cases)))
mtext("z", side=4, line=3)

### AMZN trends----
par(mar = c(5.1, 4.1, 4.1, 2.1) + 0.3)  # Leave space for z axis
plot(covid.data$Date, Google_Trends$Amazon,
     type = "l", lwd = 2, xlab = "Time", ylab = "",
     main = "Avg Stringency Index & Amazon popularity", col = "#FF9B00")
par(new = TRUE)
plot(covid.data$Date, covid.data$AvgStringencyIndex,
     type = "l", axes = FALSE, bty = "n", xlab = "", ylab = "", lwd = 2)
axis(side=4, at = pretty(range(covid.data$AvgStringencyIndex)))
mtext("z", side=4, line=3)

par(mar = c(5.1, 4.1, 4.1, 2.1) + 0.3)  # Leave space for z axis
plot(covid.data$Date, Google_Trends$Amazon,
     type = "l", lwd = 2, xlab = "Time", ylab = "",
     main = "Weekly COVID cases & Amazon popularity", col = "#FF9B00")
par(new = TRUE)
plot(covid.data$Date, covid.data$New_cases,
     type = "l", axes = FALSE, bty = "n", xlab = "", ylab = "", lwd = 2)
axis(side=4, at = pretty(range(covid.data$New_cases)))
mtext("z", side=4, line=3)


### NFLX trends----
par(mar = c(5.1, 4.1, 4.1, 2.1) + 0.3)  # Leave space for z axis
plot(covid.data$Date, Google_Trends$Netflix,
     type = "l", lwd = 2, xlab = "Time", ylab = "",
     main = "Avg Stringency Index & Netflix popularity", col = "#E50914")
par(new = TRUE)
plot(covid.data$Date, covid.data$AvgStringencyIndex,
     type = "l", axes = FALSE, bty = "n", xlab = "", ylab = "", lwd = 2)
axis(side=4, at = pretty(range(covid.data$AvgStringencyIndex)))
mtext("z", side=4, line=3)

par(mar = c(5.1, 4.1, 4.1, 2.1) + 0.3)  # Leave space for z axis
plot(covid.data$Date, Google_Trends$Netflix,
     type = "l", lwd = 2, xlab = "Time", ylab = "",
     main = "Weekly COVID cases & Netflix popularity", col = "#E50914")
par(new = TRUE)
plot(covid.data$Date, covid.data$New_cases,
     type = "l", axes = FALSE, bty = "n", xlab = "", ylab = "", lwd = 2)
axis(side=4, at = pretty(range(covid.data$New_cases)))
mtext("z", side=4, line=3)

### ZOOM trends----
par(mar = c(5.1, 4.1, 4.1, 2.1) + 0.3)  # Leave space for z axis
plot(covid.data$Date, Google_Trends$Zoom,
     type = "l", lwd = 2, xlab = "Time", ylab = "",
     main = "Avg Stringency Index & Zoom popularity", col = "#2D8CFF")
par(new = TRUE)
plot(covid.data$Date, covid.data$AvgStringencyIndex,
     type = "l", axes = FALSE, bty = "n", xlab = "", ylab = "", lwd = 2)
axis(side=4, at = pretty(range(covid.data$AvgStringencyIndex)))
mtext("z", side=4, line=3)

par(mar = c(5.1, 4.1, 4.1, 2.1) + 0.3)  # Leave space for z axis
plot(covid.data$Date, Google_Trends$Zoom,
     type = "l", lwd = 2, xlab = "Time", ylab = "",
     main = "Weekly COVID cases & Zoom popularity", col = "#2D8CFF")
par(new = TRUE)
plot(covid.data$Date, covid.data$New_cases,
     type = "l", axes = FALSE, bty = "n", xlab = "", ylab = "", lwd = 2)
axis(side=4, at = pretty(range(covid.data$New_cases)))
mtext("z", side=4, line=3)

