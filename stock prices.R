library(ggplot2)
library(lubridate)
# Import Data and Preprocessing--------------------------------------------------------------------
#AMAZON
AMZN_weekly = read.csv("C:\\Users\\flavi\\OneDrive\\Desktop\\UNIPD DATA SCIENCE\\SECOND YEAR\\Business Economic and Financial Data\\progetto\\data\\AMZN_weekly.csv", sep = ";")

AMZN_weekly = AMZN_weekly[, c(1, 6)]
colnames(AMZN_weekly) = c('Time', 'Close')
AMZN_weekly$Time = as.Date(AMZN_weekly$Time, format = "%d/%m/%Y")
str(AMZN_weekly)

#NETFLIX
NFLX_weekly = read.csv("C:\\Users\\flavi\\OneDrive\\Desktop\\UNIPD DATA SCIENCE\\SECOND YEAR\\Business Economic and Financial Data\\progetto\\data\\NFLX_weekly.csv", sep = ";")

NFLX_weekly = NFLX_weekly[, c(1, 6)]
colnames(NFLX_weekly) = c('Time', 'Close')
NFLX_weekly$Time = as.Date(NFLX_weekly$Time, format = "%d/%m/%Y")
str(NFLX_weekly)

#ZOOM
ZM_weekly = read.csv("C:\\Users\\flavi\\OneDrive\\Desktop\\UNIPD DATA SCIENCE\\SECOND YEAR\\Business Economic and Financial Data\\progetto\\data\\ZM_weekly.csv", sep = ";")

ZM_weekly = ZM_weekly[, c(1, 6)]
colnames(ZM_weekly) = c('Time', 'Close')
ZM_weekly$Time = as.Date(ZM_weekly$Time, format = "%d/%m/%Y")
str(ZM_weekly)

empty_data = data.frame(AMZN_weekly[1:71,1], rep(0, 71)) 
colnames(empty_data) = c('Time', 'Close')
ZM_weekly = rbind(empty_data, ZM_weekly)

# Plot with old dataset -------------------------------------------------------------------------
Close_Data = data.frame(NFLX_weekly$Time, AMZN_weekly$Close, NFLX_weekly$Close, ZM_weekly$Close)
colnames(Close_Data) = c('Time', 'Amazon', 'Netflix', 'Zoom')

ggplot() +
  geom_line(data = Close_Data, aes(x = Time, y = Amazon), size = 1, color="dark blue") +
  geom_line(data = Close_Data, aes(x = Time, y = Netflix), size = 1, color="dark red") +
  geom_line(data = Close_Data, aes(x = Time, y = Zoom), size = 1, color="light blue") +
  theme_bw()

plot(ts(Close_Data[c(2,3,4)], freq=365.25/7, start = decimal_date(as.Date("2017-12-04"))), main = "Close values")

