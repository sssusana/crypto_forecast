#SISS

#Libraries
install.packages("forecast")
library('ggplot2')
library('forecast')
library('tseries')

#Load dataset
crypto_data <- read_csv("~/Documents/GitHub/crypto_forecast/ethereum_classic_price.csv")
crypto_data <- read_csv("~/Documents/GitHub/crypto_forecast/crypto.csv")
crypto_data <- crypto_data[c(1:326), ] #from mar 2017 and on
#crypto data
head(crypto_data)
View(crypto_data)
#Converting Date from char - Jan 29, 2018 - to date
crypto_data$Date<-(gsub("\\,", "", crypto_data$Date))

today <- Sys.Date()
format(today, format= "%B %d %Y")
crypto_data$Date <- as.Date(crypto_data$Date, "%b %d %Y")

crypto_data$Date = as.Date(crypto_data$Date)

#Model - removing outliers and creating a clean column; plot it.

CloseO  <- ts(crypto_data$Close)
crypto_data$CloseClean = tsclean(CloseO, replace.missing = FALSE)
ggplot() +
  geom_line(data = crypto_data, aes(x = Date, y = CloseClean)) + ylab('Close Values') + 
  scale_x_date(date_labels = "%b-%y", date_breaks = '2 month') +
  scale_y_continuous(name = crypto_data$CloseClean)+
  scale_y_continuous(labels = scales::dollar)


#Creating 2 models: montly moving average and weekly moving average 

crypto_data$MA7 = ma(crypto_data$CloseClean, order=7) # weekly
crypto_data$MA30 = ma(crypto_data$CloseClean, order=30) # montly


#Plotting the three models

ggplot() +
  geom_line(data = crypto_data, aes(x = Date, y = CloseClean, colour = "Value")) +
  geom_line(data = crypto_data, aes(x = Date, y = MA7,   colour = "Weekly Moving Average"))  +
  geom_line(data = crypto_data, aes(x = Date, y = MA30, colour = "Monthly Moving Average"))  +
  ylab('Close Value') +
  scale_x_date(date_labels = "%b-%y", date_breaks = '2 month') +
  scale_y_continuous(labels = scales::dollar)


# Decomposing models (weekly average)
value_ma <- ts(na.omit(crypto_data$MA7), frequency = 30 )
decomp = stl(value_ma, s.window="periodic")
deseasonal_MA7 <- seasadj(decomp)
plot(decomp)

