install.packages("TTR")
install.packages("tseries")
library("TTR")
library("tseries")

#Read data into R

HDFC_data = read.csv('E:\\Pradeep_pc_backup\\Pradeep_pc_backup\\Data_Science_Material\\Data_science_certification\\NSE-HDFC.csv')

HDFCtimeseries<-HDFC_data[ ,c('Close')]

#TS Object
HDFCtimeseries <- ts(HDFCtimeseries,frequency = 12,  start=c(1998,1),end=(2017))

table(is.na(HDFCtimeseries))

HDFCtimeseries = na.omit(HDFCtimeseries)

table(is.na(HDFCtimeseries))

plot.ts(HDFCtimeseries)

#Decompose data
HDFCtimeseriescomponents <- decompose(HDFCtimeseries)
plot(HDFCtimeseriescomponents)

HDFCtimeseriescomponents$trend
plot(HDFCtimeseriescomponents$trend)

HDFCtimeseriescomponents$seasonal
plot(HDFCtimeseriescomponents$seasonal)

# HDFCtimeseriesSMA3 <- SMA(HDFCtimeseries,n=3)
# plot.ts(HDFCtimeseriesSMA3)
# 
# HDFCtimeseriesSMA8 <- SMA(HDFCtimeseries,n=8)
# plot.ts(HDFCtimeseriesSMA8)
# 
# HDFCtimeseriesSMA12 <- SMA(HDFCtimeseries,n=12)
# plot.ts(HDFCtimeseriesSMA12)

#Moving Average Forecast
moving_average = forecast(ma(HDFCtimeseries, order=3), h=4)
moving_average;
plot(moving_average, ylim=c(0,60))
lines(HDFCtimeseries)

#Forecast using Exponential Smoothing using Holtwinters 
HDFCforecast <- HoltWinters(HDFCtimeseries, beta=FALSE, gamma=FALSE)
HDFCforecast

HDFCforecast$fitted

plot(HDFCforecast)

HDFCforecast$SSE

library("forecast")

HDFCforecast3 <- forecast.HoltWinters(HDFCforecast, h=3)

HDFCforecast3

plot.forecast(HDFCforecast3)

#Forecast using ARIMA

#removing trend and seasonal and making it constant
HDFCtimeseriesdiff1 <- diff(HDFCtimeseries, differences=1)
HDFCtimeseriesdiff2 <- diff(HDFCtimeseries, differences=2)
plot.ts(HDFCtimeseriesdiff2)

#Checking for stationary
adf.test(HDFCtimeseriesdiff1,alternative = 'stationary')

# creating lags

acf(HDFCtimeseriesdiff1, lag.max=20)
acf(HDFCtimeseriesdiff1, lag.max=20, plot=FALSE)

pacf(HDFCtimeseriesdiff1, lag.max=20)             # plot a partial correlogram
pacf(HDFCtimeseriesdiff1, lag.max=20, plot=FALSE)

HDFCtimeseriesarima <- arima(HDFCtimeseries, order=c(0,1,0)) # fit an ARIMA(0,1,1) model
HDFCtimeseriesarima

library("forecast") # load the "forecast" R library
HDFCtimeseriesarimaforecasts <- forecast.Arima(HDFCtimeseriesarima, h=3)
HDFCtimeseriesarimaforecasts


plot.forecast(HDFCtimeseriesarimaforecasts)
