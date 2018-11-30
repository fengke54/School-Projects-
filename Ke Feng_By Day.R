####By Day Model
#-----------Import Required Libraries-------------
library(methods)
library(tseries)
library(forecast)
library(lubridate)
library(TSA)
library(dplyr)
library(ggplot2)
library(lmtest)
library(fBasics)

#----Data Loading and Pre-Processing--------------------
#This is a high-frequency data. When dealing high-frequency data with multiple transactions in one day, we sum them up and count the sum sales per day as our y-variable.  
or=read.csv("C:/Users/kfeng3/Desktop/or.csv")
or1=na.omit(or)
sales=or1$sales=or1$Quantity * or1$UnitPrice
date=or1$InvoiceDate=as.POSIXct(or1$InvoiceDate, format="%m/%d/%Y")
date    
sumsales=aggregate(sales, by=list(date), FUN=sum)
sumsales   
ssales=sumsales$x
t=as.Date(sumsales$Group.1, format="%m/%d%/Y")
t            

#---Initial Plot----------------------------------------
qplot(t, ssales, geom="line", xlab="Date", ylab="Sum of Daily Sales", main="Online Retail Daily Transcation from Dec 2010 to Dec 2011")
acf(ssales, lag=40)
pacf(ssales, lag=40)
eacf(ssales)
adf.test(ssales)

#--------Deseasonality-------------------------------
seasonaldiff=diff(ssales, 6)
plot(seasonaldiff, type="l", main="Plot after De-season")
adf.test(seasonaldiff)
acf(seasonaldiff, lag.max =40)
pacf(seasonaldiff, lag.max =40)

#SARIMA Model Fitting
m1=arima(seasonaldiff, order=c(0,1,4), seasonal=list(order=c(2,1,1), period=6))
m1
BIC(m1)
coeftest(m1)

#dropping insignificant varaibles 
m2=arima(seasonaldiff, order=c(0,NA,NA,NA), seasonal=list(order=c(2,1,1), period=6))
m2
m2=arima(seasonaldiff, order=c(0,1,1), seasonal=list(order=c(2,1,1), period=6))
m2
coeftest(m2)
BIC(m2)

#Residual Test
Box.test(m2$residuals)
qqnorm(m2$residuals)
hist(m2$residuals)

#ARIMA Model Building
m3=auto.arima(seasonaldiff, seasonal = F)
m3
BIC(m3)
coeftest(m3)
#Residual Test
Box.test(m3$residuals)
qqnorm(m3$residuals)
hist(m3$residuals)

#BIC Comparison
BIC(m2)
BIC(m3)

#Forecast
pred=forecast(m2,h=10)
pred
plot(pred, main="Prediction Plot")

#Backtest
btest=0.80*length(ssales)
btest1=backtest(m2, ssales, btest, 1, by.period = F)
btest2=backtest(m3, ssales, btest, 1, by.period = F)

#Garch Model, why Garch doesn't work 
logreturns=diff(log(ssales), lag=1)
r=logreturns
abs(r)
acf(r)
acf(abs(r))
r2=r*r
acf(r2)
Box.test(abs(r), type="Ljung")
Box.test(r2, type="Ljung")



