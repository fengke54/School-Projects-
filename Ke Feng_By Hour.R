#By Hour
#-----------Import Required Libraries-------------
library(parsedate)
library(ggplot2)
library(forecast)
library(tseries)

#----Data Loading and Pre-Processing--------------------
#This is a high-frequency data. When dealing high-frequency data with multiple transactions in one day, we sum them up and count the sum sales per day as our y-variable.  
or=read.csv("C:/Users/kfeng3/Desktop/or.csv")
or1=na.omit(or)
sales=or1$sales=or1$Quantity * or1$UnitPrice
hour=as.POSIXct(or1$InvoiceDate, format="%m/%d/%Y %H")
head(hour)
hour
sumsales=aggregate(sales, by=list(hour), FUN=sum)
sumsales   
ssales=sumsales$x
t=sumsales$Group.1

#---Initial Plot----------------------------------------
qplot(t, ssales, geom="line", xlab="Date", ylab="Sum of Daily Sales", main="Online Retail Daily Transcation from Dec 2010 to Dec 2011")
acf(ssales, lag=50)
pacf(ssales, lag=50)
eacf(ssales)
adf.test(ssales)

#--------Deseasonality-------------------------------
#Assume there is a seasonal unit root Zt=(1-L12)Yt
seasonaldiff=diff(ssales, 10)
plot(ssales, type="l")
acf(seasonaldiff, lag=80)
pacf(seasonaldiff, lag=80)
adf.test(seasonaldiff)
plot(seasonaldiff, type="l")

#SARIMA Model Fitting
m1=arima(seasonaldiff, order=c(2,1,0), seasonal=list(order=c(0,1,1), period=10))
m1
BIC(m1)
coeftest(m1)

#Residual Test
Box.test(m1$residuals)
qqnorm(m1$residuals)
hist(m1$residuals)

#Forecast
pred=forecast(m1,50)
pred
plot(pred)


