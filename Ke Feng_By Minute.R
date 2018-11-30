#By Minutes
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
minutes=as.POSIXct(or1$InvoiceDate, format="%m/%d/%Y %H:%M")
head(minutes)
minutes
sumsales=aggregate(sales, by=list(minutes), FUN=sum)
sumsales   
ssales=sumsales$x
t=sumsales$Group.1

#---Initial Plot----------------------------------------
qplot(t, ssales, geom="line", xlab="Date", ylab="Sum of Daily Sales", main="Online Retail Daily Transcation from Dec 2010 to Dec 2011")
acf(ssales, lag=50)
pacf(ssales, lag=50)
eacf(ssales)
adf.test(ssales)

#Model Fitting
m1=auto.arima(ssales)
m1
BIC(m1)

#Residual Test
Box.test(m1$residuals)
qqnorm(m1$residuals)
hist(m1$residuals)

m2=arima(ssales,order=c(0,0,2))
m2
BIC(m2)

#Residual Test
Box.test(m2$residuals)
qqnorm(m2$residuals)
hist(m2$residuals)

#Forecast
pred=forecast(m2,50)
pred
plot(pred)

