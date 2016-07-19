


fraser = scan("fraser.txt") 
head(fraser)

plot(fraser)

fraser.ts <- ts(fraser, frequency=12, start=c(1913,3)) #Data is monthly so, freq = 12 and starting from March, 2013
head(fraser.ts)

#1) Seasonal decomposition of time series 
stl(fraser.ts, s.window="periodic")


#populating a variable so we can do further work,
fraser.stl = stl(fraser.ts, s.window="periodic")
summary(fraser.stl)

monthplot(fraser.stl)
#river flow starts to increase in the spring, crests by early summer, and then tails off into winter 

library(forecast)
seasonplot(fraser.ts)


plot(fraser.stl)
#seasonality graph is perfect 
#For Trend; few years were wetter than others 
#remainder (noise/error) is pretty constant

#2) The SMA function
#n is the number of periods to average over
library(TTR)
fraser.SMA3 <- SMA(fraser,n=12)
plot(fraser.SMA3)

#Stretching to 5 Years 
fraser.SMA60 <- SMA(fraser,n=60)
fraser.SMA60 <- SMA(fraser,n=60)
plot(fraser.SMA60)
#clear, long-term changes to the flow rate.  doesn't really trend in either direction

#3) The decompose function
fraser.components <- decompose(fraser.ts)
#recalculating without seasonality 
fraser.adjusted <- fraser - fraser.components$season
plot(fraser.adjusted)
#there is no long term trend with the flows
# wide variances at times, but I am still guessing that is due to some shorter-term weather patterns

#4)Exponential smoothing
fraser.forecast <- HoltWinters(fraser.ts,beta=FALSE)
fraser.forecast
fraser.forecast$SSE
plot(fraser.forecast)
#The estimated data appears to have same level of variance

fraser.forecast$fitted


#Note; Simple Moving average (SMA) and Expoential Moving Average (EMA) explained
#http://www.babypips.com/school/elementary/moving-averages/simple-moving-averages.html

#5) Forecast
install.packages("forecast")
library(forecast)
fraser.forecast2 <- forecast.HoltWinters(fraser.forecast, h=8)
fraser.forecast2
plot.forecast(fraser.forecast2)
#Correlogram
acf(fraser.forecast2$residuals,lag.max=20)
#the most recent flow rate of the river has the biggest effect on the current flow rate, as expected

#Box test
Box.test(fraser.forecast2$residuals,lag=20,type="Ljung-Box")
#very small p-value, so we have a good fit
plot.ts(fraser.forecast2$residuals) #looking at the residuals 
#Nothing jumps out particularly. Still, it seems like a large variance

-----------

##AUTOMATED FORECASTING (will automatically select exponential and ARIMA models)
fraser.ets <- ets(fraser.ts)
#The alpha value is about double the values chosen in the previous examples
#The gamma (seasonality) value is enormous
plot(fraser.ets)
#The results are a match to the previous similar graph from the results of the stl call

##ARIMA
auto.arima(fraser.ts) #ARIMA selection
fraser.arima <- arima(fraser.ts, order=c(1,0,0))
summary(fraser.arima)
#Sigma squared of 2.14 million, which is very high
#Log likelihood of 8,000
tsdisplay(arima.errors(fraser.arima))
#forecast using the ARIMA 
fraser.farima <- forecast(fraser.arima, h=8)
summary(fraser.farima)
plot(fraser.farima)


##Automated ARIMA forecasting
fraser.aarima <- auto.arima(fraser.ts)
summary(fraser.aarima)
#Sigma squared of 500,000
#Log likelihood of 7,500

# regenerating the model using the values chosen
fraser.arima3 <- arima(fraser.ts, order=c(4,0,0), seasonal=list(order=c(1,0,0), period=12))
summary(fraser.arima3)
#Negative likelihood shows an excellent fit
fraser.farima3 <- forecast(fraser.arima3, h=8)
summary(fraser.farima3)
plot(fraser.farima3)
#We can see a larger value in the forecast part of the plot. The forecast also does not appear to go as far below the axis 
#(negative) as in our previous example. The y axis is also shorter.