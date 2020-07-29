king<- scan("C:/Users/Mamta/Desktop/kings.csv",skip=3)
kings_tms<-ts(king)
kings_tms
plot.ts(kings_tms)
library('TTR')
kings_tmsSMA <-SMA(kings_tms,n=8)
plot.ts(kings_tmsSMA)
kings_tmsdiff <- diff(kings_tms,differences = 1)
plot.ts(kings_tmsdiff)
#plot a correlogram
acf(kings_tmsdiff,lag.max = 20) 
acf(kings_tmsdiff,lag.max = 20,plot = FALSE)
pacf(kings_tmsdiff,lag.max = 20)
pacf(kings_tmsdiff,lag.max = 20,plot = FALSE)
kings_tmsARIMA <-arima(kings_tms,order = c(0,1,1))
kings_tmsARIMA
library("forecast")
kings_tmsforecast<- forecast(kings_tmsARIMA,h=5)
kings_tmsforecast
plot(kings_tmsforecast)
acf(kings_tmsforecast$residuals,lag.max = 20)
Box.test(kings_tmsforecast$residuals,lag = 20,type = "Ljung-Box")

plot.ts(kings_tmsforecast$residuals)
plotForecasrErrors(kings_tmsforecast$residuals)
