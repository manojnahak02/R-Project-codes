data<- scan("C:/Users/Mamta/Desktop/souvenir.csv")
souvenir_tms <- ts(data,frequency = 12,start = c(1987,1))
souvenir_tms
plot.ts(souvenir_tms)
logsouvenir_tms<- log(souvenir_tms)
plot.ts(logsouvenir_tms)
souvenir_tmsforecast<- HoltWinters(logsouvenir_tms)
souvenir_tmsforecast
plot(souvenir_tmsforecast)
library('forecast')
souvenir_tmsforecast2 <- forecast(souvenir_tmsforecast,h=48)
plot(souvenir_tmsforecast2)
