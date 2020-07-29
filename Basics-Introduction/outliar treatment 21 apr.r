#outliar treatment
d <- 1:20
d
library(scales)
#capping and floaring example at 10 percentage above and below
#squish function works under scale
d2 <- squish(d, round(quantile(d, c(.10, .90))))
d2

#outliar replacement using box plot and quantile strategy
old<-mtcars
mtcars<-mtcars

summary(mtcars$hp)
boxplot(mtcars$hp)

#creating upper limit value
upper<- 180+1.5*IQR(mtcars$hp)
upper

#creating lower limit value
lower<- 96.5-1.5*IQR(mtcars$hp)
lower

#replacement of outlier with upper limit value
mtcars$hp[mtcars$hp > upper]<- upper
summary(mtcars$hp)
boxplot(mtcars$hp)

#lower limit replacement
mtcars$hp[mtcars$hp < lower]<- lower
summary(mtcars$hp)

#outliar treatment with quantile
outputQuantile <- quantile(faithful$waiting, seq(0,1,by=0.05)) #breakup from 0 to 100 at 5%
outputQuantile
cbind(outputQuantile) #for getting the output in column wise

#manually replacement of highest and lowest value through capping and floaring
qn = quantile(faithful$waiting, c(0.01, 0.99), na.rm = TRUE)
df = within(faithful, 
            {
              waiting = ifelse(waiting < qn[1], qn[1], waiting)
              waiting = ifelse(waiting > qn[2], qn[2], waiting)
              }
            )
summary(faithful)
summary(df)
