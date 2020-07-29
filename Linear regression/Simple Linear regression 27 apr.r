#Linear regression
View(faithful)
attach(faithful)

names(faithful)
str(faithful) #structure of dataset to check the data type
head(faithful,10)

#behaviour and missing value and univariate analysis
summary(faithful)

#another way missing values can be identified
sapply(faithful, function(x) sum(is.na(x)))

#scatter plot and correlation
plot(waiting, eruptions) # plot of (x,y)
#or
plot(eruptions~waiting) #eruption depend on waiting (y~x)
cor(faithful) #cor is correlation
input<-faithful

#simple regression model (lm = linear model)
eruption.lm = lm(eruptions ~ waiting, data = input)
summary(eruption.lm)
# #Coefficients:
#              Estimate Std. Error t value Pr(>|t|)    
# (Intercept) -1.874016   0.160143  -11.70   <2e-16 *** (B0=-1.874016)coeff
#   waiting   0.075628   0.002219   34.09   <2e-16 ***(B1=0.075628)
# formula (y = B0 + B1x1 + B2X2 + ... E)
#Example prediction for 80 minutes waiting time
y=-1.87402 + 0.07563*80
y
  
anova(eruption.lm) 
#will give P value and we can interpret whether we can accept H0 and H1


# assumption of regression model
# below command will plot all the assumption plot in 2*2 format
 #par stands for page
par(mfrow = c(2,2))
plot(eruption.lm)

library(lmtest) #calling lmtest
dwtest(eruption.lm)
# Durbin-Watson test
# data:  eruption.lm
# DW = 2.561, p-value = 1
# alternative hypothesis: true autocorrelation is greater than 0
#interpretaion = H0 is accepted there is no auto correlation

#linearity
plot(eruptions~waiting)

#just to check the prediction mathematically
input<- faithful
Probability<- data.frame(eruption.lm$fitted.values)
Residual<- data.frame(eruption.lm$residuals)
input[,"Probability"]<-Probability$eruption.lm.fitted.values
input[,"Residual"]<-Residual$eruption.lm.residuals
summary(eruption.lm)
plot(eruptions~waiting)
abline(eruption.lm,col= "red") #bestfit line

#prediction
newdata = data.frame(waiting= 80)
predict(eruption.lm, newdata)

