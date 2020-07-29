library(car)
input <- mtcars
names(input)
cor(input)
model <- lm(mpg~. , data = input) #. indicate all the variables
summary(model)
vif(model) #vif under package Car

#removing Disp variable based on vif value
model <- lm(mpg~. -disp, data = input) #enter method 
summary(model)
vif(model)

#removing cyl based on vif value
model <- lm(mpg~. -disp-cyl, data = input) #enter method 
summary(model)
vif(model)

#doing forward and backward and stepwise regression
model1 <- lm(mpg~., data = input)
summary(model1)
vif(model1)

#backward method (step is used to specify the direction)
model2 <- step(lm(mpg~., data=input), direction = "backward")
summary(model2)
vif(model2)
par(mfrow=c(2,2))
plot(model2)
library(lmtest)
dwtest(model2) #dwtest and ncvtest works under lmtest
ncvTest(model2)
# 
# > ncvTest(model2)
# Non-constant Variance Score Test 
# Variance formula: ~ fitted.values 
# Chisquare = 1.55815, Df = 1, p = 0.21194 
#here p value is > than 0.05, we accept H0

#stepwise method
model3 <- step(lm(mpg~., data= input), direction = "both")
summary(model3)
vif(model3)

#forward method

model4 <-  step(lm(mpg~., data = input), direction = "forward")
summary(model4)
vif(model4)

