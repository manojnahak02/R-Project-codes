#Multiple regression
#taking specific columns from mtcars for prediction
#Objective :- The goal of the model is to establish the relationship btwn
#"mpg" as a response variable with "disp", "hp" and "wt" as predictor variables.
# Y varaible is mpg and remaining are X variables
# respensive variable which effect the milage(mpg) of car (correlation)

input <- mtcars[,c("mpg","disp","hp","wt")]
names(input)
cor(input)
attach(input)
plot(hp,mpg) #graph shows -ve correlation
model <- lm(mpg~disp+hp+wt, data = input)
summary(model)
# 
# Coefficients:
#            Estimate Std. Error t value Pr(>|t|)    
#(Intercept) 37.105505   2.110815  17.579  < 2e-16 ***
#disp     -0.000937   0.010350  -0.091  0.92851 >0.05 doesnot have significant impact, we can remove it
#hp       -0.031157   0.011436  -2.724  0.01097 *  <0.05 have significant impact
#wt       -3.800891   1.066191  -3.565  0.00133 ** <0.05 have significant impact


model2 <- lm(mpg~hp+wt, data = input) #removed disp
summary(model2)

#reading P vale
#1.12e-06 *** = 0.00000112 which is < 0.05

par(mfrow=c(2,2))
plot(model2)

y= 37.22727-0.03177*110-3.87783*3.435;y
# 
# in console type
# Hit <return> to see next plot
