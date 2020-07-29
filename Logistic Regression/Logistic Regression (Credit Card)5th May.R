# Credit card default data
# A data set containing information on ten thousand customers
# the aim here is to predict which customers will default on 
# their credit card debt.
# A data frame with 10000 observations on the following 4 variables
# Gender, Balance, Income, DPD(Days past due)


Credit <- read.csv(file = "E:\\Imarticus DSP 24\\R Programs\\Logistic Regression\\Credit_card.csv")
View(Credit)
str(Credit)
names(Credit)
attach(Credit)
Credit$target_30<-as.factor(ifelse(Credit$DPD>30,1,0))
Credit$target_60<-as.factor(ifelse(Credit$DPD>60,1,0))
Credit$target_90<-as.factor(ifelse(Credit$DPD>90,1,0))

table(Credit$target_30) #to check the counts
4922/10000 #calculating the bad rate

table(Credit$target_60)
333/10000 #bad rate

table(Credit$target_90)
162/10000

#as our bench mark is 3% to 30% so, target_60 comes under our bench mark (Y)
Credit<- Credit[-c(4,5,7)]

##Simple Logistic regression by selecting one x
#glm (Generalized Linear Models)= use for logistic 
logit <- glm(target_60 ~ balance, data=Credit, family = 'binomial')
summary(logit)
##anova(logit, test = 'Chisq')

## Prediction try with 2000
testing<- data.frame(balance = 2000)
testing.probs <- predict(logit, testing, type = 'response')
#response will give probability [0 to 1] then convert it to range
testing.probs
# output
# 1 
# 0.5857694 58% chance for being a defaulter whose balance is >=2000

#different way of Simple Logistic Regression

library(caret)
Train <- createDataPartition(Credit$target_60, p=0.7, list=FALSE)
training <- Credit[ Train, ] #like vlookup in excel
testing<- Credit[-Train, ]
logit<- glm(target_60~balance, data=training, family='binomial')
summary(logit)
#first run the R code givrn by Irfan sir for ACC
#concordance and discordance
Acc(logit)

#Prediction function applied on test data
testing$probs <- predict(logit,testing, type='response')
testing$predict <- as.factor(ifelse(testing$probs>0.70,1,0))
table(testing$predict, testing$target_60) #confusion matrix
library(e1071)
confusionMatrix(testing$predict, testing$target_60)

##ROC curve
library(ROCR)
#make prediction on training set
predictTrain <- predict(logit, testing, type="response")
#prediction function
ROCRpred <- prediction(predictTrain, testing$target_60)
#performance function is use to fetch
ROCRperf <- performance(ROCRpred, "tpr", "fpr")
#plot ROC curve
plot(ROCRperf)

library(ROCR)
pred= prediction(testing$probs, testing$target_60) #y hat and actyal Y
as.numeric(performance(pred, "auc")@y.values)
# Output
# [1] 0.9456078 we are enable to predict on 94% of customer


#create a new dummy variable for Gender
Credit<-read.csv(file = "E:\\Imarticus DSP 24\\R Programs\\Logistic Regression\\Credit_card.csv")
Credit$target_60<-as.factor(ifelse(Credit$DPD>60,1,0))
Credit$Dummy<-as.factor(ifelse(Credit$Gender=='M',1,0))
Credit$DPD<-NULL #deleting the column
Credit$Gender<-NULL

#Multiple Logistic Regression
library(caret)
Train<- createDataPartition(Credit$target_60, p=0.7, list = FALSE)
training<- Credit[ Train, ]
testing<- Credit[ -Train, ]
logit<- glm(target_60~income+balance+Dummy,family = 'binomial', 
            data=training)
summary(logit)

#model including all variable
logit2 <- step(glm(target_60~income+balance+Dummy, family='binomial', 
                   data=training), direction = "both")
summary(logit2)
Acc(logit2)
##for odd ratio
exp(coef(logit2))
# output
# > exp(coef(logit2))
# (Intercept)      balance       Dummy1 
# 1.086359e-05 1.005745e+00 1.982612e+00 
# Interpret= (1.98261..) i.e 2 times higher chance of males being default


#Mathematical calculation check
y=-11.307323+0.005614*2220.966+0.735054*1
y=-11.307323+0.005614*817.18041+0.735054*0

a<- exp(-y)
b<- 1+a
c<-1/b
c

#gvif work when we have more than 1 independent variable
##Prediction
testing$probs <- predict(logit2, testing, type='response')
testing$predict<- as.factor(ifelse(testing$probs>0.70,1,0))
table(testing$predict, testing$target_60)
confusionMatrix(testing$target_60, testing$predict)

##ROC Curve
library(ROCR)
#make prediction on training set
predictTrain = predict(logit2, testing, type="response")
#Prediction function
ROCRpred = prediction(predictTrain, testing$target_60)
#performance function
ROCRperf = performance(ROCRpred, "tpr", "fpr")

#Plot ROC curve
plot(ROCRperf)
library(ROCR)
pred = prediction(testing$probs, testing$target_60)
as.numeric(performance(pred, "auc")@y.values)
