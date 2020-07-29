#Objective: 
#Health insurance co. wants to predict the premium amount/EMI on monthly basis
data<-read.csv(file ="E:/Imarticus DSP 24/R Programs/Linear regression/insurance.csv")
names(data)
str(data)
data$sex<- as.numeric(ifelse(data$sex=='female',0,1))
data$smoker<-as.numeric(ifelse(data$smoker=='yes',1,0))
data$age<-as.numeric(data$age)
data$children<-as.numeric(data$children)
data$region<-NULL
#data$Month<-NULL
str(data)
sapply(data, function(x)sum(is.na(x)))
boxplot(data$age)
boxplot(data$sex)
boxplot(data$bmi)
summary(data$bmi)
upper<-34.69+1.5*IQR(data$bmi);
upper
data$bmi[data$bmi > upper]<-upper
boxplot(data$bmi)

boxplot(data$children)

boxplot(data$charges)
summary(data$charges)
upper<-16640 + 1.5 * IQR(data$charges);
upper
data$charges[data$charges > upper] <- upper
boxplot(data$charges)

library(caret)
Train<-createDataPartition(data$charges,p=0.7, list=FALSE)
training <- data[Train,]
testing <- data[-Train,]
cor(training)

model<-lm(charges~.,data=training)
summary(model)

hist(training$charges)
hist((1/training$charges)) #reciprocal
hist(log(training$charges)) #log transformation in Y variable

model2<-step(lm(log(charges)~., data = training),direction = "backward")
summary(model2)

library(car)
vif(model2)
par(mfrow=c(2,2))
plot(model2)

library(lmtest)
dwtest(model2)
library(car)
ncvTest(model2)

testing$fitted<-predict(model2, testing)
testing$original<- exp(testing$fitted)
