#Objective: 
#Health insurance co. wants to predict the premium amount/EMI on monthly basis

#data <- read.csv(file.choose())
data <- read.csv(file = "E:\\Imarticus DSP 24\\R Programs\\Linear regression\\insurance.csv")
View(data)
data$region<-NULL #to delete the column
names(data)
str(data) #to check data type
data$age<-as.numeric(data$age)
data$children<-as.numeric(data$children)
table(data$smoker) #to get the yes/no counts
table(data$sex)

#converting catagorical into numeric data by creating a new column
data$smoker_flag <- ifelse(data$smoker == 'yes',1,2)
data$sex_flag<- ifelse(data$sex == 'male',1,2)
data$smoker_flag <- as.numeric(data$smoker_flag)
data$sex_flag <- as.numeric(data$sex_flag)
str(data)

#to check missing values
sapply(data,function(x) sum(is.na(x)))

#to check outlier
boxplot(data$age)
boxplot(data$bmi)
boxplot(data$children)
boxplot(data$charges)
boxplot(data$Month)

#treatment of outlier for bmi with winzorizing technique
summary(data$bmi)
upper <- 34.69 + 1.5 * IQR(data$bmi);
upper
data$bmi[data$bmi > upper]<- upper #replacement of outliers
boxplot(data$bmi)
summary(data$bmi)

#treatment of outlier for charges
summary(data$charges)
upper <- 16640 + 1.5 * IQR(data$charges);
upper
data$charges[data$charges > upper]<- upper
boxplot(data$charges)
summary(data$charges)

#data subset
abc <- data[,-c(2,5)] #deleting column no 2 & 5

#data partition by 70:30
library(caret)

Train <- createDataPartition(abc$charges, p=0.70, list=FALSE)
#list means column, when it is false we break data rowwise
training <- abc[Train, ] #row match data will be saved i.e 70%
testing <- abc[ -Train, ] #row unmatched data will be saved i.e 30%

#model building and may leads to overfitting
cor(training)
model<- lm(charges~., data = training)
summary(model)
#As the adjusted R Square is 1, our model is overfitted

#transformation help us to reduce the power of a number
#it is used to convert the left/right skwed data into normal/bell shaped curve
hist(training$charges)
hist((1/training$charges)) #reciprocal
hist(log(training$charges)) #log transformation in Y variable

#model 2
#Note Y variable values are converted into log values 
model2 <- step(lm(log(charges)~., data=training), direction = "backward")
summary(model2)
library(car)
#Multicollinearity
vif(model2)
#Assumption
par(mfrow = c(2,2))
plot(model2)

library(lmtest)
dwtest(model2)
library(car)
ncvTest(model2)

#prediction
testing$fitted <- predict(model2, testing)
#exp used for antilog
testing$original <- exp(testing$fitted)

