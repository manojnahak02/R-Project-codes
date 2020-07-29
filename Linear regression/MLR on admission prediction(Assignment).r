#Objective: 
#prediction of Graduate Admissions from an Indian perspective
#adm<- read.csv(file.choose())
adm <- read.csv(file = "E:\\Imarticus DSP 24\\R Programs\\Linear regression\\admission prediction.csv")
names(adm)
str(adm)
#converting interger to numeric data type
adm$GRE.Score<-as.numeric(adm$GRE.Score)
adm$TOEFL.Score<-as.numeric(adm$TOEFL.Score)
adm$University.Rating<-as.numeric(adm$University.Rating)
adm$Research<-as.numeric(adm$Research)
str(adm)

#to check outlier
boxplot(adm$GRE.Score)
boxplot(adm$TOEFL.Score)
boxplot(adm$University.Rating)
boxplot(adm$SOP)
boxplot(adm$LOR)
boxplot(adm$CGPA)
boxplot(adm$Research)
boxplot(adm$Chance.of.Admit)

#treatment of outlier for LOR with winzorizing technique
summary(adm$LOR)
lower<- 3 - 1.5 * IQR(adm$LOR)
lower
adm$LOR[adm$LOR < lower]<- lower #replacement of outlier
boxplot(adm$LOR)
summary(adm$LOR)

#treatment of outlier for chance of admit with winzorizing technique
summary(adm$Chance.of.Admit)
lower<- 0.63 - 1.5 * IQR(adm$Chance.of.Admit)
lower
adm$Chance.of.Admit[adm$Chance.of.Admit < lower] <- lower
boxplot(adm$Chance.of.Admit)
summary(adm$Chance.of.Admit)

adm2<- adm #created backup

#data partition by 70:30
library(caret)
Train <- createDataPartition(adm$Chance.of.Admit, p=0.70, list = FALSE)
#list means column, when it is false we break data rowwise
training <- adm[Train, ]
testing <- adm[-Train, ]

cor(training)
model1<- lm(Chance.of.Admit~., data = training)
summary(model1)
hist(training$Chance.of.Admit)
model2 <- lm(Chance.of.Admit~. -University.Rating -SOP, data = training)
summary(model2)
par(mfrow=c(2,2))
plot(model2)
library(lmtest)
dwtest(model2)
library(car)
ncvTest(model2)

testing$predict <- predict(model2,testing)
