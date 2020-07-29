#EMPLOYEE ATTRITION

#ATTACHING FILE AND CHECKING STRUCTURE AND FEW ROWS

Employees <- read.csv(file.choose())
View(Employees)
str(Employees)


#Y variable as factor and X variables as factor where required to be converted
Employees$Attrition<-as.factor(ifelse(Employees$Attrition=="Yes",1,0))
Employees$Education<-as.factor(Employees$Education)
Employees$EnvironmentSatisfaction<-as.factor(Employees$EnvironmentSatisfaction)
Employees$JobInvolvement<-as.factor(Employees$JobInvolvement)
Employees$JobLevel<-as.factor(Employees$JobLevel)
Employees$JobSatisfaction<-as.factor(Employees$JobSatisfaction)
Employees$PerformanceRating<-as.factor(Employees$PerformanceRating)
Employees$RelationshipSatisfaction<-as.factor(Employees$RelationshipSatisfaction)
Employees$StockOptionLevel<-as.factor(Employees$StockOptionLevel)
Employees$WorkLifeBalance<-as.factor(Employees$WorkLifeBalance)

str(Employees)


# check missing values
summary(Employees)
sapply(Employees, function(x) sum(is.na(x)))


#check levelwise contribution to identify variables to be dropped
names(Employees)
xtabs(~Employees$Attrition +Employees$Age)
xtabs(~Employees$Attrition +Employees$BusinessTravel)
xtabs(~Employees$Attrition +Employees$DailyRate)
xtabs(~Employees$Attrition +Employees$Department)
xtabs(~Employees$Attrition +Employees$DistanceFromHome)
xtabs(~Employees$Attrition +Employees$Education)
xtabs(~Employees$Attrition +Employees$EducationField)
xtabs(~Employees$Attrition +Employees$EmployeeCount)
xtabs(~Employees$Attrition +Employees$EmployeeNumber)
xtabs(~Employees$Attrition +Employees$EnvironmentSatisfaction)
xtabs(~Employees$Attrition +Employees$Gender)
xtabs(~Employees$Attrition +Employees$HourlyRate)
xtabs(~Employees$Attrition +Employees$JobInvolvement)
xtabs(~Employees$Attrition +Employees$JobLevel)
xtabs(~Employees$Attrition +Employees$JobRole)
xtabs(~Employees$Attrition +Employees$JobSatisfaction)
xtabs(~Employees$Attrition +Employees$MaritalStatus)
xtabs(~Employees$Attrition +Employees$MonthlyIncome)
xtabs(~Employees$Attrition +Employees$MonthlyRate)
xtabs(~Employees$Attrition +Employees$NumCompaniesWorked)
xtabs(~Employees$Attrition +Employees$Over18)
xtabs(~Employees$Attrition +Employees$OverTime)
xtabs(~Employees$Attrition +Employees$PercentSalaryHike)
xtabs(~Employees$Attrition +Employees$PerformanceRating)
xtabs(~Employees$Attrition +Employees$RelationshipSatisfaction)
xtabs(~Employees$Attrition +Employees$StandardHours)
xtabs(~Employees$Attrition +Employees$StockOptionLevel)
xtabs(~Employees$Attrition +Employees$TotalWorkingYears)
xtabs(~Employees$Attrition +Employees$TrainingTimesLastYear)
xtabs(~Employees$Attrition +Employees$WorkLifeBalance)
xtabs(~Employees$Attrition +Employees$YearsAtCompany)
xtabs(~Employees$Attrition +Employees$YearsInCurrentRole)
xtabs(~Employees$Attrition +Employees$YearsSinceLastPromotion)
xtabs(~Employees$Attrition +Employees$YearsWithCurrManager)


#Drop variables that can be evidently not used
Employees$Over18<-NULL
Employees$StandardHours<-NULL
Employees$EmployeeCount<-NULL


#run a tempmodel to identify significant variables
tempmodel<-glm(Attrition~.-EmployeeNumber, family = binomial,data = Employees)
summary(tempmodel)


#Drop insignifcant variables
Employees$DailyRate<-NULL
Employees$Department<-NULL
Employees$Education<-NULL
Employees$EducationField<-NULL
Employees$HourlyRate<-NULL
Employees$JobRole<-NULL
Employees$MaritalStatus<-NULL
#Employees$MonthlyIncome<-NULL
Employees$MonthlyRate<-NULL
Employees$PercentSalaryHike<-NULL
Employees$PerformanceRating<-NULL
Employees$TotalWorkingYears<-NULL

names(Employees)


#Boxplot for identifying outlier
boxplot(Employees$Age)
boxplot(Employees$DistanceFromHome)
boxplot(Employees$NumCompaniesWorked)
boxplot(Employees$TrainingTimesLastYear)
boxplot(Employees$YearsAtCompany)
boxplot(Employees$YearsInCurrentRole)
boxplot(Employees$YearsSinceLastPromotion)
boxplot(Employees$YearsWithCurrManager)

#teatment of outlier
boxplot(Employees$TrainingTimesLastYear)
summary(Employees$TrainingTimesLastYear)
upper<-3+1.5*IQR(Employees$TrainingTimesLastYear);upper
lower<-2-1.5*IQR(Employees$TrainingTimesLastYear);lower
Employees$TrainingTimesLastYear[Employees$TrainingTimesLastYear>upper]<-upper
Employees$TrainingTimesLastYear[Employees$TrainingTimesLastYear<lower]<-lower
boxplot(Employees$TrainingTimesLastYear)
summary(Employees$TrainingTimesLastYear)

boxplot(Employees$YearsAtCompany)
summary(Employees$YearsAtCompany)
upper<-9+1.5*IQR(Employees$YearsAtCompany);upper
Employees$YearsAtCompany[Employees$YearsAtCompany>upper]<-upper
boxplot(Employees$YearsAtCompany)
summary(Employees$YearsAtCompany)

boxplot(Employees$YearsInCurrentRole)
summary(Employees$YearsInCurrentRole)
upper<-7+1.5*IQR(Employees$YearsInCurrentRole);upper
Employees$YearsInCurrentRole[Employees$YearsInCurrentRole>upper]<-upper
boxplot(Employees$YearsInCurrentRole)
summary(Employees$YearsInCurrentRole)

boxplot(Employees$YearsSinceLastPromotion)
summary(Employees$YearsSinceLastPromotion)
upper<-3+1.5*IQR(Employees$YearsSinceLastPromotion);upper
Employees$YearsSinceLastPromotion[Employees$YearsSinceLastPromotion>upper]<-upper
boxplot(Employees$YearsSinceLastPromotion)
summary(Employees$YearsSinceLastPromotion)

boxplot(Employees$YearsWithCurrManager)
summary(Employees$YearsWithCurrManager)
upper<-7+1.5*IQR(Employees$YearsWithCurrManager);upper
Employees$YearsWithCurrManager[Employees$YearsWithCurrManager>upper]<-upper
boxplot(Employees$YearsWithCurrManager)
summary(Employees$YearsWithCurrManager)

##STEP-5
##Data partition
library(caret)
set.seed(123)
Train<-createDataPartition(Employees$Attrition, p=0.7, list=FALSE)
#response variable is 'attrition'
training<-Employees[Train,]
testing<-Employees[-Train,]


##STEP-6
##Multiple Logistics regression
#model building
model=glm(Attrition~.-EmployeeNumber, family = 'binomial', data = training)
summary(model)

##ACC function
Acc=function(model){
  Data = cbind(model$y, model$fitted.values) 
  ones = Data[Data[,1] == 1,]
  zeros = Data[Data[,1] == 0,]
  conc=matrix(0, dim(zeros)[1], dim(ones)[1])
  disc=matrix(0, dim(zeros)[1], dim(ones)[1])
  ties=matrix(0, dim(zeros)[1], dim(ones)[1])
  for (j in 1:dim(zeros)[1])
  {
    for (i in 1:dim(ones)[1])
    {
      if (ones[i,2]>zeros[j,2])
      {conc[j,i]=1}
      else if (ones[i,2]<zeros[j,2])
      {disc[j,i]=1}
      else if (ones[i,2]==zeros[j,2])
      {ties[j,i]=1}
    }
  }
  Pairs=dim(zeros)[1]*dim(ones)[1]
  PercentConcordance=(sum(conc)/Pairs)*100
  PercentDiscordance=(sum(disc)/Pairs)*100
  PercentTied=(sum(ties)/Pairs)*100
  return(list("Percent Concordance"=PercentConcordance,"Percent Discordance"=PercentDiscordance,"Percent Tied"=PercentTied,"Pairs"=Pairs))
}

Acc(model)
##86.86% Concordance

##variable selection method
model1=step(glm(Attrition~.-EmployeeNumber, family='binomial', data = training), direction="both")
summary(model1)
anova(model1, test='Chisq') 
Acc(model1)
##accuracy=86.52%, not much difference

#to check multicollinearity
library(car)
vif(model1)
#VIF values are <5 thus no multicolinearity


##STEP-7
#Finding odds ratio
exp(coef(model1))

##STEP-8
##Prediction
testing$probs<-predict(model1, testing, type='response')
testing$Predict<-as.factor(ifelse(testing$probs>0.70,1,0))
table(testing$Attrition, testing$Predict) 

##STEP-9
#confusion matrix
library(caret)
confusionMatrix(testing$Attrition, testing$Predict)
#Accuracy: 87%
library(ROCR)
library(ggplot2)

##STEP-9
##ROCR curve
##Make predictions on the test data
predictTrain=predict(model1, testing, type="response") 
#Prediction function
ROCRpred=prediction(predictTrain, testing$Attrition)
#Performance function
ROCRperf=performance(ROCRpred, "tpr", "fpr")
##ploting ROC curve
plot(ROCRperf)
#Add colors
plot(ROCRperf, colorize=TRUE)

##STEP-11
#AUC
pred=prediction(testing$probs, testing$Attrition)
as.numeric(performance(pred,"auc")@y.values)
#82.83%

##############################DECISION TREEE#########################

library(tree)
##Data partition
library(caret)
set.seed(123)
Train<-createDataPartition(Employees$Attrition, p=0.7, list=FALSE)
#response variable is 'attrition'
train<-Employees[Train,]
test<-Employees[-Train,]

#Building model
tree.model = tree(Attrition~., data=train)
plot(tree.model)
text(tree.model, pretty=0)

# Hence the data nodes are more, before prediction first we have to prune the data
# tree.pred=predict(tree.model, data=test)
# table(tree.pred, test$Attrition)
# confusionMatrix(tree.pred, test$Attrition)

#Pruning
cv.data= cv.tree(tree.model, FUN=prune.misclass) #FUN is used when y is catagorical
names(cv.data)
plot(cv.data$size, cv.data$dev, type="b")
prune.data = prune.misclass (tree.model, best = 9)#misclass is used when y is classification
plot(prune.data)
text(prune.data, pretty = 0)
tree.pred=predict(prune.data, test, type = "class")
table(tree.pred, test$Attrition)
confusionMatrix(tree.pred, test$Attrition)
#Accuracy : 0.8614  

#Changing cost function and check the performance
prune.data1 = prune.misclass (tree.model, best = 8)
plot(prune.data1)
text(prune.data1, pretty = 0)
tree.pred1=predict(prune.data1, test, type = "class")
confusionMatrix(tree.pred1, test$Attrition)
#No change in the accuracy.


################### SUPPORT VECTOR MACHINE ##########################

#data partition
library(caret)
set.seed(123)
Train<-createDataPartition(Employees$Attrition, p=0.7, list=FALSE)
#response variable is 'attrition'
train1<-Employees[Train,]
test1<-Employees[-Train,]

#Building SVM MODEL without cost
library(e1071)
svm_model <- svm(Attrition~., data=train1, scale=FALSE)
summary(svm_model)
#Number of Support Vectors:  1030
confusionMatrix(svm_model$fitted,train1$Attrition)
#Accuracy = 1 (i.e 100%) overfitter

#building model2 with cost
svm_model2 <- svm(Attrition~., data=train1, cost=0.01, scale = FALSE)
summary(svm_model2)
#Number of Support Vectors:  1030
confusionMatrix(svm_model2$fitted,train1$Attrition)
#Accuracy = 0.8388


#prediction
test1_pred<-predict(svm_model2,test1)
#accuracy of model on test data
confusionMatrix(test1_pred,test1$Attrition)


tune.out<-tune(svm,Attrition~.,data=train1, kernel="linear",
               ranges = list(cost=c(0.001,0.01,0.1,1,5,10,100)))
summary(tune.out)
#best model 
bestmod=tune.out$best.model
summary(bestmod)
#accuracy of model on traning data
confusionMatrix(bestmod$fitted,train1$Attrition)
#Accuracy = 0.8932


#prediction on testing data
ypred=predict(bestmod,test1)
table(predict=ypred,truth=test1$Attrition)

#accuracy of model on test data
confusionMatrix(ypred,test1$Attrition)
#Accuracy = 0.8977
