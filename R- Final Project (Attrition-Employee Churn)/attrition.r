####Objective:
#To predict which employee is likely to churn

# Data definitions for categorical variables: 
# Education 1 'Below College' 2 'College' 3 'Bachelor' 4 'Master' 5 'Doctor'
# EnvironmentSatisfaction 1 'Low' 2 'Medium' 3 'High' 4 'Very High'
# JobInvolvement 1 'Low' 2 'Medium' 3 'High' 4 'Very High'
# JobSatisfaction 1 'Low' 2 'Medium' 3 'High' 4 'Very High'
# PerformanceRating 1 'Low' 2 'Good' 3 'Excellent' 4 'Outstanding'
# RelationshipSatisfaction 1 'Low' 2 'Medium' 3 'High' 4 'Very High'
# WorkLifeBalance 1 'Bad' 2 'Good' 3 'Better' 4 'Best'

Employees <- read.csv(file = "E:\\Imarticus DSP 24\\Final Projects\\R\\Attrition.csv")
View(Employees)
str(Employees)
names(Employees)

#step 1 converting Y variable into Factor
Employees$Attrition<- as.factor(ifelse(Employees$Attrition=="Yes",1,0))
str(Employees)

##STEP-2
#Converting X variables from int to catgorical or Factor variable
str(Employees)
Employees$Gender<- as.factor(ifelse(Employees$Gender=="Male",1,0))
Employees$Education=as.factor(Employees$Education)
Employees$EnvironmentSatisfaction=as.factor(Employees$EnvironmentSatisfaction)
Employees$JobInvolvement=as.factor(Employees$JobInvolvement)
Employees$JobSatisfaction=as.factor(Employees$JobSatisfaction)
Employees$PerformanceRating=as.factor(Employees$PerformanceRating)
Employees$RelationshipSatisfaction=as.factor(Employees$RelationshipSatisfaction)
Employees$WorkLifeBalance=as.factor(Employees$WorkLifeBalance)
Employees$Department=as.factor(Employees$Department)
Employees$EducationField=as.factor(Employees$EducationField)
Employees$MaritalStatus=as.factor(Employees$MaritalStatus)


#step-3
#Dropping the variables
Employees$EmployeeCount<-NULL
#Employees$MonthlyRate<-NULL
Employees$Over18<-NULL
Employees$StandardHours<-NULL
#Employees$TotalWorkingYears<-NULL

#missing value and univariate analysis
summary(Employees)
sapply(Employees, function(x) sum(is.na(x)))
#or 
sum(is.na(Employees))

##STEP-4
#Boxplot for identifying outlier
boxplot(Employees$TotalWorkingYears)
boxplot(Employees$MonthlyRate)
boxplot(Employees$Age)
boxplot(Employees$DailyRate)
boxplot(Employees$DistanceFromHome)
boxplot(Employees$EmployeeNumber)
boxplot(Employees$HourlyRate)
boxplot(Employees$JobLevel)
boxplot(Employees$MonthlyIncome)
boxplot(Employees$NumCompaniesWorked)
boxplot(Employees$PercentSalaryHike)
boxplot(Employees$StockOptionLevel)
boxplot(Employees$TrainingTimesLastYear)
boxplot(Employees$YearsAtCompany)
boxplot(Employees$YearsInCurrentRole)
boxplot(Employees$YearsSinceLastPromotion)
boxplot(Employees$YearsWithCurrManager)

#teatment of outlier
boxplot(Employees$TotalWorkingYears)
summary(Employees$TotalWorkingYears)
upper<-15+1.5*IQR(Employees$TotalWorkingYears)
Employees$TotalWorkingYears[Employees$TotalWorkingYears>upper]<-upper
boxplot(Employees$TotalWorkingYears)

boxplot(Employees$MonthlyIncome)
summary(Employees$MonthlyIncome)
upper<-8379+1.5*IQR(Employees$MonthlyIncome);upper
Employees$MonthlyIncome[Employees$MonthlyIncome>upper]<-upper
boxplot(Employees$MonthlyIncome)

boxplot(Employees$NumCompaniesWorked)
summary(Employees$NumCompaniesWorked)
upper<-4+1.5*IQR(Employees$NumCompaniesWorked);upper
Employees$NumCompaniesWorked[Employees$NumCompaniesWorked>upper]<-upper
boxplot(Employees$NumCompaniesWorked)

boxplot(Employees$StockOptionLevel)
summary(Employees$StockOptionLevel)
upper<-1+1.5*IQR(Employees$StockOptionLevel);upper
Employees$StockOptionLevel[Employees$StockOptionLevel>upper]<-upper
boxplot(Employees$StockOptionLevel)

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
##Binary Logistics regression, as y= 0 or 1
#model building
model=glm(Attrition~., family = 'binomial', data = training)
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
#87.99% Concordance

#variable selection method
#It gives the best set of variables for model.
model1=step(glm(Attrition~., family='binomial', data = training), direction="both")
summary(model1)
#The variables removed were eg Department, Edu field, Maritial status
#anova(model1, test='Chisq')
Acc(model1)
##accuracy=87.17%, not much difference

#to check multicollinearity
library(car)
vif(model1)
#VIF values are <5 thus no multicolinearity

##STEP-7
#Finding odds ratio
exp(coef(model1))
#The chances of Males Attrition are 1.5396 times 
#larger than that of Females Attrition.

##STEP-8
##Prediction
testing$probs<-predict(model1, testing, type='response')
testing$Predict<-as.factor(ifelse(testing$probs>0.70,1,0))
table(testing$Attrition, testing$Predict) 
    #Actual
#     0   1
# 0 362   7
# 1  48  23

#Type 1 error 48, Type 2 error 7

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
#84%
library(pROC)
plot(roc(testing$Attrition, testing$probs), col = 'red', print.auc=TRUE)

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
prune.data = prune.misclass (tree.model, best = 12)#misclass is used when y is classification
plot(prune.data)
text(prune.data, pretty = 0)
tree.pred=predict(prune.data, test, type = "class")
table(tree.pred, test$Attrition)
confusionMatrix(tree.pred, test$Attrition)
#Accuracy : 0.8614  

#Changing cost function and check the performance
prune.data1 = prune.misclass (tree.model, best = 10)
plot(prune.data1)
text(prune.data1, pretty = 0)
tree.pred1=predict(prune.data1, test, type = "class")
confusionMatrix(tree.pred1, test$Attrition)
#Not much change in the accuracy i.e 86.82. Finalize 9 nodes


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
#Accuracy on Testing Data is 83.86

tune.out<-tune(svm,Attrition~.,data=train1, kernel="linear",
              ranges = list(cost=c(0.001,0.01,0.1,1,5,10,100)))
summary(tune.out)
#best model 
bestmod=tune.out$best.model
summary(bestmod)
#here summary shows that svm have taken Cost=1
#accuracy of model on traning data
confusionMatrix(bestmod$fitted,train1$Attrition)
#Accuracy = 0.8932


#prediction on testing data
ypred=predict(bestmod,test1)
table(predict=ypred,truth=test1$Attrition)

#accuracy of model on test data
confusionMatrix(ypred,test1$Attrition)
#Accuracy = 0.9023
