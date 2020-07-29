data<-read.csv(file.choose())
names(data)
str(data)
#Attrition - Yes->1 No->0
#BusinessTravel - Non-Travel->0 Travel_Rarely->1 Travel_Frequently->2

#Converting int and factor to num
data$Attrition_Flag<-ifelse(data$Attrition=='Yes',1,0)
data$Age<-as.numeric(data$Age)
for (i in 1:length(data$BusinessTravel)) {
  if(data$BusinessTravel[i]=='Non-Travel'){
    data$BusinessTravel_Flag[i] <- 0
    
  } 
  else if (data$BusinessTravel[i]=="Travel_Rarely"){
    data$BusinessTravel_Flag[i] <- 1
    
  } 
  else if (data$BusinessTravel[i]=="Travel_Frequently"){
    data$BusinessTravel_Flag[i] <- 2
    
  }
}



data$DailyRate<-as.numeric(data$DailyRate)
for (i in 1:length(data$Department)) {
  if(data$Department[i]=='Human Resources'){
    data$Department_Flag[i] <- 0
    
  } 
  else if (data$Department[i]=="Research & Development"){
    data$Department_Flag[i] <- 1
    
  } 
  else if (data$Department[i]=="Sales"){
    data$Department_Flag[i] <- 2
    
  }
}
data$DistanceFromHome<-as.numeric(data$DistanceFromHome)
data$Education<-as.numeric(data$Education)
for (i in 1:length(data$EducationField)) {
  if(data$EducationField[i]=='Human Resources'){
    data$EducationField_Flag[i] <- 0
    
  } 
  else if (data$EducationField[i]=="Life Sciences"){
    data$EducationField_Flag[i] <- 1
    
  } 
  else if (data$EducationField[i]=="Marketing"){
    data$EducationField_Flag[i] <- 2
    
  }
  
  else if (data$EducationField[i]=="Medical"){
    data$EducationField_Flag[i] <- 3
    
  }
  
  else if (data$EducationField[i]=="Other"){
    data$EducationField_Flag[i] <- 4
    
  }

  else if (data$EducationField[i]=="Technical Degree"){
    data$EducationField_Flag[i] <- 5
    
  }
}
data$EmployeeCount<-as.numeric(data$EmployeeCount)
data$EmployeeNumber<-as.numeric(data$EmployeeNumber)
data$EnvironmentSatisfaction<-as.numeric(data$EnvironmentSatisfaction)
data$Gender<-ifelse(data$Gender=='Female',1,0)
data$HourlyRate<-as.numeric(data$HourlyRate)
data$JobInvolvement<-as.numeric(data$JobInvolvement)
data$JobLevel<-as.numeric(data$JobLevel)
for (i in 1:length(data$JobRole)) {
  if(data$JobRole[i]=='Healthcare Representative'){
    data$JobRole_Flag[i] <- 0
    
  } 
  else if (data$JobRole[i]=="Human Resources"){
    data$JobRole_Flag[i] <- 1
    
  } 
  else if (data$JobRole[i]=="Laboratory Technician"){
    data$JobRole_Flag[i] <- 2
    
  }
  
  else if (data$JobRole[i]=="Manager"){
    data$JobRole_Flag[i] <- 3
    
  }
  
  else if (data$JobRole[i]=="Manufacturing Director"){
    data$JobRole_Flag[i] <- 4
    
  }
  
  else if (data$JobRole[i]=="Research Director"){
    data$JobRole_Flag[i] <- 5
    
  }
  
  else if (data$JobRole[i]=="Research Scientist"){
    data$JobRole_Flag[i] <- 6
    
  }
  else if (data$JobRole[i]=="Sales Executive"){
    data$JobRole_Flag[i] <- 7
    
  }
  else if (data$JobRole[i]=="Sales Representative"){
    data$JobRole_Flag[i] <- 8
    
  }
}
data$JobSatisfaction<-as.numeric(data$JobSatisfaction)
for (i in 1:length(data$MaritalStatus)) {
  if(data$MaritalStatus[i]=='Divorced'){
    data$MaritalStatus_Flag[i] <- 0
    
  } 
  else if (data$MaritalStatus[i]=="Married"){
    data$MaritalStatus_Flag[i] <- 1
    
  } 
  else if (data$MaritalStatus[i]=="Single"){
    data$MaritalStatus_Flag[i] <- 2
    
  }
}
data$MonthlyIncome<-as.numeric(data$MonthlyIncome)
data$MonthlyRate<-as.numeric(data$MonthlyRate)
data$NumCompaniesWorked<-as.numeric(data$NumCompaniesWorked)
data$Over18<-ifelse(data$Over18=='Y',1,0)
data$OverTime<-ifelse(data$OverTime=='Yes',1,0)
data$PercentSalaryHike<-as.numeric(data$PercentSalaryHike)
data$PerformanceRating<-as.numeric(data$PerformanceRating)
data$RelationshipSatisfaction<-as.numeric(data$RelationshipSatisfaction)
data$StandardHours<-as.numeric(data$StandardHours)
data$StockOptionLevel<-as.numeric(data$StockOptionLevel)
data$TotalWorkingYears<-as.numeric(data$TotalWorkingYears)
data$TrainingTimesLastYear<-as.numeric(data$TrainingTimesLastYear)
data$WorkLifeBalance<-as.numeric(data$WorkLifeBalance)
data$YearsAtCompany<-as.numeric(data$YearsAtCompany)
data$YearsInCurrentRole<-as.numeric(data$YearsInCurrentRole)
data$YearsSinceLastPromotion<-as.numeric(data$YearsSinceLastPromotion)
data$YearsWithCurrManager<-as.numeric(data$YearsWithCurrManager)
str(data)
View(data)





#To check if there are any missing values
summary(data)
sapply(data, function(x) sum(is.na(x)))


#To check if there are ny outliers
boxplot(data$Age)
boxplot(data$DailyRate)
boxplot(data$DistanceFromHome)
boxplot(data$Education)
boxplot(data$EmployeeCount)
boxplot(data$EmployeeNumber)
boxplot(data$EnvironmentSatisfaction)
boxplot(data$HourlyRate)
boxplot(data$JobInvolvement)
boxplot(data$JobLevel)
boxplot(data$JobSatisfaction)
boxplot(data$PercentSalaryHike)
boxplot(data$RelationshipSatisfaction)
boxplot(data$StandardHours)
boxplot(data$WorkLifeBalance)
boxplot(data$MonthlyRate)

#Treatment for outliers
boxplot(data$PerformanceRating)
summary(data$PerformanceRating)
upper<-3.00+1.5*IQR(data$PerformanceRating);upper
data$PerformanceRating[data$PerformanceRating>upper]<-upper
boxplot(data$PerformanceRating)
summary(data$PerformanceRating)



boxplot(data$MonthlyIncome)
summary(data$MonthlyIncome)
upper<-8379+1.5*IQR(data$MonthlyIncome);upper
data$MonthlyIncome[data$MonthlyIncome>upper]<-upper
boxplot(data$MonthlyIncome)
summary(data$MonthlyIncome)


boxplot(data$StockOptionLevel)
summary(data$StockOptionLevel)
upper<-1.00+1.5*IQR(data$StockOptionLevel);upper
data$StockOptionLevel[data$StockOptionLevel>upper]<-upper
boxplot(data$StockOptionLevel)
summary(data$StockOptionLevel)




boxplot(data$TotalWorkingYears)
summary(data$TotalWorkingYears)
upper<-15.00+1.5*IQR(data$TotalWorkingYears);upper
data$TotalWorkingYears[data$TotalWorkingYears>upper]<-upper
boxplot(data$TotalWorkingYears)
summary(data$TotalWorkingYears)



boxplot(data$TrainingTimesLastYear)
summary(data$TrainingTimesLastYear)
upper<-3.00+1.5*IQR(data$TrainingTimesLastYear);upper
data$TrainingTimesLastYear[data$TrainingTimesLastYear>upper]<-upper
lower<-2.00-1.5*IQR(data$TrainingTimesLastYear);lower
data$TrainingTimesLastYear[data$TrainingTimesLastYear<lower]<-lower
boxplot(data$TrainingTimesLastYear)
summary(data$TrainingTimesLastYear)




boxplot(data$YearsAtCompany)
summary(data$YearsAtCompany)
upper<-9.00+1.5*IQR(data$YearsAtCompany);upper
data$YearsAtCompany[data$YearsAtCompany>upper]<-upper
boxplot(data$YearsAtCompany)
summary(data$YearsAtCompany)





boxplot(data$YearsInCurrentRole)
summary(data$YearsInCurrentRole)
upper<-7.00+1.5*IQR(data$YearsInCurrentRole);upper
data$YearsInCurrentRole[data$YearsInCurrentRole>upper]<-upper
boxplot(data$YearsInCurrentRole)
summary(data$YearsInCurrentRole)





boxplot(data$YearsSinceLastPromotion)
summary(data$YearsSinceLastPromotion)
upper<-3.00+1.5*IQR(data$YearsSinceLastPromotion);upper
data$YearsSinceLastPromotion[data$YearsSinceLastPromotion>upper]<-upper
boxplot(data$YearsSinceLastPromotion)
summary(data$YearsSinceLastPromotion)






boxplot(data$YearsWithCurrManager)
summary(data$YearsWithCurrManager)
upper<-7.00+1.5*IQR(data$YearsWithCurrManager);upper
data$YearsWithCurrManager[data$YearsWithCurrManager>upper]<-upper
boxplot(data$YearsWithCurrManager)
summary(data$YearsWithCurrManager)




boxplot(data$NumCompaniesWorked)
summary(data$NumCompaniesWorked)
upper<-4.00+1.5*IQR(data$NumCompaniesWorked);upper
data$NumCompaniesWorked[data$NumCompaniesWorked>upper]<-upper
boxplot(data$NumCompaniesWorked)
summary(data$NumCompaniesWorked)

data$EmployeeCount<-NULL
data$EmployeeNumber<-NULL
data$Over18<-NULL
data$StockOptionLevel<-NULL
data$YearsWithCurrManager<-NULL
data$Attrition<-NULL
data$BusinessTravel<-NULL
data$Department<-NULL
data$EducationField<-NULL
data$JobRole<-NULL
data$MaritalStatus<-NULL
data$PerformanceRating<-NULL
data$StandardHours<-NULL
#data$DailyRate<-NULL
#data$TrainingTimesLastYear<-NULL

View(data)

library(caret)
Train<-createDataPartition(data$Attrition_Flag,p=0.7,list=FALSE)
training<-data[Train,]
testing<-data[-Train,]





#model Building
model = glm(Attrition_Flag~.,family = 'binomial',data=training)
summary(model)


#Variable significance selection
reg.model = step(glm(Attrition_Flag~.,family='binomial',data=training ), direction="both")
summary(reg.model)
anova(reg.model,test='Chisq')
Acc(reg.model)


library(car)
vif(reg.model) #if value is lt 5 means there is no inter co relation

library(oddsratio)
library(vcd)
exp(coef(reg.model))


#Prediction
testing$probs<-predict(reg.model,testing,type='response')
testing$Predict<-as.factor(ifelse(testing$probs>0.70,1,0))
#confusionMatrix(data$Attrition_Flag,testing$Predict)
table(testing$Attrition_Flag,testing$Predict)
library(caret)
#the count of 00 and 11 should match the total count 
#confusionMatrix(testing$Attrition_Flag,testing$Predict)



library(ROCR)
library(ggplot2)
#Make prediction on test set
predictTrain=predict(reg.model,testing,type="response")
#Prediction function
ROCRpred = prediction(predictTrain,testing$Attrition_Flag)
#Performance function
ROCRpref=performance(ROCRpred,"tpr","fpr")

#ploting ROCR curve
plot(ROCRpref)
#Add colors
plot(ROCRpref,colorize=TRUE)

#AUC
pred=prediction(testing$probs,testing$Attrition_Flag)
as.numeric(performance(pred,"auc")@y.values)

summary(model)


#model building may lead to overfitting
#cor(training)
#model<-lm(Attrition_Flag~.,data=training)
#summary(model)

#model 2
#hist(training$Attrition_Flag)
#transformation to have bell shaped curve
#hist((1/training$Attrition_Flag))
#hist(log(training$Attrition_Flag))
#model2<-step(lm(log(Attrition_Flag)~.,data=training),direction="backward")
#summary(model2)


