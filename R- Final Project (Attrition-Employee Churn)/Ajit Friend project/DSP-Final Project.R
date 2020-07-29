FProject=read.csv(file.choose())
View(FProject)
names(FProject)
str(FProject)


#Removing Unique Values Variables:
FProject$EmployeeCount=NULL
FProject$EmployeeNumber=NULL
FProject$Over18=NULL
FProject$StandardHours=NULL




#Converting into factors:
FProject$Education=as.factor(FProject$Education)
FProject$EnvironmentSatisfaction=as.factor(FProject$EnvironmentSatisfaction)
FProject$JobInvolvement=as.factor(FProject$JobInvolvement)
FProject$JobSatisfaction=as.factor(FProject$JobSatisfaction)
FProject$PerformanceRating=as.factor(FProject$PerformanceRating)
FProject$RelationshipSatisfaction=as.factor(FProject$RelationshipSatisfaction)
FProject$WorkLifeBalance=as.factor(FProject$WorkLifeBalance)
FProject$JobLevel=as.factor(FProject$JobLevel)
FProject$StockOptionLevel=as.factor(FProject$StockOptionLevel)
FProject$TrainingTimesLastYear=as.factor(FProject$TrainingTimesLastYear)
#How many times person took trainings

#New Structure:
str(FProject)

#EDA:
summary(FProject)
#Checking for Missing Valaues:
sapply(FProject, function(x) sum(is.na(x)))
#No missing Value in Data.

#checkin fo outliers:
boxplot(FProject)

boxplot(FProject$Age)
boxplot(FProject$DailyRate)
boxplot(FProject$DistanceFromHome)
boxplot(FProject$HourlyRate)

#Outliear for MonthlyIncome:
boxplot(FProject$MonthlyIncome)
#treating for Income:
summary(FProject$MonthlyIncome)
upper=8379+1.5*IQR(FProject$MonthlyIncome)
FProject$MonthlyIncome[FProject$MonthlyIncome>upper]=upper
#Treated:
boxplot(FProject$MonthlyIncome)
boxplot(FProject$MonthlyRate)

#outlier in NumCpaniesWorked:
boxplot(FProject$NumCompaniesWorked)
summary(FProject$NumCompaniesWorked)
upper=6 #there was one outlier which was 9 companies but we are taking manually 6.
FProject$NumCompaniesWorked[FProject$NumCompaniesWorked>upper]=upper
boxplot(FProject$NumCompaniesWorked)

boxplot(FProject$PercentSalaryHike)

#Outlier in TotalWorkingYears:
boxplot(FProject$TotalWorkingYears)
summary(FProject$TotalWorkingYears)
upper=15+1.5*IQR(FProject$TotalWorkingYears)
FProject$TotalWorkingYears[FProject$TotalWorkingYears>upper]=upper
boxplot(FProject$TotalWorkingYears)


#Treatment of YearsAtCompany:
boxplot(FProject$YearsAtCompany)
summary(FProject$YearsAtCompany)
upper=9+1.5*IQR(FProject$YearsAtCompany)
FProject$YearsAtCompany[FProject$YearsAtCompany>upper]=upper
boxplot(FProject$YearsAtCompany)

#Treatment for YearsInCurrentRole:
boxplot(FProject$YearsInCurrentRole)
summary(FProject$YearsInCurrentRole)
FProject$YearsInCurrentRole[FProject$YearsInCurrentRole>12]=mean(FProject$YearsInCurrentRole)
boxplot(FProject$YearsInCurrentRole)

#Treatment for YearsSinceLastPromotion:
boxplot(FProject$YearsSinceLastPromotion)
summary(FProject$YearsSinceLastPromotion)
upper=3+1.5*IQR(FProject$YearsSinceLastPromotion)
FProject$YearsSinceLastPromotion[FProject$YearsSinceLastPromotion>upper]=upper
boxplot(FProject$YearsSinceLastPromotion)

#Treatment for YearsWithCurrManager:
boxplot(FProject$YearsWithCurrManager)
summary(FProject$YearsWithCurrManager)
upper=7+1.5*IQR(FProject$YearsWithCurrManager)
FProject$YearsWithCurrManager[FProject$YearsWithCurrManager>upper]=upper
boxplot(FProject$YearsWithCurrManager)


#ALL TREATED|Final Boxplot:
boxplot(FProject)




#Attrition rate for ATTRITION- YES & ATTRITION- NO:
#for YES:
table(FProject$Attrition)
237/1470*100
          #16.122%



#In a classification problem a stadard baseline method is to
#predict the the chance of ATTRITION= YES
#building model:


#Data Partition:
library(caret)
Train=createDataPartition(FProject$Attrition, p=0.75, list=FALSE)
Training=FProject[Train,]
Testing=FProject[-Train,]


# MODEL BUILDING :

#Logistic model without Relavelling|References and VSM:
TrModel=glm(Attrition~., data=Training, family = 'binomial')

summary(TrModel)


#Applying VSM and refrences:
table(FProject$BusinessTravel)
table(FProject$Department)
table(FProject$Education)
table(FProject$EducationField)
table(FProject$EnvironmentSatisfaction)
table(FProject$Gender)
table(FProject$JobInvolvement)
table(FProject$JobLevel)
table(FProject$JobRole)
table(FProject$JobSatisfaction)
table(FProject$MaritalStatus)
table(FProject$OverTime)
table(FProject$PerformanceRating)
table(FProject$RelationshipSatisfaction)
table(FProject$WorkLifeBalance)
table(FProject$TrainingTimesLastYear)

attach(Training)
detach(Training)



#for Performace rating variable we have only 3 and 4 in data but in R 3 is coded 
#as factor 1 and 4 is coded as vector 2 so for selecting 4 as reference we will 
#give 2 as reference...;

#Logistic Model:
TrModel1=step(glm(Attrition~Age+DailyRate+DistanceFromHome+HourlyRate+MonthlyIncome+MonthlyRate+
               NumCompaniesWorked+PercentSalaryHike+StockOptionLevel+TotalWorkingYears+
               YearsAtCompany+YearsInCurrentRole+YearsSinceLastPromotion+YearsWithCurrManager+
               relevel(BusinessTravel,ref='Non-Travel')+
               relevel(Department, ref='Human Resources')+
               relevel(Education,ref=5)+
               relevel(EducationField,ref='Human Resources')+
               relevel(EnvironmentSatisfaction,ref=1)+
               relevel(Gender,ref='Female')+
               relevel(JobInvolvement,ref=1)+
               relevel(JobLevel,ref=5)+
               relevel(JobRole,ref='Human Resources')+
               relevel(JobSatisfaction,ref=2)+
               relevel(MaritalStatus,ref='Divorced')+
               relevel(OverTime,ref='Yes')+
               relevel(PerformanceRating,ref=2)+
               relevel(RelationshipSatisfaction,ref=1)+
               relevel(TrainingTimesLastYear,ref='0')+
               relevel(WorkLifeBalance,ref=1), data = Training, family = 'binomial' ), direction = 'both')

summary(TrModel1)

Acc(TrModel1)

#We are Getting 89.828----- i.e. 90% accuracy for training data...

#VIF|Checking for Multicollinearity:
library(car)
vif(TrModel1)

#Multicollinearity in JobRole= 88.9705.....so will neglect the variable:

#Logistic Model:
TrModel2=step(glm(Attrition~Age+DailyRate+DistanceFromHome+HourlyRate+MonthlyIncome+MonthlyRate+
                    NumCompaniesWorked+PercentSalaryHike+StockOptionLevel+TotalWorkingYears+
                    YearsAtCompany+YearsInCurrentRole+YearsSinceLastPromotion+YearsWithCurrManager+
                    relevel(BusinessTravel,ref='Non-Travel')+
                    relevel(Department, ref='Human Resources')+
                    relevel(Education,ref=5)+
                    relevel(EducationField,ref='Human Resources')+
                    relevel(EnvironmentSatisfaction,ref=1)+
                    relevel(Gender,ref='Female')+
                    relevel(JobInvolvement,ref=1)+
                    relevel(JobLevel,ref=5)-
                    relevel(JobRole,ref='Human Resources')+
                    relevel(JobSatisfaction,ref=2)+
                    relevel(MaritalStatus,ref='Divorced')+
                    relevel(OverTime,ref='Yes')+
                    relevel(PerformanceRating,ref=2)+
                    relevel(RelationshipSatisfaction,ref=1)+
                    relevel(TrainingTimesLastYear,ref='0')+
                    relevel(WorkLifeBalance,ref=1), data = Training, family = 'binomial' ), direction = 'both')

summary(TrModel2)
Acc(TrModel2)
#VIF:
vif(TrModel2)

#Still in JObLevel= 21.74....neglecting this variable:

#Logistic Model:
TrModel3=step(glm(Attrition~Age+DailyRate+DistanceFromHome+HourlyRate+MonthlyIncome+MonthlyRate+
                    NumCompaniesWorked+PercentSalaryHike+StockOptionLevel+TotalWorkingYears+
                    YearsAtCompany+YearsInCurrentRole+YearsSinceLastPromotion+YearsWithCurrManager+
                    relevel(BusinessTravel,ref='Non-Travel')+
                    relevel(Department, ref='Human Resources')+
                    relevel(Education,ref=5)+
                    relevel(EducationField,ref='Human Resources')+
                    relevel(EnvironmentSatisfaction,ref=1)+
                    relevel(Gender,ref='Female')+
                    relevel(JobInvolvement,ref=1)-
                    relevel(JobLevel,ref=5)-
                    relevel(JobRole,ref='Human Resources')+
                    relevel(JobSatisfaction,ref=2)+
                    relevel(MaritalStatus,ref='Divorced')+
                    relevel(OverTime,ref='Yes')+
                    relevel(PerformanceRating,ref=2)+
                    relevel(RelationshipSatisfaction,ref=1)+
                    relevel(TrainingTimesLastYear,ref='0')+
                    relevel(WorkLifeBalance,ref=1), data = Training, family = 'binomial' ), direction = 'both')

summary(TrModel3)
#VIF:
vif(TrModel3)

#No more VIF greater than 5...so will check final Accuracy of our Training Model:

Acc(TrModel3)


#Still in YearsAtCompany= 6.0654....neglecting this variable:

#Logistic Model:
TrModel4=step(glm(Attrition~Age+DailyRate+DistanceFromHome+HourlyRate+MonthlyIncome+MonthlyRate+
                    NumCompaniesWorked+PercentSalaryHike+StockOptionLevel+TotalWorkingYears-
                    YearsAtCompany+YearsInCurrentRole+YearsSinceLastPromotion+YearsWithCurrManager+
                    relevel(BusinessTravel,ref='Non-Travel')+
                    relevel(Department, ref='Human Resources')+
                    relevel(Education,ref=5)+
                    relevel(EducationField,ref='Human Resources')+
                    relevel(EnvironmentSatisfaction,ref=1)+
                    relevel(Gender,ref='Female')+
                    relevel(JobInvolvement,ref=1)-
                    relevel(JobLevel,ref=5)-
                    relevel(JobRole,ref='Human Resources')+
                    relevel(JobSatisfaction,ref=2)+
                    relevel(MaritalStatus,ref='Divorced')+
                    relevel(OverTime,ref='Yes')+
                    relevel(PerformanceRating,ref=2)+
                    relevel(RelationshipSatisfaction,ref=1)+
                    relevel(TrainingTimesLastYear,ref='0')+
                    relevel(WorkLifeBalance,ref=1), data = Training, family = 'binomial' ), direction = 'both')

summary(TrModel4)
#VIF:
vif(TrModel4)

#No more VIF greater than 5...so will check final Accuracy of our Training Model:

Acc(TrModel4)



#NOW:
##TO get the odds ratio:
library(oddsratio)
library(vcd)
exp(coef(TrModel4))

#NOW we will Test our model on Testing DATASET:

#Prediction:
Testing$probs= predict(TrModel4, Testing, type='response')

#Threshold=(0.5,0.6,0.7,0.75)
Testing$Predict= as.factor(ifelse(Testing$probs>0.75, 'Yes','No'))

table(Testing$Attrition, Testing$Predict)

library(caret)
confusionMatrix(Testing$Attrition, Testing$Predict)

#From the confusion matrix we can see the model accuracy is 87% with
#with Low sensitivity and high Specificity which is good.

##So we can interpret our model has 87% predictive ability.


#Now, to make R O C curve:
library(ROCR)
predictTrain= predict(TrModel4, Testing, type= 'response')

#Prediction function:
ROCRpred= prediction(predictTrain, Testing$Attrition)

#Performance function:
ROCRpref= performance(ROCRpred, 'tpr', 'fpr')


#Plotting ROC curve:
plot(ROCRpref)

#Add Colors:
plot(ROCRpref, colorize= TRUE)


#OR WE CAN USE:
library(pROC)
plot(roc(Testing$Attrition,Testing$probs, direction="<"),
     col="Red", lwd=2.5, main="WAY OF LINE")



##AUC:
pred= prediction(Testing$probs, Testing$Attrition)
as.numeric(performance(pred, 'auc')@y.values)  #to fetch all auc for all values of y.

#Which means 77% area cuvered by the model for predicting.... 


#OR WE CAN SEE IN ROC CURVE ONLY:
plot(roc(Testing$Attrition,Testing$probs), col='red', print.auc=TRUE)






#AIC:

#AIC is an estimate of a constant plus the relative distance 
#between the unknown true likelihood function of the data and 
#the fitted likelihood function of the model, so that a lower 
#AIC means a model is considered to be closer to the truth.

