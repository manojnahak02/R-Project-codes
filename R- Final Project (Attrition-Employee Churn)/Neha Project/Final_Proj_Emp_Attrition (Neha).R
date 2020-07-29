#Objective :- Predicting Employee Attrition

data<-read.csv(file.choose())
names(data)
str(data)
summary(data)

#Attrition - Yes->1 No->0

#Converting int and factor to num

data$Attrition<-as.factor(ifelse(data$Attrition=='Yes',1,0))

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

data$BusinessTravel<-as.factor(data$BusinessTravel_Flag)
data$BusinessTravel_Flag<-NULL

#data$BusinessTravel<-(lapply(data$BusinessTravel,function(x)(as.factor(x))))
#data$BusinessTravel<-do.call(rbind.data.frame, data$BusinessTravel)
#lapply() converted all the values of business_travel cols into factor (1,2,3) and it assigned number based on alphabetical sort

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
data$Department<-as.factor(data$Department_Flag)
data$Department_Flag<-NULL

data$DistanceFromHome<-as.numeric(data$DistanceFromHome)

data$Education<-as.factor(data$Education)#

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
data$EducationField<-as.factor(data$EducationField_Flag)
data$EducationField_Flag<-NULL

data$EmployeeCount<-as.numeric(data$EmployeeCount)

data$EmployeeNumber<-as.numeric(data$EmployeeNumber)

data$EnvironmentSatisfaction<-as.factor(data$EnvironmentSatisfaction)#

data$Gender<-as.factor(ifelse(data$Gender=='Female',1,0))

data$HourlyRate<-as.numeric(data$HourlyRate)

data$JobInvolvement<-as.factor(data$JobInvolvement)#

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
data$JobRole<-as.factor(data$JobRole_Flag)
data$JobRole_Flag<-NULL

data$JobSatisfaction<-as.factor(data$JobSatisfaction)#

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
data$MaritalStatus<-as.factor(data$MaritalStatus_Flag)
data$MaritalStatus_Flag<-NULL

data$MonthlyIncome<-as.numeric(data$MonthlyIncome)

data$MonthlyRate<-as.numeric(data$MonthlyRate)

data$NumCompaniesWorked<-as.numeric(data$NumCompaniesWorked)

data$Over18<-as.factor(ifelse(data$Over18=='Y',1,0))

data$OverTime<-as.factor(ifelse(data$OverTime=='Yes',1,0))

data$PercentSalaryHike<-as.numeric(data$PercentSalaryHike)

data$PerformanceRating<-as.factor(data$PerformanceRating)#

data$RelationshipSatisfaction<-as.factor(data$RelationshipSatisfaction)#

data$StandardHours<-as.numeric(data$StandardHours)

data$StockOptionLevel<-as.numeric(data$StockOptionLevel)

data$TotalWorkingYears<-as.numeric(data$TotalWorkingYears)

data$TrainingTimesLastYear<-as.numeric(data$TrainingTimesLastYear)

data$WorkLifeBalance<-as.factor(data$WorkLifeBalance)#

data$YearsAtCompany<-as.numeric(data$YearsAtCompany)

data$YearsInCurrentRole<-as.numeric(data$YearsInCurrentRole)

data$YearsSinceLastPromotion<-as.numeric(data$YearsSinceLastPromotion)

data$YearsWithCurrManager<-as.numeric(data$YearsWithCurrManager)

str(data)
View(data)

#To check if there are any missing values
summary(data)
sapply(data, function(x) sum(is.na(x))) #no missing values

#manually removing the cols(manual variable selection)
data$StandardHours<-NULL
data$MonthlyRate<-NULL
data$HourlyRate<-NULL
data$JobLevel<-NULL
data$EmployeeCount<-NULL
data$DailyRate<-NULL
data$NumCompaniesWorked<-NULL
data$Over18<-NULL
data$StockOptionLevel<-NULL
data$YearsWithCurrManager<-NULL

names(data)


#To check if there are any outliers
lapply (data, class)#getting list of all cols with its datatype

#wrote these two lines bcz Age box plot was getting error of margin
par("mar")#checked the margin , should be =5.1 4.1 4.1 2.1
par(mar=c(1,1,1,1)) #change the margin

boxplot(data$Age)
boxplot(data$DistanceFromHome)
#boxplot(data$Education)
boxplot(data$EmployeeNumber)
boxplot(data$MonthlyIncome)
boxplot(data$PercentSalaryHike)#+ no need to treat
boxplot(data$TotalWorkingYears)#+no need to treat
boxplot(data$TrainingTimesLastYear)#-+no need to treat
boxplot(data$YearsAtCompany)#+no need to treat
boxplot(data$YearsInCurrentRole)#+no need to treat
boxplot(data$YearsSinceLastPromotion)#+no need to treat

#data partition
library(caret)
library(tree)
split <- createDataPartition(y =data$Attrition , p = 0.7,list = FALSE)
train <- data[split,]
test <- data[-split,]

##buildingmodel
tree.data = tree(Attrition~. , data = train)
plot(tree.data)
text(tree.data,pretty =0)
tree.data ## big tree = high complexity tree

tree.pred = predict(tree.data ,test,type = "class")#type = 'Class' will gv ans in terms of 0 or 1
#decision tree gives output in terms of 0 & 1 (when y is categorical)
table(tree.pred,test$Attrition)
library(caret)
confusionMatrix(tree.pred , test$Attrition)

#We perform pruning in the below step to reduce treee size and keeping accuracy greater than wt we got above(ie 81.59%)

cv.data = cv.tree(tree.data , FUN=prune.misclass)#y is categorical hence using  FUN=prune.misclass
names(cv.data)
plot(cv.data$size,cv.data$dev,type ="b",xlab='x-axis',ylab ='yaxis')

prune.data =prune.misclass(tree.data,best = 10)#when y is regression we write prune.tree()
#when y is ctegorical then we write prune.misclass()
plot(prune.data)
text(prune.data , pretty =0)

#prediction
tree.pred = predict(prune.data,test, type="class")
table(tree.pred,test$Attrition)
confusionMatrix(tree.pred ,test$Attrition)





