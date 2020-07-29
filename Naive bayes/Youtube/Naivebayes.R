#Naive bayes classification
#Objective: is to check the probability of a student getting admission based on 
            #his GRE & GPA score and rank of hi universities.
#libraries
library(naivebayes)
library(dplyr)
library(ggplot2)
library(psych)

#Data
data<-read.csv(file = "E:/Imarticus DSP 24/R Programs/Naive bayes/Youtube/binary.csv")
str(data)

#cross Tabulation for admit and Rank
xtabs(~admit+rank, data=data)
data$admit <- as.factor(data$admit)
data$rank <- as.factor(data$rank)

#VISUALIZATION
#In naive bayes we chk that the X variables are not highly co-related.
pairs.panels(data[-1]) #-1 will exclude the Y variable
#for numeric variables the graph will be build to chk normality and corelation.
#corelation between gre and gpa are not strong and
#corelation coeffecient is 0.38.

#Boxplot
#The compound assignment %<>% operator is used to update a value
data %>% ggplot(aes(x=admit, y=gre, fill = admit))+
  geom_boxplot() +
  ggtitle("Box Plot for GRE")
#insight both the plots are overlapping, but overall the avg gre score for 1
# is more that 0

data %>% ggplot(aes(x=admit, y=gpa,fill = admit))+
  geom_boxplot() +
  ggtitle("Box Plot for GPA")

#Density Plot
data %>% ggplot(aes(x=gre, fill=admit))+
  geom_density(alpha=0.8, color='blue')+ #0.8 is for tranperent graph
  ggtitle("Density plot for GRE")
#graph shows that data is overlapping

data %>% ggplot(aes(x=gpa, fill=admit))+
  geom_density(alpha=0.8, color='blue')+ #0.8 is for tranperent graph
  ggtitle("Density plot for GPA")

#Data Partition with replacement 80-20
set.seed(1234)
datapart <- sample(2, nrow(data), replace = T, prob = c(0.8,0.2))
train <- data[datapart  == 1,]
test <- data[datapart == 2,]

#Naive Bayes model
# Bayes theorem+
#   p(a|b) means prob of a given that event b occures

model <- naive_bayes(admit ~ ., data=train)
#probability can be calculated if we have normal distribution curve 
#with mean and standard deviation
model

# output
# rank   0          1
# 1 0.10313901 0.24509804
# 2 0.36771300 0.42156863
# 3 0.33183857 0.24509804
# 4 0.19730942 0.08823529
# prob of stud from Rank1 school getting admit is 0.245

train %>%
  filter(admit=="0")%>%
  summarise(mean(gre), sd(gre))

# Output
#    mean(gre) sd(gre)
# 1  578.6547 116.325

plot(model)

#PREDICT
p <- predict(model, train, type = 'prob') #type of prediction is probability
#now check first few rows
head(cbind(p,train))

#CONFUSION MATRIX for Train data
p1 <- predict(model,train) #store the prediction values
(tab1 <- table(p1, train$admit))
#identifying misclassification % which is 0.295
1-sum(diag(tab1)) / sum(tab1)

#CONFUSION MATRIX for Test data
p2 <- predict(model,test) #store the prediction values
(tab2 <- table(p2, test$admit))
#identifying misclassification % which is 0.32
1-sum(diag(tab2)) / sum(tab2)

## to improve this misclassification, while developing the MODEL we can 
## useKernal = TRUE, which will slightly increase the accuracy 


model <- naive_bayes(admit ~ ., data=train, usekernel = T)

#CONFUSION MATRIX for Train data
p1 <- predict(model,train)
(tab1 <- table(p1, train$admit))
#identifying misclassification % which is 0.273
1-sum(diag(tab1)) / sum(tab1)

#CONFUSION MATRIX for Test data
p2 <- predict(model,test)
(tab2 <- table(p2, test$admit))
#identifying misclassification % which is 0.30
1-sum(diag(tab2)) / sum(tab2)
