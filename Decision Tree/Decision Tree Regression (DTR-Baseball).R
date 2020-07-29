#Objective 
#We use the basket data to predict a baseball players salary (Y) based on the
#numbers of years he has played in the major league, and the number
#of hits he made in the previous year
#New league- A factor with levels A and N indicating player's league
#at the beginning of 1986
library(knitr)
library(tree)
baseball<- read.csv("E:\\Imarticus DSP 24\\R Programs\\Decision Tree\\baseball.csv")
summary(baseball)
View(baseball)
sapply(baseball, function(x)sum(is.na(x)))

#model building (tree or rpart)
tree.fit <- tree(Salary~Hits+Years, data=baseball) #cutoff is taken by entropy
summary(tree.fit)
plot(tree.fit)
text(tree.fit) #to get cuttoff header

#The argument pretty=0 instructs R to include the catagory names
#for any qualitative predictors
text(tree.fit, pretty=0)
tree.fit

#prediction
yhat <- predict(tree.fit, baseball)
Hit<- data.frame(baseball, yhat)

#mean square error (Y - yhat) i.e MSE should be near to 0
mean((Hit$Salary-Hit$yhat)^2)
#another way manual way
Hit$residual<-Hit$Salary - Hit$yhat
Hit$residual<-(Hit$yhat)^2
sum(Hit$residual1)/263
mean((Hit$Salary-Hit$yhat)^2)

##how to increase the accuracy of the model and decrease the mse
#log transform salary to make it a bit more normally distributed
baseball<- read.csv("E:\\Imarticus DSP 24\\R Programs\\Decision Tree\\baseball.csv")
hist(baseball$Salary)
baseball$Salary <- log(baseball$Salary)#log transformation to get normal hist
hist(baseball$Salary)
#Agin model created
tree.fit<- tree(Salary~Hits+Years, data=baseball)
summary(tree.fit)
plot(tree.fit)
text(tree.fit, pretty=0)
tree.fit

#Prediction
yhat<- predict(tree.fit, baseball)
Hit1<- data.frame(baseball,yhat)
mean((Hit1$Salary-Hit1$yhat)^2)#mean square error (Y - yhat) i.e MSE

#Another way of doing analysis
#Tree Pruning
#log transform salary to make it a bit more normally distributed
baseball<-read.csv("E:\\Imarticus DSP 24\\R Programs\\Decision Tree\\baseball.csv")
hist(baseball$Salary)
baseball$Salary<-log(baseball$Salary)
hist(baseball$Salary)
library(caret)
split <- createDataPartition(y=baseball$Salary, p=0.7, list = FALSE)
#list means column, when it is false we break data rowwise
train<- baseball[split,]
test<- baseball[-split,]

#Create tree model
trees<-tree(Salary~.,data=train)
plot(trees)
text(trees, pretty=0)

#Prediction
yhat<-predict(trees, test)
Hit1<-data.frame(test,yhat)
mean((Hit1$Salary-Hit1$yhat)^2)

#CV = Cross Validate to see whether pruning the tree will improve performance
cv.trees<- cv.tree(trees)
plot(cv.trees$size, cv.trees$dev, type = "b")
#Size = terminal, dev = error, b is used to connect the dots
prune.trees<- prune.tree(trees, best=4) #4 is taken from cv graph
plot(prune.trees)
text(prune.trees, pretty=0)

##Prediction
yhat<-predict(prune.trees, test)
Hit2<-data.frame(test,yhat)
mean((Hit2$Salary-Hit2$yhat)^2)
