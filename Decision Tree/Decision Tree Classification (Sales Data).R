#Objective 
#Sales of Child car seats
# 
library(tree)

##creating a dependence variable base on sales

salesdata<- read.csv("E:\\Imarticus DSP 24\\R Programs\\Decision Tree\\Sales data.csv")
summary(salesdata$Sales)
salesdata$High=as.factor(ifelse(salesdata$Sales<=8,0,1))#converted in catagorical
salesdata$Sales<-NULL
data<-salesdata
rm(salesdata)
sapply(data, function(x)sum(is.na(x)))

#data partition
library(caret)
split <- createDataPartition(y=data$High, p=0.7, list=FALSE)
train <- data[split,]
test <- data[-split,]

#Building model
tree.data = tree(High~., data=train)
plot(tree.data)
text(tree.data, pretty=0)
tree.data
tree.pred=predict(tree.data, test, type="class")#Class will give ans in 0 or 1
table(tree.pred, test$High)
library(caret)
confusionMatrix(tree.pred, test$High)

#The funstion CV.tree() performs cross - validation in order to cv.tree()
#determine the optimal level of tree complexity;
#cost complexity pruning is used inorder to select a sequence of trees
#for consideration.
#we use the argument FUN=prune.misclass in order to indicate that we
#want the classification error rate to guide the cross-validation
#and pruning process.
cv.data= cv.tree(tree.data, FUN=prune.misclass) #FUN is used when y is catagorical
names(cv.data)
plot(cv.data$size, cv.data$dev, type="b")
prune.data = prune.misclass (tree.data, best = 10)#misclass is used when y is classification
plot(prune.data)
text(prune.data, pretty = 0)
tree.pred=predict(prune.data, test, type = "class")
table(tree.pred, test$High)
confusionMatrix(tree.pred, test$High)

#Changing cost function and check the performance
prune.data1 = prune.misclass(tree.data, best = 9)
plot(prune.data1)
text(prune.data1, pretty = 0)
tree.pred1 = predict (prune.data1, test, type = "class")
confusionMatrix(tree.pred1, test$High)

