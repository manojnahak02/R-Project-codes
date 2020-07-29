
mydata<-read.csv(file.choose()) #get Stolen data.csv file
View(mydata)
library(e1071)
a<-naiveBayes(Stolen~., data=mydata)
a


b<-read.csv(file.choose()) #get Stolen data test.csv file
ab<-predict(a,b,"raw")
b1<-data.frame(cbind(ab[,2],b))

print(b1)
