#Joins

df1 = data.frame(CustomerId=c(1:6), product = c(rep("mobile",3),rep("Radio",3)))
#creating table as df1 with 6 rows and 2 columns 
#were mobile and radio are replicated 3 times in column name product

df2 = data.frame(CustomerId=c(2,4,6), state = c(rep("kar",2),rep("mum",1)))

names(df1)
names(df2)

#innerjoin or merge it is bydefault
merge(df1,df2,by="CustomerId")

#outerjoin (all is used in outerjoin which even gives unmatched/missing values)
merge(x=df1,y=df2,by="CustomerId",all = TRUE)

#leftjoin (all.x means fixing the left table(x) and getting data from another table)
merge(x=df1,y=df2,by="CustomerId",all.x = TRUE)

#rightjoin (all.y means fixing the right table(y) and getting data from another table)
merge(x=df1,y=df2,by="CustomerId",all.y = TRUE)

#merging
product <- data.frame(loan_account_no = c(234,235,236,237,238), 
                      interest = c(11,22,33,44,55), data = 1:5)
demo <- data.frame(loan_account_no = c(234,235,236,4,5), income=c(66,77,88,99,99),
                   data = 1:5)

merge(product, demo, by = "loan_account_no") #NA's match, so 6 rows

#create vector objects

city <- c("Tampa", "Seattle", "Hartford", "Denver")
state <- c("FL","WA","CT","CO")
zipcode <- c(33602, 98104, 061061, 80294)
addresses = data.frame(city,state,zipcode)
View(addresses)

#Combine another data frame using same columns
city = c("Lowry", "Charlotte")
state = c("CO","FL")
zipcode  = c(80230, 33498)

new.addresses <- data.frame(city,state,zipcode)
print(new.addresses)

#rbind : append rows (to add rows)
#rbind : combines or append data from both data frames
#same columns name is the primary need

all.addresses <- rbind(addresses, new.addresses)
print(all.addresses)
names(all.addresses)

##renaming the column names
my_data <- iris
names(my_data)
names(my_data) [names(my_data) == "Sepal.Length"] <- "s_length"
names(my_data) [names(my_data) == "Sepal.Width"] <- "s_width"
names(my_data)
names(iris)

#misssing value
y<- c(1,2,3, NA)
is.na(y) #to check the missing values in y

x<- c(1,2,NA,3) #as NA is a string value
mean(x)

#To remove missing values
mean(x,na.rm = TRUE)

#To remove all missing values records
newdata <- na.omit(x) #to delet the entire rows were na is there

#replacing missing values
x<- c(1,2,3,NA,6,7,8,NA,NA)
library(gtools) #to use the package gtools
na.replace(x,999) #this function works on gtools only

#To replace Mv by mean

Math <- c(88,95,85,NA,67,98,NA,67)
mean(Math)
median(Math)
Math[is.na(Math)] # to check the number of NA
Math[!is.na(Math)] # to get data other then NA
mean(Math, na.rm = TRUE) #computing mean after removing NA values
mean(Math[!is.na(Math)])
median(Math[!is.na(Math)])
Math[is.na(Math)] <- mean(Math[!is.na(Math)]) #here NA is replaced by the mean value



