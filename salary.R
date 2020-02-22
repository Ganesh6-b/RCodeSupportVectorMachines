setwd("F://R//files")
Train_data <- read.csv("SalaryData_Train(1).csv")
Test_data <- read.csv("SalaryData_Test(1).csv")
View(Train_data)

table(Train_data$education)
str(Train_data)
#initially to make catogorical variable to the numerical
str(Train_data)
library(dummies)
attach(Train_data)
a <- dummy(workclass)
b <- dummy(education)
c <- dummy(maritalstatus)
d <- dummy(occupation)
e <- dummy(relationship)
f <- dummy(race)
g <- dummy(sex)
h <- dummy(native)

Train <- cbind(Train_data[,c(14,1,4,10,11,12)],a,b,c,d,e,f,g,h)
View(Train)
table(Train$Salary)

convert <- function(x) {
  x <- ifelse("<=50k", 0 , 1)
}
Train <- apply(Train$Salary ,convert)
View(Train)

str(Train)

i <- dummy(Test_data$workclass)
j <- dummy(Test_data$education)
k <- dummy(Test_data$maritalstatus)
l <- dummy(Test_data$occupation)
m <- dummy(Test_data$relationship)
n <- dummy(Test_data$race)
o <- dummy(Test_data$sex)
p <- dummy(Test_data$native)

Test <- cbind(Test_data[,c(14,1,4,10,11,12)],i,j,k,l,m,n,o,p)

#normalization of data
Train[,c(2,3,4,5,6)] <- scale(Train[,c(2,3,4,5,6)])
View(Train) #normalized data

#model building

library(kernlab)

model1 <- ksvm(Train$Salary~., data = Train, kernel = "rbfdot")
