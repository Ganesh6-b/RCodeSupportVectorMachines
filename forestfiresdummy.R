setwd("F://R//Rfiles")
forestfiles <- read.csv("forestfires (1).csv")
View(forestfiles)
attach(forestfiles)

table(area)
#after making the catogorical variable to factor variable
#need to normalize the other numerical datas
forestfiles[,c(3:11)] <- scale(forestfiles[,c(3:11)])
View(forestfiles)
str(forestfiles)
#partition of data
library(caret)
datas <- createDataPartition(forestfiles$size_category, p = 0.75, list = FALSE)
Train_data <- forestfiles[datas,]
Test_data <- forestfiles[-datas,]

View(Train_data)

Train_data <- Train_data[,-c(1,2)]
View(Train_data)
Test_data <- Test_data[,-c(1,2)]

#building model

library(e1071)

model1 <- svm(Train_data$size_category~., data = Train_data, kernel = "linear")

summary(model1)

pred <- predict(model1, newdata = Test_data[,-31])
table(pred, Test_data$size_category)
mean(pred == Test_data$size_category) # 0.8984

library(kernlab)

model2 <- ksvm(Train_data$size_category~., data = Train_data, kernel = "anovadot")
summary(model2)
pred2 <- predict(model2, newdata = Test_data[,-31])
mean(pred2 == Test_data$size_category) #0.92

#anovadot makes a best model

install.packages("ElemStatLearn")
library(ElemStatLearn)

set = Train_data

