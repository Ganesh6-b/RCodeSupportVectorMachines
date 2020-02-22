setwd("F://R//Rfiles")
forestdata <- read.csv("forestfires.csv")
View(forestdata)
str(forestdata)

library(caret)
data_train <- createDataPartition(forestdata$size_category, p = 0.75, list = F)
Train <- forestdata[data_train,]
Test <- forestdata[-data_train,] 
class(Train)

View(Train)
attach(Train)
install.packages("kernlab")
library(kernlab)
model1 <- ksvm(Train$size_category ~ ., data = Train, kernel = "rbfdot")

summary(model1)
pred <- predict(model1, newdata = Test)
pred
mean(pred == Test$size_category) #0.757

model2 <- ksvm(Train$size_category ~ ., data = Train, kernel = "polydot")

summary(model2)
pred2 <- predict(model2, newdata = Test)
pred2
mean(pred2 == Test$size_category) #0.89

table(pred2, Test$size_category)
library(gmodels)

CrossTable(pred2, Test$size_category)

model3 <- ksvm(Train$size_category ~. , data = Train, kernel = "anovadot")

summary(model3)

pred3 <- predict(model3, newdata = Test)
pred3
mean(pred3 == Test$size_category) #0.929

model4 <- ksvm(Train$size_category ~ . , data = Train, kernel = "vanilladot")

pred4 <- predict(model4, newdata = Test)

mean(pred4 == Test$size_category) #0.89


#anovadot is the best model 